;;; vuistack-table-commands.el --- Column and table commands and the minor mode of vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; Filter and column commands (visibility, ordering, pinning,
;; scrolling, sizing), whole-table commands, and `vuistack-table-mode'
;; binding every vuistack-table command.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pp)
(require 'vui)
(require 'vuistack-table-model)
(require 'vuistack-table-state)
(require 'vuistack-table-rows)

;; -- commands: filtering --

(defun vuistack-table-filter (text)
  "Filter the table at point by TEXT (its global filter), preserving its sort.
An empty TEXT clears the filter.  A `column: text' form scopes the
search to a visible column.  TEXT is the filter language of
`vuistack-table--parse-query' (substring, ~fuzzy, =exact, >N, LO..HI ...)."
  (interactive (list (read-string "Filter rows (column: text; empty clears): ")))
  (pcase-let* ((`(,id ,columns) (vuistack-table--table-at-point))
               (filter (and columns (vuistack-table--parse-filter columns text))))
    (unless columns
      (user-error "Move point onto a table before filtering"))
    (setf (alist-get id vuistack-table--filter-state nil nil #'equal) filter)
    (vuistack-table--changed id t)
    (if filter
        (message "Filter: %s" (nth 2 filter))
      (message "Filter cleared"))))

(defun vuistack-table-filter-clear ()
  "Clear the filter of the table at point and repaint without refetching."
  (interactive)
  (pcase-let ((`(,id ,columns) (vuistack-table--table-at-point)))
    (unless columns
      (user-error "Move point onto a table before clearing its filter"))
    (setf (alist-get id vuistack-table--filter-state nil nil #'equal) nil)
    (vuistack-table--changed id t)
    (message "Filter cleared")))

(defun vuistack-table--faceted-rows (id columns index)
  "Table ID's rows past every filter but column INDEX's own (TanStack's
faceted row model)."
  (let ((state (vuistack-table-state id)))
    (cl-remove-if-not
     (lambda (row)
       (vuistack-table--row-passes-p columns row (plist-get state :filter)
                                     (cl-remove index (plist-get state :column-filters)
                                                :key #'car)))
     (plist-get (vuistack-table--table id) :rows))))

(defun vuistack-table--read-column-filter ()
  "Read a filter for the column at point, completing its faceted values."
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (column (nth index columns))
               (rows (vuistack-table--faceted-rows id columns index))
               (facets (vuistack-table-faceted-unique-values column rows))
               (format (vuistack-table--formatter column))
               (candidates (mapcar (lambda (f) (cons (funcall format (car f)) (cdr f)))
                                   facets))
               (range (vuistack-table-faceted-min-max column rows))
               (current (alist-get index (vuistack-table--get id :column-filters))))
    (vuistack-table--column-allows column :disable-filter "filtering")
    (let ((completion-extra-properties
           (list :annotation-function
                 (lambda (candidate)
                   (format "  %d" (or (cdr (assoc candidate candidates)) 0))))))
      (completing-read
       (format "Filter %s%s (empty clears): "
               (or (plist-get column :header) "column")
               (if range
                   (format " [%s..%s]" (funcall format (car range)) (funcall format (cdr range)))
                 ""))
       (mapcar #'car candidates) nil nil current))))

(defun vuistack-table-filter-column (query)
  "Filter the column at point by QUERY; an empty QUERY clears its filter.
Column filters stack (every one must pass) beside the global filter.
QUERY is the filter language of `vuistack-table--parse-query';
interactively it completes the column's faceted values with counts."
  (interactive (list (vuistack-table--read-column-filter)))
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (query (string-trim query))
               (filters (cl-remove index (vuistack-table--get id :column-filters) :key #'car)))
    (vuistack-table--column-allows (nth index columns) :disable-filter "filtering")
    (vuistack-table--put id :column-filters
                         (if (string-empty-p query)
                             filters
                           (append filters (list (cons index query)))))
    (vuistack-table--changed id t)))

(defun vuistack-table-filter-reset ()
  "Clear the global and every column filter of the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :filter nil)
    (vuistack-table--put id :column-filters nil)
    (vuistack-table--changed id t)
    (message "Filters cleared")))

;; -- commands: columns --

(defun vuistack-table--effective-order (id columns)
  "Table ID's column order: its :order completed with the other COLUMNS."
  (let ((order (cl-remove-if-not (lambda (i) (< -1 i (length columns)))
                                 (vuistack-table--get id :order))))
    (append order (cl-remove-if (lambda (i) (memq i order))
                                (number-sequence 0 (1- (length columns)))))))

(defun vuistack-table-hide-column ()
  "Hide the column at point (TanStack column visibility)."
  (interactive)
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (hidden (vuistack-table--get id :hidden)))
    (vuistack-table--column-allows (nth index columns) :disable-hide "hiding")
    (when (>= (1+ (length hidden)) (length columns))
      (user-error "Cannot hide the last visible column"))
    (vuistack-table--put id :hidden (append hidden (list index)))
    (vuistack-table--changed id)))

(defun vuistack-table-toggle-column (header)
  "Show or hide the column HEADER of the table at point."
  (interactive
   (pcase-let* ((`(,id ,columns ,_index) (vuistack-table--context))
                (hidden (vuistack-table--get id :hidden)))
     (list (completing-read
            "Toggle column: "
            (cl-loop for col in columns for i from 0
                     collect (format "%s%s" (or (plist-get col :header) i)
                                     (if (memq i hidden) " (hidden)" "")))
            nil t))))
  (pcase-let* ((`(,id ,columns ,_index) (vuistack-table--context))
               (index (vuistack-table--resolve-column
                       columns (string-remove-suffix " (hidden)" header)))
               (hidden (vuistack-table--get id :hidden)))
    (unless index (user-error "No column %s" header))
    (if (memq index hidden)
        (vuistack-table--put id :hidden (remq index hidden))
      (vuistack-table--column-allows (nth index columns) :disable-hide "hiding")
      (vuistack-table--put id :hidden (append hidden (list index))))
    (vuistack-table--changed id)))

(defun vuistack-table-show-all-columns ()
  "Show every hidden column of the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :hidden nil)
    (vuistack-table--changed id)))

(defun vuistack-table--move-column (delta)
  "Move the column at point DELTA visible places (TanStack column ordering)."
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (hidden (vuistack-table--get id :hidden))
               (order (vuistack-table--effective-order id columns))
               (visible (cl-remove-if (lambda (i) (memq i hidden)) order))
               (pos (cl-position index visible))
               (target (and pos (nth (+ pos delta) visible))))
    (when (and target (>= (+ pos delta) 0))
      (let ((from (cl-position index order))
            (to (cl-position target order)))
        (setf (nth from order) target
              (nth to order) index))
      (vuistack-table--put id :order order)
      (vuistack-table--changed id))))

(defun vuistack-table-move-column-left ()
  "Move the column at point one place left."
  (interactive)
  (vuistack-table--move-column -1))

(defun vuistack-table-move-column-right ()
  "Move the column at point one place right."
  (interactive)
  (vuistack-table--move-column 1))

(defun vuistack-table-reset-column-order ()
  "Restore the column order of the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :order nil)
    (vuistack-table--changed id)))

(defun vuistack-table-pin-column ()
  "Cycle the column at point through pinned left, pinned right and unpinned.
Pinned columns stay put while `vuistack-table-scroll-columns-right'
scrolls the centre ones (sticky pinning)."
  (interactive)
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (left (vuistack-table--get id :pin-left))
               (right (vuistack-table--get id :pin-right)))
    (vuistack-table--column-allows (nth index columns) :disable-pin "pinning")
    (cond ((memq index left)
           (vuistack-table--put id :pin-left (remq index left))
           (vuistack-table--put id :pin-right (append right (list index))))
          ((memq index right)
           (vuistack-table--put id :pin-right (remq index right)))
          (t (vuistack-table--put id :pin-left (append left (list index)))))
    (vuistack-table--changed id)))

(defun vuistack-table-scroll-columns-right (&optional n)
  "Scroll the unpinned columns of the table at point N (default 1) to the left."
  (interactive "p")
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :scroll (max 0 (+ (or (vuistack-table--get id :scroll) 0)
                                              (or n 1))))
    (vuistack-table--changed id)))

(defun vuistack-table-scroll-columns-left (&optional n)
  "Scroll the unpinned columns of the table at point N (default 1) back."
  (interactive "p")
  (vuistack-table-scroll-columns-right (- (or n 1))))

(defun vuistack-table-resize-column (delta)
  "Widen the column at point by DELTA (narrow when negative).
The width stays within the column's :min-width and :max-width."
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (column (nth index columns))
               (current (or (alist-get index (vuistack-table--get id :widths))
                            (alist-get index (plist-get (vuistack-table--table id) :widths))
                            (plist-get column :width)
                            10))
               (widths (vuistack-table--get id :widths)))
    (vuistack-table--column-allows column :disable-resize "resizing")
    (setf (alist-get index widths)
          (vuistack-table--clamp-width column (+ current delta)))
    (vuistack-table--put id :widths widths)
    (vuistack-table--changed id)))

(defun vuistack-table-widen-column (&optional n)
  "Widen the column at point by N (default 1)."
  (interactive "p")
  (vuistack-table-resize-column (or n 1)))

(defun vuistack-table-narrow-column (&optional n)
  "Narrow the column at point by N (default 1)."
  (interactive "p")
  (vuistack-table-resize-column (- (or n 1))))

(defun vuistack-table-reset-column-widths ()
  "Forget the widths set by hand in the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :widths nil)
    (vuistack-table--changed id)))

;; -- commands: the whole table --

(defun vuistack-table-reset-table ()
  "Reset the table at point to its :initial-state (TanStack reset)."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (setq vuistack-table--sort-state
          (cl-remove id vuistack-table--sort-state :key #'car :test #'equal)
          vuistack-table--filter-state
          (cl-remove id vuistack-table--filter-state :key #'car :test #'equal)
          vuistack-table--table-states
          (cl-remove id vuistack-table--table-states :key #'car :test #'equal))
    (vuistack-table--changed id)))

(defun vuistack-table-describe ()
  "Show the state and row counts of the table at point (a devtools view)."
  (interactive)
  (pcase-let* ((`(,id ,columns ,_index) (vuistack-table--context))
               (table (vuistack-table--table id))
               (model (plist-get table :model))
               (state (vuistack-table-state id)))
    (with-help-window (help-buffer)
      (princ (format "vuistack-table %S\n\n" id))
      (princ (format "Columns: %d (%d shown)\nRows: %d core, %d filtered; page %d of %d\n\n"
                     (length columns) (length (plist-get table :display))
                     (length (plist-get table :rows))
                     (length (plist-get model :filtered-rows))
                     (1+ (or (plist-get model :page) 0))
                     (or (plist-get model :page-count) 1)))
      (princ "State:\n")
      (princ (pp-to-string state)))))

(defvar vuistack-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S") #'vuistack-table-sort)
    (define-key map (kbd "/") #'vuistack-table-filter)
    (define-key map (kbd "C-c C-f") #'vuistack-table-filter-clear)
    (define-key map (kbd "f") #'vuistack-table-filter-column)
    (define-key map (kbd "F") #'vuistack-table-filter-reset)
    (define-key map (kbd "H") #'vuistack-table-hide-column)
    (define-key map (kbd "V") #'vuistack-table-toggle-column)
    (define-key map (kbd "<") #'vuistack-table-move-column-left)
    (define-key map (kbd ">") #'vuistack-table-move-column-right)
    (define-key map (kbd "|") #'vuistack-table-pin-column)
    (define-key map (kbd "{") #'vuistack-table-scroll-columns-left)
    (define-key map (kbd "}") #'vuistack-table-scroll-columns-right)
    (define-key map (kbd "+") #'vuistack-table-widen-column)
    (define-key map (kbd "-") #'vuistack-table-narrow-column)
    (define-key map (kbd "=") #'vuistack-table-reset-column-widths)
    (define-key map (kbd "G") #'vuistack-table-group-by-column)
    (define-key map (kbd "x") #'vuistack-table-toggle-expand)
    (define-key map (kbd "X") #'vuistack-table-toggle-expand-all)
    (define-key map (kbd "m") #'vuistack-table-toggle-select)
    (define-key map (kbd "M") #'vuistack-table-toggle-select-all)
    (define-key map (kbd "U") #'vuistack-table-clear-selection)
    (define-key map (kbd "T") #'vuistack-table-pin-row-top)
    (define-key map (kbd "B") #'vuistack-table-pin-row-bottom)
    (define-key map (kbd "]") #'vuistack-table-next-page)
    (define-key map (kbd "[") #'vuistack-table-previous-page)
    (define-key map (kbd "#") #'vuistack-table-set-page-size)
    (define-key map (kbd "c") #'vuistack-table-cell-select)
    (define-key map (kbd "C") #'vuistack-table-cell-extend)
    (define-key map (kbd "w") #'vuistack-table-copy-cells)
    (define-key map (kbd "E") #'vuistack-table-edit-cell)
    (define-key map (kbd "C-c C-r") #'vuistack-table-reset-table)
    (define-key map (kbd "C-c C-d") #'vuistack-table-describe)
    map)
  "Keymap of `vuistack-table-mode'.")

;;;###autoload
(define-minor-mode vuistack-table-mode
  "Drive the `vuistack-table's of a vui buffer from the keyboard.
\\{vuistack-table-mode-map}"
  :lighter nil
  :keymap vuistack-table-mode-map)

(provide 'vuistack-table-commands)
;;; vuistack-table-commands.el ends here
