;;; vuistack-table-rows.el --- Row commands of vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; Commands on the rows and cells of the table at point: grouping and
;; expanding, row selection, row pinning, pagination and cell selection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'vuistack-table-model)
(require 'vuistack-table-state)

;; -- commands: grouping and expanding --

(defun vuistack-table-group-by-column ()
  "Group the table at point by the column at point, or stop grouping by it.
Groups nest in the order columns were added; each column's
:aggregation fills the group rows."
  (interactive)
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (grouping (vuistack-table--get id :grouping)))
    (vuistack-table--column-allows (nth index columns) :disable-group "grouping")
    (vuistack-table--put id :grouping (if (memq index grouping)
                                          (remq index grouping)
                                        (append grouping (list index))))
    (vuistack-table--changed id t)))

(defun vuistack-table--expandable-ids (id)
  "Ids of every node of table ID that can expand."
  (let* ((model (plist-get (vuistack-table--table id) :model))
         (can-expand (plist-get model :can-expand))
         ids)
    (cl-labels ((walk (nodes)
                  (dolist (node nodes)
                    (when (funcall can-expand node) (push (plist-get node :id) ids))
                    (walk (plist-get node :children)))))
      (walk (plist-get model :tree)))
    (nreverse ids)))

(defun vuistack-table-toggle-expand ()
  "Expand or collapse the row (or group) at point."
  (interactive)
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (node (vuistack-table--node-at-point))
               (row-id (plist-get node :id))
               (expanded (vuistack-table--get id :expanded)))
    (unless (get-text-property (line-beginning-position) 'vuistack-table-expandable)
      (user-error "Nothing to expand on this row"))
    (when (eq expanded t)
      (setq expanded (vuistack-table--expandable-ids id)))
    (vuistack-table--put id :expanded (if (member row-id expanded)
                                          (remove row-id expanded)
                                        (append expanded (list row-id))))
    (vuistack-table--changed id)))

(defun vuistack-table-toggle-expand-all ()
  "Expand every row of the table at point, or collapse them all."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :expanded
                         (if (or (eq (vuistack-table--get id :expanded) t)
                                 (let ((all (vuistack-table--expandable-ids id)))
                                   (and all (cl-every (lambda (i)
                                                        (member i (vuistack-table--get
                                                                   id :expanded)))
                                                      all))))
                             nil
                           t))
    (vuistack-table--changed id)))

;; -- commands: row selection --

(defun vuistack-table--selectable-p (options row)
  "Whether ROW may be selected per OPTIONS :enable-row-selection."
  (let ((enable (plist-get options :enable-row-selection)))
    (if (functionp enable) (funcall enable row) enable)))

(defun vuistack-table--selection-ids (node options)
  "Row ids selecting NODE selects: a group's leaf rows; a row with sub-rows,
itself and its descendants unless OPTIONS :disable-sub-row-selection."
  (let ((ids nil))
    (cl-labels ((walk (n)
                  (cond ((plist-get n :members)
                         (mapc #'walk (plist-get n :members)))
                        (t (when (vuistack-table--selectable-p options (plist-get n :row))
                             (push (plist-get n :id) ids))
                           (unless (plist-get options :disable-sub-row-selection)
                             (mapc #'walk (plist-get n :children)))))))
      (walk node))
    (nreverse ids)))

(defun vuistack-table--toggle-ids (id ids single)
  "Select IDS in table ID, or deselect them when all are selected.
SINGLE replaces the selection instead of adding to it."
  (let ((selected (vuistack-table--get id :selected)))
    (vuistack-table--put
     id :selected
     (cond ((and ids (cl-every (lambda (i) (member i selected)) ids))
            (cl-remove-if (lambda (i) (member i ids)) selected))
           (single ids)
           (t (append selected (cl-remove-if (lambda (i) (member i selected)) ids)))))
    (vuistack-table--changed id)))

(defun vuistack-table-toggle-select (&optional range)
  "Select or deselect the row at point (a group or parent: its rows).
With a prefix argument RANGE, select every row from the last one
toggled to this one (TanStack shift range selection)."
  (interactive "P")
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (table (vuistack-table--table id))
               (options (plist-get table :options))
               (node (vuistack-table--node-at-point))
               (last (vuistack-table--get id :last-selected))
               (order (plist-get table :row-order))
               (from (and range last (cl-position last order :test #'equal)))
               (to (cl-position (plist-get node :id) order :test #'equal))
               (ids (if (and from to)
                        (delete-dups
                         (cl-mapcan
                          (lambda (row-id)
                            (let ((n (vuistack-table--find-node
                                      (plist-get (plist-get table :model) :tree) row-id)))
                              (and n (vuistack-table--selection-ids n options))))
                          (cl-subseq order (min from to) (1+ (max from to)))))
                      (vuistack-table--selection-ids node options))))
    (unless (plist-get options :enable-row-selection)
      (user-error "This table has no row selection"))
    (unless ids (user-error "This row cannot be selected"))
    (vuistack-table--put id :last-selected (plist-get node :id))
    (if (and from to)
        (progn (vuistack-table--put id :selected
                                    (append (cl-remove-if (lambda (i) (member i ids))
                                                          (vuistack-table--get id :selected))
                                            ids))
               (vuistack-table--changed id))
      (vuistack-table--toggle-ids id ids (plist-get options :single-select)))))

(defun vuistack-table-toggle-select-all (&optional page-only)
  "Select every filtered row of the table at point, or deselect them all.
With a prefix argument PAGE-ONLY, just the rows of the current page."
  (interactive "P")
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (table (vuistack-table--table id))
               (options (plist-get table :options))
               (model (plist-get table :model))
               (nodes (if page-only
                          (mapcar #'car (plist-get model :entries))
                        (plist-get model :filtered)))
               (ids (delete-dups (cl-mapcan (lambda (n) (vuistack-table--selection-ids
                                                         n options))
                                            nodes))))
    (unless (plist-get options :enable-row-selection)
      (user-error "This table has no row selection"))
    (when (plist-get options :single-select)
      (user-error "This table selects one row at a time"))
    (vuistack-table--toggle-ids id ids nil)))

(defun vuistack-table-clear-selection ()
  "Deselect every row of the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :selected nil)
    (vuistack-table--changed id)))

(defun vuistack-table-selected-rows (&optional id filtered)
  "The selected row plists of table ID (default: at point), in row order.
FILTERED keeps only those past the filters (TanStack's filtered
selected row model)."
  (let* ((id (or id (car (vuistack-table--table-at-point))))
         (selected (vuistack-table--get id :selected))
         (model (plist-get (vuistack-table--table id) :model))
         (nodes (plist-get model (if filtered :filtered :core)))
         rows)
    (cl-labels ((walk (list)
                  (dolist (node list)
                    (when (member (plist-get node :id) selected)
                      (push (plist-get node :row) rows))
                    (walk (plist-get node :children)))))
      (walk nodes))
    (nreverse rows)))

;; -- reading rows --

(defun vuistack-table-get-row (id row-id)
  "The row plist of table ID whose row id is ROW-ID (TanStack getRow), or nil."
  (plist-get (vuistack-table--find-node
              (plist-get (plist-get (vuistack-table--table id) :model) :core)
              row-id)
             :row))

;; -- commands: row pinning --

(defun vuistack-table--pin-row (key other)
  "Toggle the row at point in pinned list KEY, taking it out of OTHER."
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (options (plist-get (vuistack-table--table id) :options))
               (node (vuistack-table--node-at-point))
               (row-id (plist-get node :id))
               (enable (plist-get options :enable-row-pinning))
               (pinned (vuistack-table--get id key)))
    (when (plist-get node :members)
      (user-error "Group rows cannot be pinned"))
    (when (and (functionp enable) (not (funcall enable (plist-get node :row))))
      (user-error "This row cannot be pinned"))
    (when (plist-get options :disable-row-pinning)
      (user-error "This table has no row pinning"))
    (vuistack-table--put id other (remove row-id (vuistack-table--get id other)))
    (vuistack-table--put id key (if (member row-id pinned)
                                    (remove row-id pinned)
                                  (append pinned (list row-id))))
    (vuistack-table--changed id)))

(defun vuistack-table-pin-row-top ()
  "Pin the row at point above the others, or unpin it."
  (interactive)
  (vuistack-table--pin-row :pin-top :pin-bottom))

(defun vuistack-table-pin-row-bottom ()
  "Pin the row at point below the others, or unpin it."
  (interactive)
  (vuistack-table--pin-row :pin-bottom :pin-top))

;; -- commands: pagination --

(defun vuistack-table-goto-page (page)
  "Show PAGE (1-based) of the table at point."
  (interactive "nPage: ")
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (model (plist-get (vuistack-table--table id) :model))
               (count (or (plist-get model :page-count) 1)))
    (unless (vuistack-table--get id :page-size)
      (user-error "This table is not paginated"))
    (vuistack-table--put id :page (min (max 0 (1- page)) (1- count)))
    (vuistack-table--changed id)
    (message "Page %d of %d" (1+ (vuistack-table--get id :page)) count)))

(defun vuistack-table-next-page (&optional last)
  "Show the next page of the table at point.
With a prefix argument LAST, the last page."
  (interactive "P")
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (model (plist-get (vuistack-table--table id) :model)))
    (vuistack-table-goto-page (if last
                                  (plist-get model :page-count)
                                (+ 2 (or (plist-get model :page) 0))))))

(defun vuistack-table-previous-page (&optional first)
  "Show the previous page of the table at point.
With a prefix argument FIRST, the first page."
  (interactive "P")
  (pcase-let* ((`(,id ,_columns ,_index) (vuistack-table--context))
               (model (plist-get (vuistack-table--table id) :model)))
    (vuistack-table-goto-page (if first 1 (or (plist-get model :page) 0)))))

(defun vuistack-table-set-page-size (size)
  "Show SIZE rows a page in the table at point; 0 turns pagination off."
  (interactive "nRows per page (0 for all): ")
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :page-size (and (> size 0) size))
    (vuistack-table--changed id t)))

;; -- commands: cell selection --

(defun vuistack-table--cell-at-point ()
  "(ID ROW-ID . COLUMN-INDEX) of the table cell at point."
  (pcase-let ((`(,id ,_columns ,index) (vuistack-table--context t)))
    (cons id (cons (plist-get (vuistack-table--node-at-point) :id) index))))

(defun vuistack-table-cell-select (&optional arg)
  "Start a cell selection range at the cell at point.
With one prefix argument ARG add the range to the selection; with two
add it as an exclusion."
  (interactive "P")
  (pcase-let* ((`(,id . ,cell) (vuistack-table--cell-at-point))
               (range (list :anchor cell :focus cell
                            :exclude (equal arg '(16)))))
    (vuistack-table--put id :cell-ranges
                         (if arg
                             (append (vuistack-table--get id :cell-ranges) (list range))
                           (list range)))
    (vuistack-table--changed id)))

(defun vuistack-table-cell-extend ()
  "Extend the last cell selection range to the cell at point."
  (interactive)
  (pcase-let* ((`(,id . ,cell) (vuistack-table--cell-at-point))
               (ranges (copy-sequence (vuistack-table--get id :cell-ranges))))
    (if (null ranges)
        (vuistack-table-cell-select)
      (setcar (last ranges) (plist-put (copy-sequence (car (last ranges))) :focus cell))
      (vuistack-table--put id :cell-ranges ranges)
      (vuistack-table--changed id))))

(defun vuistack-table-cell-select-clear ()
  "Clear the cell selection of the table at point."
  (interactive)
  (pcase-let ((`(,id ,_columns ,_index) (vuistack-table--context)))
    (vuistack-table--put id :cell-ranges nil)
    (vuistack-table--changed id)))

(defun vuistack-table--range-rectangle (range rows columns)
  "RANGE's rows and columns: two lists, in ROWS and COLUMNS order."
  (pcase-let* ((`(,anchor-row . ,anchor-col) (plist-get range :anchor))
               (`(,focus-row . ,focus-col) (plist-get range :focus))
               (r1 (cl-position anchor-row rows :test #'equal))
               (r2 (cl-position focus-row rows :test #'equal))
               (c1 (cl-position anchor-col columns))
               (c2 (cl-position focus-col columns)))
    (when (and r1 r2 c1 c2)
      (list (cl-subseq rows (min r1 r2) (1+ (max r1 r2)))
            (cl-subseq columns (min c1 c2) (1+ (max c1 c2)))))))

(defun vuistack-table-selected-cells (&optional id)
  "The selected cells of table ID (default: at point), (ROW-ID . COLUMN-INDEX).
Ranges apply in order, an :exclude range taking its cells away."
  (let* ((id (or id (car (vuistack-table--table-at-point))))
         (table (vuistack-table--table id))
         (rows (plist-get table :row-order))
         (columns (mapcar (lambda (c) (plist-get c :vuistack-table--index))
                          (plist-get table :display)))
         (cells nil))
    (dolist (range (vuistack-table--get id :cell-ranges))
      (pcase-let ((`(,range-rows ,range-columns)
                   (vuistack-table--range-rectangle range rows columns)))
        (dolist (r range-rows)
          (dolist (c range-columns)
            (if (plist-get range :exclude)
                (setq cells (delete (cons r c) cells))
              (unless (member (cons r c) cells) (push (cons r c) cells)))))))
    (nreverse cells)))

(defun vuistack-table--tsv-escape (value)
  "VALUE as one TSV field: nil empty, formula-like text quoted with a
leading ', fields with tabs, newlines or quotes double-quoted."
  (let* ((text (if (null value) "" (format "%s" value)))
         (text (if (and (stringp value) (string-match-p "\\`[\t\r ]*[=+@-]" value))
                   (concat "'" text)
                 text)))
    (if (string-match-p "[\"\t\n\r]" text)
        (concat "\"" (string-replace "\"" "\"\"" text) "\"")
      text)))

(defun vuistack-table-selected-cell-data (&optional id)
  "Raw values of table ID's selected cells: one grid (a list of rows of
values) per included range, excluded cells nil."
  (let* ((id (or id (car (vuistack-table--table-at-point))))
         (table (vuistack-table--table id))
         (columns (plist-get table :columns))
         (model (plist-get table :model))
         (rows (plist-get table :row-order))
         (display (mapcar (lambda (c) (plist-get c :vuistack-table--index))
                          (plist-get table :display)))
         (selected (vuistack-table-selected-cells id)))
    (cl-loop
     for range in (vuistack-table--get id :cell-ranges)
     for rect = (vuistack-table--range-rectangle range rows display)
     when (and rect (not (plist-get range :exclude)))
     collect
     (cl-loop for row-id in (car rect)
              for node = (vuistack-table--find-node (plist-get model :tree) row-id)
              collect (cl-loop for c in (cadr rect)
                               collect (and (member (cons row-id c) selected) node
                                            (vuistack-table--node-value columns node c)))))))

(defun vuistack-table-copy-cells ()
  "Copy the selected cells of the table at point to the kill ring as TSV.
Several ranges are separated by a blank line."
  (interactive)
  (let ((grids (vuistack-table-selected-cell-data)))
    (unless grids (user-error "No cells selected"))
    (kill-new (mapconcat
               (lambda (grid)
                 (mapconcat (lambda (row) (mapconcat #'vuistack-table--tsv-escape row "\t"))
                            grid "\n"))
               grids "\n\n"))
    (message "Copied %d cell range%s" (length grids) (if (cdr grids) "s" ""))))

(provide 'vuistack-table-rows)
;;; vuistack-table-rows.el ends here
