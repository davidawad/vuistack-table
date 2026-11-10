;;; vuistack-table-layout.el --- Faces and line layout of vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; Strings for one table line each: rows (padding, alignment, faces,
;; multi-line cells, cell spans), headings, header groups and footers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vuistack-table-cell)
(require 'vuistack-table-kpi)
(require 'vuistack-table-columns)

(defface vuistack-table-stripe
  '((((background dark)) :background "#32302f")
    (((background light)) :background "#f2f0eb"))
  "Alternating row background of a zebra `vuistack-table'."
  :group 'vuistack-table)

(defface vuistack-table-header
  '((t :inherit bold :underline t))
  "Column heading row of `vuistack-table'."
  :group 'vuistack-table)

(defface vuistack-table-positive
  '((t :inherit success))
  "Positive numeric cells of a :sign-color column."
  :group 'vuistack-table)

(defface vuistack-table-negative
  '((t :inherit error))
  "Negative numeric cells of a :sign-color column."
  :group 'vuistack-table)

(defcustom vuistack-table-max-auto-width 40
  "Widest an auto-sized column grows before truncating."
  :type 'natnum
  :group 'vuistack-table)

(defface vuistack-table-footer
  '((t :inherit bold :overline t))
  "Footer row of `vuistack-table' (column :footer cells)."
  :group 'vuistack-table)

(defface vuistack-table-cell-selected
  '((t :inherit region))
  "Cells inside a `vuistack-table' cell selection."
  :group 'vuistack-table)

;; -- layout --

(defun vuistack-table--col-widths (columns rows)
  "Width of each column in COLUMNS given ROWS.
An auto width stays within the column's :min-width and :max-width."
  (mapcar
   (lambda (col)
     (or (plist-get col :width)
         ;; A :cell renderer draws to the width it is given: it needs one.
         (and (plist-get col :cell) 20)
         (let ((width (min vuistack-table-max-auto-width
                           (apply #'max
                                  (string-width (or (plist-get col :header) ""))
                                  (mapcar
                                   (lambda (row)
                                     (string-width
                                      (funcall (vuistack-table--formatter col)
                                               (vuistack-table--cell-value col row))))
                                   rows)))))
           (if (or (plist-get col :min-width) (plist-get col :max-width))
               (vuistack-table--clamp-width col width)
             width))))
   columns))

(defun vuistack-table--cell-text (col row width)
  "COL's cell for ROW, WIDTH columns wide: its :cell renderer's string
\(called with ROW and WIDTH; may span lines), else its :format of the value."
  (if (plist-get col :cell)
      (or (funcall (plist-get col :cell) row width) "")
    (funcall (vuistack-table--formatter col)
             (vuistack-table--cell-value col row))))

(defun vuistack-table--decorate-row (line row index zebra)
  "LINE for ROW at INDEX: preserve its :face, bold totals, stripe when ZEBRA."
  (when (plist-get row :face)
    (add-face-text-property 0 (length line) (plist-get row :face) t line))
  (when (or (plist-get row :total) (plist-get row :emphasis))
    (add-face-text-property 0 (length line) 'bold t line))
  (when (and zebra (cl-oddp index))
    (add-face-text-property 0 (length line) 'vuistack-table-stripe t line))
  (propertize line 'vuistack-table-row row))

(defun vuistack-table--simple-row-line (columns widths row index zebra)
  "Render a plain, single-line ROW, or return nil when it needs full handling."
  (when
      (cl-every
       (lambda (col)
         (and (not (or (plist-get col :get) (plist-get col :format)
                       (plist-get col :cell) (plist-get col :indicator)
                       (plist-get col :sign-color) (plist-get col :money)
                       (plist-get col :align) (plist-get col :action)
                       (plist-get col :span-columns)))
              (not (string-match-p
                    "\n"
                    (vuistack-table--string
                     (vuistack-table--cell-value col row))))))
       columns)
    (vuistack-table--decorate-row
     (mapconcat
      (lambda (pair)
        (vuistack-table--pad
         (vuistack-table--string (vuistack-table--cell-value (car pair) row))
         (cdr pair)))
      (cl-mapcar #'cons columns widths) " ")
     row index zebra)))

(defun vuistack-table--column-slots (columns widths row)
  "(COLUMN WIDTH POSITION) for each cell of ROW across COLUMNS at WIDTHS.
A column with :span-columns (a count, or ROW -> count) takes the width
of that many columns, clamped to its pin region; the columns it covers
get no cell (TanStack cell spanning)."
  (let ((slots nil) (skip 0) (position 0))
    (cl-loop
     for tail on columns
     for wtail on widths
     do (let ((col (car tail)))
          (if (> skip 0)
              (cl-decf skip)
            (let* ((spec (plist-get col :span-columns))
                   (want (if (functionp spec) (funcall spec row) spec))
                   (region (cl-loop for c in (cdr tail)
                                    while (eq (plist-get c :vuistack-table--pin)
                                              (plist-get col :vuistack-table--pin))
                                    count t))
                   (n (if (numberp want)
                          (max 1 (truncate (min want (1+ region))))
                        1)))
              (setq skip (1- n))
              (push (list col (+ (apply #'+ (cl-subseq wtail 0 n)) (1- n)) position)
                    slots)))
          (cl-incf position)))
    (nreverse slots)))

(defun vuistack-table-row-line (columns widths row index zebra
                                        &optional no-sort action prefix blank)
  "One table row (a string, no trailing newline) for ROW at INDEX.
Every cell is padded to its WIDTHS entry; the indicator face goes on the
cell text, the stripe (when ZEBRA and INDEX is odd) over the whole row
beneath it, so the background survives the separators and padding.
A cell whose column has an :action (else ACTION) carries
`vuistack-table-action', a thunk calling it on ROW, for
`vuistack-table-activate'.  NO-SORT leaves out the column index sorting
reads.  A :cell renderer may return several lines: the row is as tall
as its tallest cell, every line padded per column so the columns stay
aligned.  PREFIX (tree indentation and expander) leads the first cell;
the columns in BLANK (a row-spanned run) render empty."
  (or (and no-sort (null action) (null prefix) (null blank)
           (vuistack-table--simple-row-line columns widths row index zebra))
      (let* ((slots (vuistack-table--column-slots columns widths row))
             (cells
              (cl-loop
               for (col width position) in slots
               collect
               (let* ((column-index (or (plist-get col :vuistack-table--index) position))
                      (blanked (memq col blank))
                      (indicator-face
                       (and (not blanked) (plist-get col :indicator)
                            (vuistack-table-indicator-face
                             (funcall (plist-get col :indicator) row))))
                      (value (and (not blanked) (plist-get col :sign-color)
                                  (vuistack-table--money-value
                                   (vuistack-table--cell-value col row))))
                      (face (or indicator-face
                                (and value (> value 0) 'vuistack-table-positive)
                                (and value (< value 0) 'vuistack-table-negative)))
                      (fn (or (plist-get col :action) action))
                      (right (or (plist-get col :money)
                                 (eq (plist-get col :align) 'right)))
                      (pieces (split-string (if blanked
                                                ""
                                              (vuistack-table--cell-text col row width))
                                            "\n"))
                      (pieces (if (and prefix (= position 0))
                                  (cons (concat prefix (car pieces))
                                        (mapcar (lambda (p)
                                                  (concat (make-string (string-width prefix)
                                                                       ?\s)
                                                          p))
                                                (cdr pieces)))
                                pieces)))
                 (cons
                  (mapcar
                   (lambda (piece)
                     (let ((text (if (and right (<= (string-width piece) width))
                                     (concat (make-string
                                              (- width (string-width piece)) ?\s)
                                             piece)
                                   (vuistack-table--pad piece width))))
                       (when fn
                         (put-text-property 0 (length text) 'vuistack-table-action
                                            (lambda () (funcall fn row)) text))
                       (unless no-sort
                         (put-text-property 0 (length text)
                                            'vuistack-table-column-index
                                            column-index text))
                       (if face (propertize text 'face face) text)))
                   pieces)
                  width))))
             (height (apply #'max 1 (mapcar (lambda (cell) (length (car cell))) cells))))
        (vuistack-table--decorate-row
         (mapconcat
          (lambda (i)
            (mapconcat (lambda (cell)
                         (or (nth i (car cell)) (make-string (cdr cell) ?\s)))
                       cells
                       " "))
          (number-sequence 0 (1- height))
          "\n")
         row index zebra))))

(declare-function eas-compile "eas")
(declare-function eas-text-render "eas-text")

(defun vuistack-table-eas-cell (spec-fn &optional rows)
  "A :cell renderer drawing (SPEC-FN ROW) as an eas.el chart.
The chart is ROWS tall (default 3) and as wide as its column.  Nil (a
blank cell) when eas is not loaded or SPEC-FN returns nil; the error
text when eas fails, so one bad chart never breaks the table."
  (lambda (row width)
    (let ((spec (funcall spec-fn row)))
      (cond ((or (null spec) (not (fboundp 'eas-compile))) nil)
            (t (condition-case err
                   (string-trim-right
                    (eas-text-render
                     (eas-compile spec :target 'text
                                  :size (list :cols width :rows (or rows 3)))))
                 (error (propertize (error-message-string err) 'face 'error))))))))

(defun vuistack-table--align (text width right)
  "TEXT padded (RIGHT: right-aligned when it fits) to WIDTH."
  (if (and right (< (string-width text) width))
      (concat (make-string (- width (string-width text)) ?\s) text)
    (vuistack-table--pad text width)))

(defun vuistack-table-header-line (columns widths &optional sort filter no-sort)
  "The heading line for COLUMNS at WIDTHS.
SORT, a (COLUMN-INDEX . DESCENDING-P) pair or a list of them, marks each
sorted column's heading with a trailing arrow (▲ ascending, ▼
descending), numbered by priority in a multi-sort.  FILTER is the active
query, shown after the columns.  NO-SORT leaves out the column index
sorting reads."
  (let ((sorts (vuistack-table--sort-list sort)))
    (propertize
     (concat
      (mapconcat (lambda (pair)
                   (let* ((index (vuistack-table--column-index-of (car pair) columns))
                          (label (or (plist-get (car pair) :header) ""))
                          (rank (cl-position index sorts :key #'car))
                          (label (if rank
                                     (concat label
                                             (if (cdr (nth rank sorts)) " ▼" " ▲")
                                             (if (cdr sorts) (number-to-string (1+ rank)) ""))
                                   label))
                          (text (vuistack-table--align
                                 label (cdr pair)
                                 (eq (plist-get (car pair) :align) 'right))))
                     (unless no-sort
                       (put-text-property 0 (length text)
                                          'vuistack-table-column-index index text))
                     text))
                 (cl-mapcar #'cons columns widths)
                 " ")
      (when filter (format "  [filter: %s]" filter)))
     'face 'vuistack-table-header)))

(defun vuistack-table-header-group-lines (columns widths)
  "Heading lines of the group columns above COLUMNS at WIDTHS, outermost first.
A group heading spans its adjacent visible leaf columns (TanStack header
groups); a leaf with no group at a level leaves a blank placeholder."
  (let ((depth (apply #'max 0 (mapcar (lambda (c) (length (plist-get c :vuistack-table--path)))
                                      columns))))
    (cl-loop
     for level below depth
     collect
     (let ((pieces nil) (cols columns) (ws widths))
       (while cols
         (let ((group (nth level (plist-get (car cols) :vuistack-table--path)))
               (width (car ws)))
           (setq cols (cdr cols) ws (cdr ws))
           (while (and group cols
                       (eq group (nth level (plist-get (car cols) :vuistack-table--path))))
             (setq width (+ width 1 (car ws))
                   cols (cdr cols) ws (cdr ws)))
           (push (if group
                     (vuistack-table--pad (or (plist-get group :header) "") width)
                   (make-string width ?\s))
                 pieces)))
       (propertize (mapconcat #'identity (nreverse pieces) " ")
                   'face 'vuistack-table-header)))))

(defun vuistack-table-footer-line (columns widths rows)
  "The footer line for COLUMNS at WIDTHS over the filtered ROWS, or nil.
A column's :footer is a string, a function (ROWS -> string), or
`aggregate' for its :aggregation over ROWS (a grand total)."
  (when (cl-some (lambda (col) (plist-get col :footer)) columns)
    (propertize
     (mapconcat
      (lambda (pair)
        (let* ((col (car pair))
               (footer (plist-get col :footer))
               (text (cond ((eq footer 'aggregate)
                            (vuistack-table-aggregate-string
                             col (vuistack-table-aggregate-column col rows)))
                           ((functionp footer) (format "%s" (or (funcall footer rows) "")))
                           ((stringp footer) footer)
                           (t ""))))
          (vuistack-table--align text (cdr pair)
                                 (or (plist-get col :money)
                                     (eq (plist-get col :align) 'right)))))
      (cl-mapcar #'cons columns widths)
      " ")
     'face 'vuistack-table-footer)))

(provide 'vuistack-table-layout)
;;; vuistack-table-layout.el ends here
