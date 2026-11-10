;;; vuistack-table-columns.el --- Column model and aggregation for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; The column half of vuistack-table's TanStack Table model (see
;; TANSTACK-PARITY.md): group columns flattened to leaves, column ids,
;; the display column model (visibility, ordering, pinning, scrolling,
;; sizing) and aggregation (group rows, footers, grand totals).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vuistack-table-cell)

;; -- columns --

(defun vuistack-table--leaf-columns (columns)
  "COLUMNS with group columns (those with a :columns list) flattened.
A leaf under a group is a copy carrying :vuistack-table--path, its
ancestor group specs outermost first; any other leaf is itself."
  (let (out)
    (cl-labels ((walk (cols path)
                  (dolist (col cols)
                    (if (plist-get col :columns)
                        (walk (plist-get col :columns) (append path (list col)))
                      (push (if path (append (list :vuistack-table--path path) col) col)
                            out)))))
      (walk columns nil))
    (nreverse out)))

(defun vuistack-table--column-id (col)
  "COL's identity: its :id, else its :key, else its :header."
  (or (plist-get col :id) (plist-get col :key) (plist-get col :header)))

(defun vuistack-table--resolve-column (columns ref)
  "The index in COLUMNS of REF (an index, :id, :key or header), or nil."
  (if (integerp ref)
      (and (< -1 ref (length columns)) ref)
    (cl-position-if
     (lambda (col)
       (or (and (plist-get col :id) (equal ref (plist-get col :id)))
           (and (plist-get col :key) (equal ref (plist-get col :key)))
           (and (stringp ref) (stringp (plist-get col :header))
                (string-equal-ignore-case ref (plist-get col :header)))))
     columns)))

(defun vuistack-table--column-index-of (col columns)
  "COL's index in COLUMNS: its :vuistack-table--index, else its position."
  (or (plist-get col :vuistack-table--index)
      (cl-position col columns :test #'eq)))

(defun vuistack-table--display-columns (columns state &optional grouped-mode)
  "Annotated copies of the COLUMNS STATE shows, in display order.
STATE is a table state plist (see `vuistack-table-state').  The order is
the left-pinned columns (:pin-left), the centre ones (:order, with the
:grouping columns first under GROUPED-MODE `reorder', the default, left
out under `remove', kept in place under `keep'), then the right-pinned
ones (:pin-right).  :hidden columns are dropped and the first :scroll
centre columns scrolled away.  Each copy carries :vuistack-table--index
\(its index in COLUMNS), :vuistack-table--pin (left, right or nil) and,
when STATE :widths sizes it, :width."
  (let* ((n (length columns))
         (valid (lambda (list) (cl-remove-if-not (lambda (i) (and (integerp i) (< -1 i n)))
                                                 list)))
         (hidden (plist-get state :hidden))
         (grouping (funcall valid (plist-get state :grouping)))
         (order (funcall valid (plist-get state :order)))
         (order (append order (cl-remove-if (lambda (i) (memq i order))
                                            (number-sequence 0 (1- n)))))
         (order (pcase (or grouped-mode 'reorder)
                  ('reorder (append grouping (cl-remove-if (lambda (i) (memq i grouping))
                                                           order)))
                  ('remove (cl-remove-if (lambda (i) (memq i grouping)) order))
                  (_ order)))
         (left (cl-remove-if-not (lambda (i) (memq i order))
                                 (funcall valid (plist-get state :pin-left))))
         (right (cl-remove-if (lambda (i) (or (memq i left) (not (memq i order))))
                              (funcall valid (plist-get state :pin-right))))
         (centre (cl-remove-if (lambda (i) (or (memq i left) (memq i right) (memq i hidden)))
                               order))
         (centre (nthcdr (min (max 0 (or (plist-get state :scroll) 0))
                              (max 0 (1- (length centre))))
                         centre))
         (widths (plist-get state :widths)))
    (cl-loop for (side . indices) in (list (cons 'left left) (cons nil centre)
                                           (cons 'right right))
             nconc
             (cl-loop for i in indices
                      unless (memq i hidden)
                      collect (let ((width (alist-get i widths)))
                                (append (list :vuistack-table--index i
                                              :vuistack-table--pin side)
                                        (and width (list :width width))
                                        (nth i columns)))))))

(defun vuistack-table--clamp-width (col width)
  "WIDTH within COL's :min-width and :max-width."
  (let ((low (plist-get col :min-width))
        (high (plist-get col :max-width)))
    (max (or low 1) (if high (min high width) width))))

;; -- aggregation --

(defun vuistack-table-aggregate (aggregation values &optional rows)
  "VALUES (with their ROWS) reduced by AGGREGATION.
AGGREGATION is a built-in name -- `sum', `count', `min', `max',
`extent' (a (MIN . MAX)), `mean', `median', `unique', `unique-count',
`first', `last' or `auto' (sum when the first value is a number) -- or a
function (VALUES ROWS -> value), or a list of those, which gives an
alist of name -> result."
  (let ((numbers (delq nil (mapcar #'vuistack-table--number values))))
    (pcase aggregation
      ('sum (apply #'+ numbers))
      ('count (length values))
      ('min (and numbers (apply #'min numbers)))
      ('max (and numbers (apply #'max numbers)))
      ('extent (and numbers (cons (apply #'min numbers) (apply #'max numbers))))
      ('mean (and numbers (/ (float (apply #'+ numbers)) (length numbers))))
      ('median (and numbers (cl-every #'numberp values)
                    (let* ((sorted (sort (copy-sequence numbers) #'<))
                           (n (length sorted)))
                      (if (cl-oddp n)
                          (nth (/ n 2) sorted)
                        (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2.0)))))
      ('unique (delete-dups (copy-sequence values)))
      ('unique-count (length (delete-dups (copy-sequence values))))
      ('first (car values))
      ('last (car (last values)))
      ('auto (and (numberp (car values)) (apply #'+ numbers)))
      ((pred functionp) (funcall aggregation values rows))
      ((pred consp)
       (mapcar (lambda (a) (cons a (vuistack-table-aggregate a values rows))) aggregation)))))

(defun vuistack-table--rows-to-depth (rows depth get-sub-rows)
  "The frontier of ROWS down to sub-row DEPTH (0 is ROWS themselves).
A branch ending sooner gives its deepest row; GET-SUB-ROWS reads a row's."
  (if (or (<= depth 0) (null rows))
      rows
    (cl-mapcan (lambda (row)
                 (let ((subs (funcall get-sub-rows row)))
                   (if subs
                       (vuistack-table--rows-to-depth subs (1- depth) get-sub-rows)
                     (list row))))
               rows)))

(defun vuistack-table-aggregate-column (column rows &optional max-depth get-sub-rows)
  "COLUMN's :aggregation over ROWS, for a grand total or a footer.
MAX-DEPTH (default COLUMN's :max-aggregation-depth, else 0) aggregates
the sub-row frontier that deep, sub-rows read by GET-SUB-ROWS (default
the :sub-rows key).  Nil when COLUMN has no :aggregation."
  (when-let ((aggregation (plist-get column :aggregation)))
    (let ((rows (vuistack-table--rows-to-depth
                 rows (or max-depth (plist-get column :max-aggregation-depth) 0)
                 (or get-sub-rows (lambda (row) (plist-get row :sub-rows))))))
      (vuistack-table-aggregate aggregation
                                (mapcar (lambda (row) (vuistack-table--cell-value column row))
                                        rows)
                                rows))))

(defun vuistack-table-aggregate-string (column value)
  "Aggregated VALUE of COLUMN as cell text.
COLUMN's :aggregated-format (VALUE -> string) wins; an extent shows as
MIN – MAX, a list of values comma-joined, a multi-aggregation alist as
NAME VALUE pairs, anything else through the column's formatter."
  (let ((format (vuistack-table--formatter column)))
    (cond ((plist-get column :aggregated-format)
           (funcall (plist-get column :aggregated-format) value))
          ((null value) "")
          ((and (consp value) (consp (car value)) (symbolp (caar value)))
           (mapconcat (lambda (pair)
                        (format "%s %s" (car pair)
                                (vuistack-table-aggregate-string
                                 (plist-put (copy-sequence column) :aggregated-format nil)
                                 (cdr pair))))
                      value "; "))
          ((and (consp value) (numberp (car value)) (numberp (cdr value)))
           (format "%s – %s" (funcall format (car value)) (funcall format (cdr value))))
          ((consp value) (mapconcat (lambda (v) (funcall format v)) value ", "))
          (t (funcall format value)))))

(provide 'vuistack-table-columns)
;;; vuistack-table-columns.el ends here
