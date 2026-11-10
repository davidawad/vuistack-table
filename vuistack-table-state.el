;;; vuistack-table-state.el --- Per-table state and sort commands for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; Table state is buffer-local and keyed by table ID, so the tables of
;; one buffer sort, filter, page, group, select ... independently.  The
;; state of a table is a plist (`vuistack-table-state'), TanStack
;; Table's TableState in Lisp:
;;
;;   :sort            (COLUMN-INDEX . DESCENDING-P), or a list of them
;;   :filter          the global filter, (COLUMN-INDEX QUERY DISPLAY)
;;   :column-filters  alist COLUMN-INDEX -> QUERY
;;   :hidden          column indices hidden
;;   :order           column indices in the order set by hand
;;   :pin-left :pin-right   pinned column indices
;;   :widths          alist COLUMN-INDEX -> width set by hand
;;   :scroll          centre columns scrolled out of view
;;   :grouping        grouping column indices, outermost first
;;   :expanded        t (everything) or a list of row ids
;;   :selected        selected row ids
;;   :pin-top :pin-bottom   pinned row ids
;;   :page :page-size pagination
;;   :cell-ranges     cell selection ranges, plists (:anchor (ROW-ID
;;                    . COLUMN-INDEX) :focus (ROW-ID . COLUMN-INDEX)
;;                    :exclude BOOL)
;;
;; Column indices count the table's leaf columns.  The commands read
;; the table at point from the text properties every table line carries;
;; this file holds the state store and the sort commands, and
;; vuistack-table-rows.el and vuistack-table-commands.el the rest.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'vuistack-table-cell)
(require 'vuistack-table-model)

(defcustom vuistack-table-sort-removal t
  "Non-nil: a third sort on a column clears it (TanStack enableSortingRemoval).
Nil: sorting only flips between ascending and descending."
  :type 'boolean
  :group 'vuistack-table)

;; -- sorting --

(defvar-local vuistack-table--sort-state nil
  "Alist of TABLE-ID -> sort.
A sort is a (COLUMN-INDEX . DESCENDING-P) or a list of them.")

(defun vuistack-table--table-sort (id)
  "The sort of table ID in this buffer: a (COLUMN-INDEX . DESCENDING-P), a
list of them (a multi-sort), or nil."
  (alist-get id vuistack-table--sort-state nil nil #'equal))

(defun vuistack-table--sorted-rows (columns rows sort)
  "Return ROWS sorted per SORT, a (COLUMN-INDEX . DESCENDING-P), a list of
them (a multi-sort, first key first) or nil.  Each column's :sort-fn,
:sort-undefined and :invert-sorting apply (see
`vuistack-table--compare-by'); by default numbers compare numerically,
anything else by its cell string."
  (let ((sorts (cl-remove-if-not (lambda (s) (nth (car s) columns))
                                 (vuistack-table--sort-list sort))))
    (if (null sorts)
        rows
      (cl-stable-sort
       (copy-sequence rows)
       (lambda (left right)
         (< (vuistack-table--compare-by
             columns sorts
             (lambda (i) (vuistack-table--cell-value (nth i columns) left))
             (lambda (i) (vuistack-table--cell-value (nth i columns) right)))
            0))))))

;; -- filtering --

(defvar-local vuistack-table--filter-state nil
  "Alist of TABLE-ID -> (COLUMN-INDEX QUERY DISPLAY), one per table.
A nil COLUMN-INDEX searches all visible columns.")

(defun vuistack-table--table-filter (id)
  "The filter spec for table ID in this buffer, or nil."
  (alist-get id vuistack-table--filter-state nil nil #'equal))

(defun vuistack-table--parse-filter (columns text)
  "Parse filter TEXT into (COLUMN-INDEX QUERY DISPLAY), or nil.
An exact, case-insensitive column header followed by a colon scopes the
query to that visible column.  Unknown prefixes remain an all-column
query, so ordinary text containing a colon still works."
  (let ((display (string-trim text)))
    (unless (string-empty-p display)
      (let* ((colon (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" display))
             (label (and colon (string-trim (match-string 1 display))))
             (index (and colon
                         (cl-position-if
                          (lambda (column)
                            (string-equal-ignore-case
                             label (or (plist-get column :header) "")))
                          columns)))
             (query (if index (string-trim (match-string 2 display)) display)))
        (unless (string-empty-p query)
          (list index query display))))))

(defun vuistack-table--filter-rows (columns rows filter)
  "Return ROWS matching FILTER across COLUMNS.
FILTER is (COLUMN-INDEX QUERY DISPLAY); nil means no filter.  QUERY is
the filter language of `vuistack-table--parse-query': plain text
matches when it is a case-insensitive substring of a cell's formatted
text."
  (if (null filter)
      rows
    (cl-remove-if-not
     (lambda (row) (vuistack-table--row-passes-p columns row filter nil))
     rows)))

(defun vuistack-table--table-at-point ()
  "Return (TABLE-ID COLUMNS) for the table line at point."
  (let ((position (line-beginning-position)))
    (list (or (get-text-property position 'vuistack-table-id) 'default)
          (get-text-property position 'vuistack-table-columns))))

;; -- the rest of the state --

(defvar-local vuistack-table--table-states nil
  "Alist of TABLE-ID -> state plist, all of it but :sort and :filter.")

(defvar-local vuistack-table--tables nil
  "Alist of TABLE-ID -> plist the table's last render recorded:
:columns (leaf columns), :rows, :options (its props), :model (see
`vuistack-table-row-model'), :display (display columns), :widths
\(alist COLUMN-INDEX -> rendered width) and :row-order (visible row ids
in order).")

(defun vuistack-table--table (id)
  "What the last render of table ID recorded (see `vuistack-table--tables')."
  (alist-get id vuistack-table--tables nil nil #'equal))

(defun vuistack-table-state (&optional id)
  "The state plist of table ID (default: the table at point).
See the commentary of vuistack-table-state.el for its keys."
  (let ((id (or id (car (vuistack-table--table-at-point)))))
    (append (list :sort (vuistack-table--table-sort id)
                  :filter (vuistack-table--table-filter id))
            (alist-get id vuistack-table--table-states nil nil #'equal))))

(defun vuistack-table--put (id key value)
  "Set KEY of table ID's state to VALUE."
  (pcase key
    (:sort (setf (alist-get id vuistack-table--sort-state nil nil #'equal) value))
    (:filter (setf (alist-get id vuistack-table--filter-state nil nil #'equal) value))
    (_ (setf (alist-get id vuistack-table--table-states nil nil #'equal)
             (plist-put (copy-sequence (alist-get id vuistack-table--table-states
                                                  nil nil #'equal))
                        key value)))))

(defun vuistack-table--get (id key)
  "KEY of table ID's state."
  (plist-get (vuistack-table-state id) key))

(defun vuistack-table--changed (id &optional reset-page)
  "Table ID's state changed: reset its page when RESET-PAGE (TanStack
autoResetPageIndex), tell its :on-state-change, repaint."
  (when reset-page (vuistack-table--put id :page 0))
  (when-let ((fn (plist-get (plist-get (vuistack-table--table id) :options)
                            :on-state-change)))
    (funcall fn id (vuistack-table-state id)))
  (vui-refresh))

(defun vuistack-table-set-state (id &rest plist)
  "Set the keys of PLIST in table ID's state, tell :on-state-change, repaint."
  (cl-loop for (key value) on plist by #'cddr
           do (vuistack-table--put id key value))
  (vuistack-table--changed id))

(defun vuistack-table--resolve-state (columns state)
  "STATE (an :initial-state prop) with column ids resolved to COLUMNS indices.
Column references may be indices, :id or :key values, or headers."
  (let ((one (lambda (ref) (vuistack-table--resolve-column columns ref)))
        (out nil))
    (cl-loop
     for (key value) on state by #'cddr
     do (setq out
              (plist-put
               out key
               (pcase key
                 (:sort (let ((sorts (cl-loop for (ref . desc)
                                              in (if (and (consp value) (atom (car value)))
                                                     (list value)
                                                   value)
                                              for i = (funcall one ref)
                                              when i collect (cons i desc))))
                          (if (cdr sorts) sorts (car sorts))))
                 (:filter (if (stringp value)
                              (vuistack-table--parse-filter columns value)
                            value))
                 ((or :column-filters :widths)
                  (cl-loop for (ref . v) in value
                           for i = (funcall one ref)
                           when i collect (cons i v)))
                 ((or :hidden :order :pin-left :pin-right :grouping)
                  (delq nil (mapcar one value)))
                 (_ value)))))
    out))

(defun vuistack-table--seed (id columns initial-state page-size)
  "Give table ID its initial state the first time it renders.
INITIAL-STATE is the table's :initial-state (column references
resolved against COLUMNS), PAGE-SIZE its :page-size prop; columns with
:hidden t start hidden."
  (unless (assoc id vuistack-table--table-states)
    (let* ((state (vuistack-table--resolve-state columns initial-state))
           (hidden (cl-loop for col in columns for i from 0
                            when (plist-get col :hidden) collect i)))
      (when (and hidden (not (plist-member state :hidden)))
        (setq state (plist-put state :hidden hidden)))
      (when (and page-size (not (plist-member state :page-size)))
        (setq state (plist-put state :page-size page-size)))
      (when (and (plist-member state :sort) (not (vuistack-table--table-sort id)))
        (vuistack-table--put id :sort (plist-get state :sort)))
      (when (and (plist-member state :filter) (not (vuistack-table--table-filter id)))
        (vuistack-table--put id :filter (plist-get state :filter)))
      (cl-remf state :sort)
      (cl-remf state :filter)
      (push (cons id state) vuistack-table--table-states))))

(defun vuistack-table-reset-state ()
  "Forget every table's state in this buffer."
  (setq vuistack-table--sort-state nil
        vuistack-table--filter-state nil
        vuistack-table--table-states nil))

;; -- reading the table at point --

(defun vuistack-table-row-at-point ()
  "The row plist of the table line at point, or nil."
  (get-text-property (line-beginning-position) 'vuistack-table-row))

(defun vuistack-table--column-at-point ()
  "The column index of the table cell at point, or nil."
  (or (get-text-property (point) 'vuistack-table-column-index)
      (get-text-property (line-beginning-position) 'vuistack-table-column-index)))

(defun vuistack-table--context (&optional need-column)
  "(ID COLUMNS INDEX) of the table at point; INDEX is the column at point.
Signal a `user-error' off a table, or off a column when NEED-COLUMN."
  (pcase-let ((`(,id ,columns) (vuistack-table--table-at-point)))
    (unless columns
      (user-error "Move point onto a table first"))
    (let ((index (vuistack-table--column-at-point)))
      (when (and need-column (not (integerp index)))
        (user-error "Move point onto a table column first"))
      (list id columns index))))

(defun vuistack-table--node-at-point ()
  "The row-model node of the table line at point, or a `user-error'."
  (or (get-text-property (line-beginning-position) 'vuistack-table-node)
      (user-error "Move point onto a table row first")))

(defun vuistack-table--column-allows (column flag what)
  "Signal a `user-error' when COLUMN sets FLAG, naming WHAT is disabled."
  (when (plist-get column flag)
    (user-error "Column %s does not allow %s" (or (plist-get column :header) "") what)))

;; -- commands: rows and cells --

(defun vuistack-table-activate ()
  "Call the `vuistack-table-action' of the table cell at point.
Non-nil when there was one (a cell or table :action)."
  (let ((action (get-text-property (point) 'vuistack-table-action)))
    (when (functionp action)
      (funcall action)
      t)))

(defun vuistack-table-edit-cell (text)
  "Set the cell at point to TEXT through its column's :on-edit (ROW TEXT)."
  (interactive
   (pcase-let* ((`(,_id ,columns ,index) (vuistack-table--context t))
                (column (nth index columns))
                (row (vuistack-table-row-at-point)))
     (unless (plist-get column :on-edit)
       (user-error "Column %s is not editable" (or (plist-get column :header) "")))
     (list (read-string (format "%s: " (or (plist-get column :header) "Value"))
                        (funcall (vuistack-table--formatter column)
                                 (vuistack-table--cell-value column row))))))
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (column (nth index columns)))
    (unless (plist-get column :on-edit)
      (user-error "Column %s is not editable" (or (plist-get column :header) "")))
    (funcall (plist-get column :on-edit) (vuistack-table-row-at-point) text)
    (vuistack-table--changed id)))

;; -- commands: sorting --

(defun vuistack-table-sort (&optional multi)
  "Sort the table at point by the column at point.
Repeat to reverse, and again to clear it (`vuistack-table-sort-removal').
A column's :sort-desc-first starts it descending.  With a prefix
argument MULTI, cycle the column within the table's multi-sort instead
of replacing the sort (at most the table's :max-multi-sort-count keys)."
  (interactive "P")
  (pcase-let* ((`(,id ,columns ,index) (vuistack-table--context t))
               (column (nth index columns))
               (options (plist-get (vuistack-table--table id) :options))
               (multi (and multi (not (plist-get options :disable-multi-sort))))
               (sorts (vuistack-table--sort-list (vuistack-table--table-sort id)))
               (current (assq index sorts))
               (first-desc (and (plist-get column :sort-desc-first) t))
               (next (cond ((null current) (cons index first-desc))
                           ((eq (and (cdr current) t) first-desc)
                            (cons index (not (cdr current))))
                           ((and vuistack-table-sort-removal
                                 (not (plist-get options :disable-sort-removal)))
                            nil)
                           (t (cons index first-desc))))
               (sorts (if multi
                          (let ((kept (cl-remove index sorts :key #'car)))
                            (if next
                                (if current
                                    (mapcar (lambda (s) (if (eql (car s) index) next s)) sorts)
                                  (append kept (list next)))
                              kept))
                        (and next (list next))))
               (limit (plist-get options :max-multi-sort-count)))
    (vuistack-table--column-allows column :disable-sort "sorting")
    (when (and limit (> (length sorts) limit))
      (setq sorts (last sorts limit)))
    (vuistack-table--put id :sort (if (cdr sorts) sorts (car sorts)))
    (vuistack-table--changed id t)
    (message "%s"
             (if (null next)
                 (format "Column %d unsorted" (1+ index))
               (format "Sorted column %d %s" (1+ index)
                       (if (cdr next) "descending" "ascending"))))))

(defun vuistack-table-sort-reset ()
  "Clear every table's sort in this buffer and repaint without refetching."
  (interactive)
  (setq vuistack-table--sort-state nil)
  (vui-refresh))

(provide 'vuistack-table-state)
;;; vuistack-table-state.el ends here
