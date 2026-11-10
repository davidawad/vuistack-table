;;; vuistack-table-model.el --- Row and column models for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; The row half of vuistack-table's TanStack Table model (see
;; TANSTACK-PARITY.md): sort functions and the row pipeline core ->
;; filtered -> grouped -> sorted -> expanded -> paginated, plus pinned
;; rows.  Nothing here touches a buffer.
;;
;; A row model works on nodes, plists of
;;   :row ROW   the row plist (a synthetic one for a group row)
;;   :id ID     a string, from the table's :row-id or the index path
;;   :children  sub-row (or sub-group) nodes
;;   :group I   for a group row, the index of its grouping column
;;   :key K     a group row's grouping value
;;   :members   a group row's leaf nodes
;;   :values    a group row's alist of column index -> aggregated value
;;   :cells     a group row's list of cell strings, one per column

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vuistack-table-cell)
(require 'vuistack-table-columns)
(require 'vuistack-table-filter)

;; -- sort functions --

(defun vuistack-table--chunks (text)
  "TEXT split into alternating digit and non-digit runs."
  (let ((pos 0) out)
    (while (string-match "[0-9]+\\|[^0-9]+" text pos)
      (push (match-string 0 text) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun vuistack-table--alphanumeric-compare (a b &optional case-sensitive)
  "Natural order of strings A and B (digit runs numerically): -1, 0 or 1."
  (let ((xs (vuistack-table--chunks (if case-sensitive a (downcase a))))
        (ys (vuistack-table--chunks (if case-sensitive b (downcase b))))
        (result 0))
    (while (and (zerop result) xs ys)
      (let ((x (pop xs)) (y (pop ys)))
        (setq result
              (if (and (string-match-p "\\`[0-9]" x) (string-match-p "\\`[0-9]" y))
                  (let ((m (string-to-number x)) (n (string-to-number y)))
                    (cond ((< m n) -1) ((> m n) 1) (t 0)))
                (cond ((string-lessp x y) -1) ((string-lessp y x) 1) (t 0))))))
    (cond ((/= result 0) result) (xs 1) (ys -1) (t 0))))

(defun vuistack-table--order (lessp a b)
  "-1, 0 or 1 ordering A and B by LESSP."
  (cond ((funcall lessp a b) -1) ((funcall lessp b a) 1) (t 0)))

(defun vuistack-table--compare-values (col a b)
  "-1, 0 or 1 ordering COL's cell values A and B ascending.
COL's :sort-fn is a function (A B -> integer) or a built-in name:
`alphanumeric', `alphanumeric-case-sensitive', `text',
`text-case-sensitive', `datetime' or `basic'.  The default (`auto')
compares two numbers numerically and anything else by its cell string."
  (let ((fn (plist-get col :sort-fn))
        (text (lambda (v) (vuistack-table--string v))))
    (pcase fn
      ('alphanumeric (vuistack-table--alphanumeric-compare (funcall text a) (funcall text b)))
      ('alphanumeric-case-sensitive
       (vuistack-table--alphanumeric-compare (funcall text a) (funcall text b) t))
      ('text (vuistack-table--order #'string-lessp (downcase (funcall text a))
                                    (downcase (funcall text b))))
      ('text-case-sensitive (vuistack-table--order #'string-lessp (funcall text a)
                                                   (funcall text b)))
      ('datetime (vuistack-table--order #'< (or (vuistack-table--time a) 0)
                                        (or (vuistack-table--time b) 0)))
      ('basic (if (and (numberp a) (numberp b))
                  (vuistack-table--order #'< a b)
                (vuistack-table--order #'string-lessp (funcall text a) (funcall text b))))
      ((pred functionp) (let ((n (funcall fn a b))) (cond ((< n 0) -1) ((> n 0) 1) (t 0))))
      (_ (if (and (numberp a) (numberp b))
             (vuistack-table--order #'< a b)
           (vuistack-table--order #'string-lessp (funcall text a) (funcall text b)))))))

(defun vuistack-table--sort-list (sort)
  "SORT, a (COLUMN-INDEX . DESCENDING-P) or a list of them, as a list."
  (cond ((null sort) nil)
        ((integerp (car sort)) (list sort))
        (t sort)))

(defun vuistack-table--compare-by (columns sorts value-a value-b)
  "-1, 0 or 1 ordering two rows by SORTS over COLUMNS.
VALUE-A and VALUE-B map a column index to that row's value.  A column's
:sort-undefined `first' or `last' puts nil values there whatever the
direction; :invert-sorting flips its order (TanStack invertSorting)."
  (let ((result 0))
    (cl-loop for (index . descending) in sorts
             for col = (nth index columns)
             while (zerop result)
             when col
             do (let* ((a (funcall value-a index))
                       (b (funcall value-b index))
                       (undefined (plist-get col :sort-undefined)))
                  (setq result
                        (cond
                         ((and (memq undefined '(first last)) (or (null a) (null b)))
                          (cond ((and (null a) (null b)) 0)
                                ((null a) (if (eq undefined 'first) -1 1))
                                (t (if (eq undefined 'first) 1 -1))))
                         (t (let ((c (vuistack-table--compare-values col a b)))
                              (when (plist-get col :invert-sorting) (setq c (- c)))
                              (if descending (- c) c)))))))
    result))

(defun vuistack-table--node-value (columns node index)
  "NODE's value in column INDEX of COLUMNS (a group row's aggregate)."
  (if (plist-get node :members)
      (alist-get index (plist-get node :values))
    (vuistack-table--cell-value (nth index columns) (plist-get node :row))))

(defun vuistack-table--sort-nodes (columns nodes sort)
  "NODES (and recursively their children) stably sorted by SORT."
  (let ((sorts (cl-remove-if-not (lambda (s) (nth (car s) columns))
                                 (vuistack-table--sort-list sort))))
    (if (null sorts)
        nodes
      (cl-labels
          ((sort-level (level)
             (mapcar
              (lambda (node)
                (if (plist-get node :children)
                    (plist-put (copy-sequence node) :children
                               (sort-level (plist-get node :children)))
                  node))
              (cl-stable-sort
               (copy-sequence level)
               (lambda (x y)
                 (< (vuistack-table--compare-by
                     columns sorts
                     (lambda (i) (vuistack-table--node-value columns x i))
                     (lambda (i) (vuistack-table--node-value columns y i)))
                    0))))))
        (sort-level nodes)))))

;; -- the row model --

(defun vuistack-table--get-sub-rows (options)
  "The (ROW -> sub-rows) reader of OPTIONS (:get-sub-rows, else :sub-rows)."
  (or (plist-get options :get-sub-rows) (lambda (row) (plist-get row :sub-rows))))

(defun vuistack-table--build-nodes (rows options &optional parent-id)
  "Core row model: nodes for ROWS and their sub-rows.
A node's id is (:row-id ROW) per OPTIONS, else its index path (\"0\",
\"0.1\", ...) as in TanStack's default getRowId."
  (let ((get-sub-rows (vuistack-table--get-sub-rows options))
        (row-id (plist-get options :row-id)))
    (cl-loop for row in rows
             for i from 0
             collect
             (let ((path (if parent-id (format "%s.%d" parent-id i) (number-to-string i))))
               (list :row row
                     :id (if row-id (format "%s" (funcall row-id row)) path)
                     :children (vuistack-table--build-nodes
                                (funcall get-sub-rows row) options path))))))

(defun vuistack-table--filter-nodes (nodes passes options &optional depth)
  "NODES kept by PASSES (ROW -> bool), sub-rows filtered too.
OPTIONS :filter-from-leaf-rows keeps a parent when any descendant
passes; otherwise a failing parent drops its subtree.  Rows deeper than
:max-leaf-row-filter-depth are kept unfiltered."
  (let ((depth (or depth 0))
        (max-depth (or (plist-get options :max-leaf-row-filter-depth) 100))
        (from-leaves (plist-get options :filter-from-leaf-rows)))
    (if (> depth max-depth)
        nodes
      (cl-loop
       for node in nodes
       for children = (plist-get node :children)
       for kept = (and children (vuistack-table--filter-nodes children passes options
                                                              (1+ depth)))
       if (if from-leaves
              (or kept (funcall passes (plist-get node :row)))
            (funcall passes (plist-get node :row)))
       collect (if (eq kept children) node (plist-put (copy-sequence node) :children kept))))))

(defun vuistack-table--group-nodes (columns nodes grouping &optional parent-id)
  "NODES grouped by the column indices GROUPING, first-seen order.
Each group row aggregates its leaf members per column :aggregation."
  (if (null grouping)
      nodes
    (let* ((index (car grouping))
           (column (nth index columns))
           (groups nil))
      (dolist (node nodes)
        (let* ((row (plist-get node :row))
               (key (if (plist-get column :grouping-value)
                        (funcall (plist-get column :grouping-value) row)
                      (vuistack-table--cell-value column row)))
               (cell (assoc key groups)))
          (if cell (push node (cdr cell)) (push (list key node) groups))))
      (mapcar
       (lambda (group)
         (let* ((key (car group))
                (members (reverse (cdr group)))
                (id (format "%s%s:%s" (if parent-id (concat parent-id ">") "")
                            (vuistack-table--column-id column) key))
                (rows (mapcar (lambda (n) (plist-get n :row)) members))
                (values
                 (cl-loop for col in columns
                          for i from 0
                          collect
                          (cons i (cond ((= i index) key)
                                        ((plist-get col :aggregation)
                                         (vuistack-table-aggregate
                                          (plist-get col :aggregation)
                                          (mapcar (lambda (r) (vuistack-table--cell-value col r))
                                                  rows)
                                          rows))))))
                (cells
                 (cl-loop for col in columns
                          for i from 0
                          collect
                          (cond ((= i index)
                                 (format "%s (%d)"
                                         (funcall (vuistack-table--formatter col) key)
                                         (length members)))
                                ((plist-get col :aggregation)
                                 (vuistack-table-aggregate-string col (alist-get i values)))
                                (t "")))))
           (list :id id :group index :key key :members members :values values
                 :cells cells
                 :row (list :vuistack-table--group id :emphasis t)
                 :children (vuistack-table--group-nodes columns members (cdr grouping) id))))
       (nreverse groups)))))

(defun vuistack-table--leaf-nodes (nodes)
  "Every node under NODES with no :members (group rows give their members)."
  (cl-mapcan (lambda (node)
               (if (plist-get node :members)
                   (vuistack-table--leaf-nodes (plist-get node :children))
                 (cons node (vuistack-table--leaf-nodes (plist-get node :children)))))
             nodes))

(defun vuistack-table--find-node (nodes id)
  "The node under NODES whose :id is ID, or nil."
  (cl-loop for node in nodes
           thereis (if (equal id (plist-get node :id))
                       node
                     (vuistack-table--find-node (plist-get node :children) id))))

(defun vuistack-table--expanded-p (expanded node)
  "Whether NODE is expanded under EXPANDED (t for all, else a list of ids)."
  (or (eq expanded t) (member (plist-get node :id) expanded)))

(defun vuistack-table--flatten (nodes expanded can-expand &optional depth)
  "Visible entries of NODES: (NODE . DEPTH), expanded children inline.
CAN-EXPAND (NODE -> bool) says which nodes have something to expand."
  (let ((depth (or depth 0)))
    (cl-mapcan (lambda (node)
                 (cons (cons node depth)
                       (and (plist-get node :children)
                            (funcall can-expand node)
                            (vuistack-table--expanded-p expanded node)
                            (vuistack-table--flatten (plist-get node :children)
                                                     expanded can-expand (1+ depth)))))
               nodes)))

(defun vuistack-table-row-model (columns rows state &optional options)
  "Run ROWS through the TanStack row-model pipeline per STATE and OPTIONS.
COLUMNS are leaf columns.  STATE is a table state plist (:sort :filter
:column-filters :grouping :expanded :page :page-size :pin-top
:pin-bottom ...).  OPTIONS is a plist of the table's props (:row-id
:get-sub-rows :manual-filtering :manual-sorting :manual-grouping
:manual-pagination :manual-expanding :row-count :page-count
:filter-from-leaf-rows :max-leaf-row-filter-depth :global-filter-fn
:no-paginate-expanded-rows :drop-filtered-pinned-rows :detail
:row-can-expand).

Return a plist: :core, :filtered and :tree (node lists: every row, those
past the filters, and those grouped and sorted), :filtered-rows (leaf
row plists past the filters), :entries ((NODE . DEPTH) of the page),
:top and :bottom (pinned entries), :page, :page-count, :row-count and
:can-expand (NODE -> bool)."
  (let* ((core (vuistack-table--build-nodes rows options))
         (global (plist-get state :filter))
         (column-filters (plist-get state :column-filters))
         (filtered (if (or (plist-get options :manual-filtering)
                           (and (null global) (null column-filters)))
                       core
                     (vuistack-table--filter-nodes
                      core
                      (lambda (row)
                        (vuistack-table--row-passes-p columns row global column-filters
                                                      (plist-get options :global-filter-fn)))
                      options)))
         (filtered-rows (mapcar (lambda (n) (plist-get n :row))
                                (vuistack-table--leaf-nodes filtered)))
         (grouped (if (plist-get options :manual-grouping)
                      filtered
                    (vuistack-table--group-nodes columns filtered
                                                 (plist-get state :grouping))))
         (sort (plist-get state :sort))
         (sorted
          (cond ((plist-get options :manual-sorting) grouped)
                (sort (vuistack-table--sort-nodes columns grouped sort))
                ;; A fuzzy global filter ranks rows best match first, as
                ;; TanStack's fuzzy sort does, when nothing else sorts.
                ((and global (null (nth 0 global)) (string-prefix-p "~" (nth 1 global))
                      (null (plist-get state :grouping)))
                 (let ((needle (substring (nth 1 global) 1)))
                   (cl-stable-sort
                    (copy-sequence grouped) #'>
                    :key (lambda (node)
                           (or (cl-loop for col in (vuistack-table--global-columns columns)
                                        maximize
                                        (or (cl-loop for s in (vuistack-table--value-strings
                                                               col (plist-get node :row))
                                                     maximize (or (vuistack-table-fuzzy-score
                                                                   needle s)
                                                                  0))
                                            0))
                               0)))))
                (t grouped)))
         (detail (plist-get options :detail))
         (row-can-expand (plist-get options :row-can-expand))
         (can-expand (lambda (node)
                       (and (not (plist-get options :manual-expanding))
                            (or (and (plist-get node :children) t)
                                (and (not (plist-get node :members))
                                     (if row-can-expand
                                         (funcall row-can-expand (plist-get node :row))
                                       (and detail t)))))))
         (expanded (plist-get state :expanded))
         (pin-top (plist-get state :pin-top))
         (pin-bottom (plist-get state :pin-bottom))
         (keep (not (plist-get options :drop-filtered-pinned-rows)))
         (pinned-entry (lambda (id)
                         (let ((node (vuistack-table--find-node (if keep core filtered) id)))
                           (and node (cons node 0)))))
         (top (delq nil (mapcar pinned-entry pin-top)))
         (bottom (delq nil (mapcar pinned-entry pin-bottom)))
         (pinned-ids (append pin-top pin-bottom))
         (unpinned (lambda (entries)
                     (cl-remove-if (lambda (entry)
                                     (member (plist-get (car entry) :id) pinned-ids))
                                   entries)))
         (size (plist-get state :page-size))
         (manual-pages (plist-get options :manual-pagination))
         (paginate-expanded (not (plist-get options :no-paginate-expanded-rows)))
         (units (if (or paginate-expanded (null size))
                    (funcall unpinned (vuistack-table--flatten sorted expanded can-expand))
                  (funcall unpinned (mapcar (lambda (n) (cons n 0)) sorted))))
         (row-count (if manual-pages
                        (or (plist-get options :row-count) (length units))
                      (length units)))
         (page-count (cond ((null size) 1)
                           ((and manual-pages (plist-get options :page-count)))
                           (t (max 1 (ceiling row-count (float size))))))
         (page (min (max 0 (or (plist-get state :page) 0)) (1- page-count)))
         (slice (if (or (null size) manual-pages)
                    units
                  (cl-subseq units (min (* page size) (length units))
                             (min (* (1+ page) size) (length units)))))
         (entries (if (or paginate-expanded (null size))
                      slice
                    (funcall unpinned
                             (vuistack-table--flatten (mapcar #'car slice) expanded
                                                      can-expand)))))
    (list :core core :filtered filtered :filtered-rows filtered-rows :tree sorted
          :entries entries :top top :bottom bottom
          :page page :page-count page-count :row-count row-count
          :can-expand can-expand)))

(provide 'vuistack-table-model)
;;; vuistack-table-model.el ends here
