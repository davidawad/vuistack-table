;;; vuistack-table.el --- TanStack-Table-style table component for vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (vui "1.4"))
;; URL: https://github.com/davidawad/vuistack-table
;; Keywords: tools, convenience
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `vuistack-table' is a vui.el (https://github.com/d12frosted/vui.el)
;; component: an Emacs replica of TanStack Table (https://tanstack.com/table).
;; A table of plist rows with zebra striping, money and signed-number
;; cells, KPI-rule indicator cells, multi-line cell renderers and cell
;; actions, plus TanStack's features: multi-sorting with sort
;; functions, column and global filtering (fuzzy too) over a filter
;; language, faceting, grouping with aggregation, expanding sub-rows and
;; detail panels, pagination, row selection and pinning, column
;; visibility, ordering, pinning and sizing, header groups, footers,
;; cell spanning and cell selection.  TANSTACK-PARITY.md maps every
;; TanStack feature to its vuistack-table form; README.md documents the
;; props and column keys.
;;
;;   (vui-mount
;;    (vui-component 'vuistack-table
;;                   :id 'accounts
;;                   :columns '((:header "Account" :key :name)
;;                              (:header "Balance" :key :balance :money t
;;                               :sign-color t :aggregation sum
;;                               :footer aggregate))
;;                   :rows '((:name "Cash" :balance 1200)
;;                           (:name "Card" :balance -310.5))
;;                   :zebra t :page-size 25)
;;    "*accounts*")
;;
;; The files: vuistack-table-cell (cell values), -kpi (indicator
;; faces), -columns (column model, aggregation), -filter (filter
;; functions, faceting), -model (sorting, the row pipeline), -layout
;; (line strings), -state (the per-table state store, sorting), -rows
;; and -commands (the commands and `vuistack-table-mode'), and this one
;; (assembling a table, the component).
;;
;; A table renders one text vnode for all its lines (not one widget per
;; cell), so large tables stay cheap to reconcile.  Each line carries its
;; row plist as the text property `vuistack-table-row', its row-model
;; node as `vuistack-table-node', and its table as `vuistack-table-id'
;; and `vuistack-table-columns'.  State is buffer-local and keyed by
;; table ID, so several tables in one buffer keep their own.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vui)
(require 'vuistack-table-cell)
(require 'vuistack-table-kpi)
(require 'vuistack-table-model)
(require 'vuistack-table-state)
(require 'vuistack-table-layout)
(require 'vuistack-table-rows)
(require 'vuistack-table-commands)

(defvar-local vuistack-table--sticky-registry nil)
(defvar-local vuistack-table--sticky-saved-header nil)
(defvar-local vuistack-table--sticky-overlays nil)
(defvar-local vuistack-table--sticky-p nil
  "Non-nil in a buffer that has drawn a table with pinned columns or a
sticky header, so the scroll hooks skip every other buffer.")
(defvar-local vuistack-table--sticky-key nil
  "What the sticky overlays were last drawn for: each window's start and
hscroll, and the buffer's modification tick.")

(defun vuistack-table--sticky-header (&optional pos)
  "Return the sticky table header at POS, or nil outside a table body."
  (let ((pos (or pos (window-start))))
      (when-let ((entry (cl-find-if (lambda (e) (and (> pos (nth 0 e)) (< pos (nth 1 e))))
                                  vuistack-table--sticky-registry)))
      (save-excursion
        (goto-char (nth 2 entry))
        (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
               (h (max 0 (window-hscroll)))
               (pin (nth 3 entry))
               (line (if (and (> h 0) (> pin 0))
                         (concat (substring line 0 (min pin (length line)))
                                 (substring line (min (+ h pin) (length line))))
                       (substring line (min h (length line))))))
          (concat (vui--sticky-header-padding)
                  (replace-regexp-in-string "%" "%%" line)))))))

(defun vuistack-table--sticky-header-format ()
  "Compose a table header with the header-line format that was present before it."
  (let ((sticky (vuistack-table--sticky-header)))
    (if sticky
        (concat sticky (or (and vuistack-table--sticky-saved-header
                                (format-mode-line (cdr vuistack-table--sticky-saved-header))) ""))
      (and vuistack-table--sticky-saved-header
           (format-mode-line (cdr vuistack-table--sticky-saved-header)) ""))))

(defun vuistack-table--sticky-register-visible-tables ()
  "Refresh sticky table regions and per-window pinned-column overlays."
  (when (derived-mode-p 'fundamental-mode 'special-mode)
    (let ((pos (point-min)) regions)
      (while (< pos (point-max))
        (let ((id (get-text-property pos 'vuistack-table-id)))
          (if (null id) (setq pos (or (next-single-property-change pos 'vuistack-table-id) (point-max)))
            (let* ((start pos)
                   (end (save-excursion
                          (goto-char pos)
                          (let ((last pos) (continue t))
                            (while (and continue (< (point) (point-max))
                                        (eq id (get-text-property (point) 'vuistack-table-id)))
                              (setq last (line-end-position))
                              (if (>= last (point-max))
                                  (setq continue nil)
                                (forward-line 1)
                                (beginning-of-line)
                                (unless (eq id (get-text-property (point) 'vuistack-table-id))
                                  (setq continue nil))))
                            last)))
                   (table (vuistack-table--table id))
                   (opts (plist-get table :options))
                   (left (plist-get (vuistack-table-state id) :pin-left))
                   (widths (plist-get table :widths))
                   (pinned-width (+ (cl-loop for (index . width) in widths
                                            when (memq index left) sum width)
                                    (max 0 (1- (cl-count-if (lambda (pair)
                                                            (memq (car pair) left))
                                                          widths)))))
                   (header (plist-get opts :sticky-header)))
              (when (and header (plist-get opts :sticky-header)
                         (not (plist-get opts :no-header)))
                (let ((scan start) last-header)
                  (while (and (< scan end)
                              (not (get-text-property scan 'vuistack-table-row)))
                    (when (eq (get-text-property scan 'face) 'vuistack-table-header)
                      (setq last-header scan))
                    (setq scan (save-excursion
                                 (goto-char scan)
                                 (min end (1+ (line-end-position))))))
                  (when last-header
                    (push (list (save-excursion (goto-char last-header) (line-end-position))
                                end last-header pinned-width)
                          regions))))
              (when (> pinned-width 0)
                (dolist (window (get-buffer-window-list (current-buffer) nil t))
                  (let ((h (window-hscroll window)))
                    (when (> h 0)
                      (save-excursion
                        (goto-char (max start (window-start window)))
                        (beginning-of-line)
                        (while (and (< (point) end) (<= (point) (window-end window t)))
                          (when (eq id (get-text-property (point) 'vuistack-table-id))
                            (let* ((line-start (point)) (line-end (line-end-position))
                                   (text (buffer-substring line-start (min line-end (+ line-start pinned-width))))
                                   (text (concat text (make-string (max 0 (- pinned-width (string-width text))) ?\s)))
                                   (ov-start (min line-end (+ line-start h)))
                                   (ov (make-overlay ov-start
                                                     (min line-end (+ line-start h pinned-width))
                                                     nil t t)))
                              (overlay-put ov 'vuistack-table-sticky t)
                              (overlay-put ov 'window window)
                              (overlay-put ov 'display text)
                              (push ov vuistack-table--sticky-overlays)))
                          (forward-line 1)))))))
              (setq pos (min (point-max) (1+ end)))))))
      (setq vuistack-table--sticky-registry (nreverse regions))
      (when vuistack-table--sticky-registry
        (unless vuistack-table--sticky-saved-header
          (setq vuistack-table--sticky-saved-header (cons t header-line-format)))
        (setq header-line-format '(:eval (vuistack-table--sticky-header-format))))
      (unless vuistack-table--sticky-registry
        (when vuistack-table--sticky-saved-header
          (setq header-line-format (cdr vuistack-table--sticky-saved-header)
                vuistack-table--sticky-saved-header nil))))))

(defun vuistack-table--sticky-refresh (&optional buffer)
  "Update the pinned-column overlays and sticky headers of BUFFER.
BUFFER defaults to the current one.  Does nothing in a buffer without a
sticky table, or when no window of it has scrolled since the last call
and the buffer is unchanged."
  (with-current-buffer (or buffer (current-buffer))
    (when vuistack-table--sticky-p
      (let ((key (cons (buffer-modified-tick)
                       (mapcar (lambda (w) (list w (window-start w) (window-hscroll w)
                                                 (window-end w)))
                               (get-buffer-window-list nil nil t)))))
        (unless (equal key vuistack-table--sticky-key)
          (setq vuistack-table--sticky-key key)
          (mapc #'delete-overlay vuistack-table--sticky-overlays)
          (setq vuistack-table--sticky-overlays nil)
          (vuistack-table--sticky-register-visible-tables))))))

(defun vuistack-table--sticky-on-scroll (window _start)
  "Refresh the sticky state of WINDOW's buffer after it scrolled."
  (vuistack-table--sticky-refresh (window-buffer window)))

(add-hook 'post-command-hook #'vuistack-table--sticky-refresh)
(add-hook 'window-scroll-functions #'vuistack-table--sticky-on-scroll)

;; -- assembling a table --

(defun vuistack-table--group-columns (display node)
  "Columns rendering group row NODE across the DISPLAY columns.
Each reads its cell string from NODE's :cells."
  (let ((cells (plist-get node :cells)))
    (mapcar (lambda (col)
              (let ((index (plist-get col :vuistack-table--index)))
                (list :header (plist-get col :header)
                      :vuistack-table--index index
                      :vuistack-table--pin (plist-get col :vuistack-table--pin)
                      :get (lambda (_row) (nth index cells))
                      :align (and (or (plist-get col :money)
                                      (eq (plist-get col :align) 'right))
                                  'right)
                      :sign-color (plist-get col :sign-color))))
            display)))

(defun vuistack-table--prefix (node depth can-expand expanded any-expander)
  "Indentation and expander for NODE at DEPTH, or nil when it needs none."
  (let ((text (concat (make-string (* 2 depth) ?\s)
                      (cond ((funcall can-expand node)
                             (if (vuistack-table--expanded-p expanded node) "▾ " "▸ "))
                            (any-expander "  ")
                            (t "")))))
    (unless (string-empty-p text) text)))

(defun vuistack-table--selection-mark (node options selected-index)
  "The selection gutter of NODE: [x] all, [-] some, [ ] none, or blank."
  (let ((ids (vuistack-table--selection-ids node options)))
    (cond ((null ids) "    ")
          ((cl-every (lambda (i) (gethash i selected-index)) ids) "[x] ")
          ((cl-some (lambda (i) (gethash i selected-index)) ids) "[-] ")
          (t "[ ] "))))

(defun vuistack-table--mark-cells (line indices)
  "LINE with the cells of column INDICES in `vuistack-table-cell-selected'."
  (when indices
    (let ((pos 0) (len (length line)))
      (while (< pos len)
        (let ((next (next-single-property-change pos 'vuistack-table-column-index line len)))
          (when (memq (get-text-property pos 'vuistack-table-column-index line) indices)
            (add-face-text-property pos next 'vuistack-table-cell-selected nil line))
          (setq pos next)))))
  line)

(defun vuistack-table--with-gutter (gutter line)
  "LINE (maybe several lines) led by GUTTER, its later lines by spaces."
  (if (string-empty-p gutter)
      line
    (concat gutter
            (replace-regexp-in-string
             "\n" (concat "\n" (make-string (string-width gutter) ?\s)) line t t))))

(defun vuistack-table--span-blanks (display node anchors)
  "Columns of DISPLAY whose cell for NODE continues a row-spanned run.
ANCHORS is a cons whose car is the alist COLUMN -> run anchor value; it
is updated.  A column's :span-rows is t (equal non-nil values merge) or
a predicate (ANCHOR-VALUE VALUE -> bool)."
  (let ((blank nil))
    (dolist (col display)
      (when-let ((spec (plist-get col :span-rows)))
        (let* ((value (vuistack-table--cell-value col (plist-get node :row)))
               (anchor (assq col (car anchors)))
               (same (and anchor
                          (if (functionp spec)
                              (funcall spec (cdr anchor) value)
                            (and value (equal value (cdr anchor)))))))
          (if same
              (push col blank)
            (setf (alist-get col (car anchors)) value)))))
    blank))

(defun vuistack-table--fit-group-headers (display widths)
  "WIDTHS of DISPLAY widened so every group heading fits over its run.
The last auto-sized column of a too-narrow run takes the difference."
  (let ((widths (copy-sequence widths))
        (depth (apply #'max 0 (mapcar (lambda (c) (length (plist-get c :vuistack-table--path)))
                                      display))))
    (dotimes (level depth)
      (let ((i 0) (n (length display)))
        (while (< i n)
          (let ((group (nth level (plist-get (nth i display) :vuistack-table--path)))
                (j (1+ i)))
            (while (and group (< j n)
                        (eq group (nth level (plist-get (nth j display) :vuistack-table--path))))
              (cl-incf j))
            (when group
              (let ((room (+ (apply #'+ (cl-subseq widths i j)) (- j i 1)))
                    (need (string-width (or (plist-get group :header) "")))
                    (last (cl-position-if-not (lambda (c) (plist-get c :width))
                                              (cl-subseq display i j) :from-end t)))
                (when (and last (> need room))
                  (cl-incf (nth (+ i last) widths) (- need room)))))
            (setq i j)))))
    widths))

(defun vuistack-table--number-columns (display number)
  "DISPLAY with each :row-number column reading NUMBER, the display index."
  (if (cl-some (lambda (col) (plist-get col :row-number)) display)
      (mapcar (lambda (col)
                (if (plist-get col :row-number)
                    (append (list :get (lambda (_row) number)) col)
                  col))
              display)
    display))

(defun vuistack-table--render-lines (id columns rows props)
  "Every line of table ID (leaf COLUMNS, ROWS, the table's PROPS plist).
Records what commands need in `vuistack-table--tables'.  With PROPS
:auto-reset, new ROWS reset the page and the expanded rows (TanStack
autoResetPageIndex and autoResetExpanded)."
  (let* ((previous (vuistack-table--table id))
         (_ (when (and (plist-get props :auto-reset) previous
                       (not (equal rows (plist-get previous :rows))))
              (vuistack-table--put id :page 0)
              (vuistack-table--put id :expanded nil)))
         (state (vuistack-table-state id))
         (model (vuistack-table-row-model columns rows state props))
         (display (vuistack-table--display-columns columns state
                                                   (plist-get props :grouped-column-mode)))
         (no-sort (plist-get props :no-sort))
         (zebra (plist-get props :zebra))
         (can-expand (plist-get model :can-expand))
         (expanded (plist-get state :expanded))
         (selected (plist-get state :selected))
         (selection (plist-get props :enable-row-selection))
         (selected-index
          (when selection
            (let ((index (make-hash-table :test #'equal)))
              (dolist (id selected) (puthash id t index))
              index)))
         (top (plist-get model :top))
         (body (plist-get model :entries))
         (bottom (plist-get model :bottom))
         (all (append top body bottom))
         (any-expander (cl-some (lambda (e) (funcall can-expand (car e))) all))
         (prefixes (mapcar (lambda (e) (vuistack-table--prefix (car e) (cdr e) can-expand
                                                               expanded any-expander))
                           all))
         (widths (vuistack-table--col-widths
                  display (append (plist-get model :filtered-rows)
                                  (mapcar (lambda (e) (plist-get (car e) :row))
                                          (append top bottom)))))
         ;; Group rows and the expander column widen auto-sized columns.
         (widths
          (cl-loop
           for col in display
           for width in widths
           for position from 0
           collect
           (if (or (plist-get col :width) (plist-get col :cell))
               width
             (let ((width (apply #'max width
                                 (mapcar (lambda (e)
                                           (string-width
                                            (or (nth (plist-get col :vuistack-table--index)
                                                     (plist-get (car e) :cells))
                                                "")))
                                         all))))
               (if (= position 0)
                   (+ width (apply #'max 0 (mapcar (lambda (p) (if p (string-width p) 0))
                                                   prefixes)))
                 width)))))
         (widths (vuistack-table--fit-group-headers display widths))
         ;; An auto-sized sorted column makes room for its ▲ / ▼ mark.
         (sorts (vuistack-table--sort-list (plist-get state :sort)))
         (widths (cl-mapcar
                  (lambda (col width)
                    (let ((rank (cl-position (plist-get col :vuistack-table--index) sorts
                                             :key #'car)))
                      (if (or (null rank) (plist-get col :width) (plist-get col :cell))
                          width
                        (max width (+ (string-width (or (plist-get col :header) ""))
                                      2 (if (cdr sorts)
                                            (length (number-to-string (1+ rank)))
                                          0))))))
                  display widths))
         (gutter-width (if selection 4 0))
         (first-index (plist-get (car display) :vuistack-table--index))
         (filter (plist-get state :filter))
         (filter-text
          (let ((parts (delq nil (cons (nth 2 filter)
                                       (mapcar (lambda (pair)
                                                 (format "%s: %s"
                                                         (or (plist-get (nth (car pair) columns)
                                                                        :header)
                                                             (car pair))
                                                         (cdr pair)))
                                               (plist-get state :column-filters))))))
            (and parts (string-join parts "; "))))
         (row-order (mapcar (lambda (e) (plist-get (car e) :id)) all))
         (table (list :columns columns :rows rows :options props :model model
                      :display display
                      :widths (cl-mapcar (lambda (col w)
                                           (cons (plist-get col :vuistack-table--index) w))
                                         display widths)
                      :row-order row-order)))
    (setf (alist-get id vuistack-table--tables nil nil #'equal) table)
    (when (or (plist-get props :sticky-header) (plist-get state :pin-left))
      (setq vuistack-table--sticky-p t
            vuistack-table--sticky-key nil))
    (let* ((cells (vuistack-table-selected-cells id))
           (tag (lambda (text &rest extra)
                  (apply #'propertize text 'vuistack-table-id id
                         'vuistack-table-columns columns extra)))
           (gutter (lambda (text zebra-index)
                     (let ((text (if (and first-index (not no-sort))
                                     (propertize text 'vuistack-table-column-index first-index)
                                   text)))
                       (when (and zebra zebra-index (cl-oddp zebra-index))
                         (setq text (copy-sequence text))
                         (add-face-text-property 0 (length text) 'vuistack-table-stripe t text))
                       text)))
           (anchors (list nil))
           (entry-line
            (lambda (entry prefix zebra-index spanning)
              (let* ((node (car entry))
                     (group (plist-get node :members))
                     (row (plist-get node :row))
                     (blank (cond (group (setcar anchors nil) nil)
                                  (spanning (vuistack-table--span-blanks display node anchors))))
                     (number (and zebra-index
                                  (+ 1 zebra-index (* (plist-get model :page)
                                                      (or (plist-get state :page-size) 0)))))
                     (line (vuistack-table-row-line
                            (if group
                                (vuistack-table--group-columns display node)
                              (vuistack-table--number-columns display number))
                            widths row zebra-index zebra no-sort
                            (unless group (plist-get props :action))
                            prefix blank))
                     (line (vuistack-table--mark-cells
                            line (cl-loop for (r . c) in cells
                                          when (equal r (plist-get node :id)) collect c)))
                     (detail (and (not group) (plist-get props :detail)
                                  (funcall can-expand node)
                                  (vuistack-table--expanded-p expanded node)
                                  (or (null (plist-get props :row-can-expand))
                                      (funcall (plist-get props :row-can-expand) row))
                                  (funcall (plist-get props :detail) row)))
                     (line (if detail
                               (concat line "\n"
                                       (replace-regexp-in-string
                                        "^" (make-string (* 2 (1+ (cdr entry))) ?\s)
                                        (format "%s" detail)))
                             line)))
                (funcall tag
                         (vuistack-table--with-gutter
                          (funcall gutter
                                   (if selection
                                       (vuistack-table--selection-mark node props selected-index)
                                     "")
                                   zebra-index)
                          line)
                         'vuistack-table-row row
                         'vuistack-table-node node
                         'vuistack-table-expandable (funcall can-expand node)))))
           (pad (make-string gutter-width ?\s))
           (total-width (+ gutter-width (apply #'+ (length widths) -1 widths)))
           (rule (funcall tag (propertize (make-string (max 1 total-width) ?─) 'face 'shadow)))
           (prefix-tail prefixes)
           (lines nil))
      (unless (plist-get props :no-header)
        (dolist (line (vuistack-table-header-group-lines display widths))
          (push (funcall tag (concat pad line)) lines))
        (push (funcall tag
                       (concat
                        (funcall gutter
                                 (if selection
                                     (let ((ids (cl-mapcan (lambda (n)
                                                             (vuistack-table--selection-ids
                                                              n props))
                                                           (plist-get model :filtered))))
                                       (cond ((and ids (cl-every (lambda (i)
                                                                  (gethash i selected-index))
                                                                 ids))
                                              "[x] ")
                                             ((cl-some (lambda (i)
                                                         (gethash i selected-index)) ids)
                                              "[-] ")
                                             (t "[ ] ")))
                                   "")
                                 nil)
                        (vuistack-table-header-line display widths (plist-get state :sort)
                                                    filter-text no-sort)))
              lines))
      (dolist (entry top)
        (push (funcall entry-line entry (pop prefix-tail) nil nil) lines))
      (when top (push rule lines))
      (if body
          (cl-loop for entry in body
                   for i from 0
                   do (push (funcall entry-line entry (pop prefix-tail) i
                                     (cl-some (lambda (c) (plist-get c :span-rows)) display))
                            lines))
        (push (funcall tag (propertize (or (plist-get props :empty) "(no rows)")
                                       'face 'shadow))
              lines))
      (when bottom (push rule lines))
      (dolist (entry bottom)
        (push (funcall entry-line entry (pop prefix-tail) nil nil) lines))
      (when-let ((footer (vuistack-table-footer-line display widths
                                                     (plist-get model :filtered-rows))))
        (push (funcall tag (concat pad footer)) lines))
      (let ((status
             (delq nil
                   (list
                    (and (plist-get state :page-size)
                         (format "Page %d of %d · %d rows"
                                 (1+ (plist-get model :page)) (plist-get model :page-count)
                                 (plist-get model :row-count)))
                    (and (plist-get state :grouping)
                         (format "grouped by %s"
                                 (mapconcat (lambda (i) (format "%s" (or (plist-get (nth i columns)
                                                                                  :header)
                                                                       i)))
                                            (plist-get state :grouping) " > ")))
                    (and selection selected (format "%d selected" (length selected)))
                    (and (plist-get state :hidden)
                         (format "%d column%s hidden" (length (plist-get state :hidden))
                                 (if (cdr (plist-get state :hidden)) "s" "")))
                    (and (plist-get state :scroll) (> (plist-get state :scroll) 0)
                         (format "scrolled %d column%s" (plist-get state :scroll)
                                 (if (> (plist-get state :scroll) 1) "s" "")))))))
        (when status
          (push (funcall tag (propertize (string-join status " · ") 'face 'shadow)) lines)))
      (nreverse lines))))

;; -- the component --

(vui-defcomponent vuistack-table (id columns rows zebra empty no-sort no-header action
                                     initial-state on-state-change row-id get-sub-rows
                                     detail row-can-expand
                                     enable-row-selection single-select
                                     disable-sub-row-selection
                                     enable-row-pinning disable-row-pinning
                                     drop-filtered-pinned-rows
                                     page-size no-paginate-expanded-rows row-count page-count
                                     manual-sorting manual-filtering manual-grouping
                                     manual-pagination manual-expanding
                                     filter-from-leaf-rows max-leaf-row-filter-depth
                                     global-filter-fn grouped-column-mode
                                     max-multi-sort-count disable-multi-sort
                                     disable-sort-removal auto-reset sticky-header)
  "A TanStack-Table-style table.  See the file commentary and README.
ID (default `default') names the table for its state; give each table
in one buffer its own ID.  EMPTY is the text shown for no rows.  NO-SORT
leaves out the column marks every column command reads.  NO-HEADER
drops the heading lines.  ACTION (ROW -> any) is what
`vuistack-table-activate' on a cell calls when its column has no
:action of its own.  The other props are TanStack table options; the
README lists them."
  :render
  (let* ((id (or id 'default))
         (columns (vuistack-table--leaf-columns columns))
         (props (list :zebra zebra :empty empty :no-sort no-sort :no-header no-header
                      :action action :on-state-change on-state-change :row-id row-id
                      :get-sub-rows get-sub-rows :detail detail
                      :row-can-expand row-can-expand
                      :enable-row-selection enable-row-selection
                      :single-select single-select
                      :disable-sub-row-selection disable-sub-row-selection
                      :enable-row-pinning enable-row-pinning
                      :disable-row-pinning disable-row-pinning
                      :drop-filtered-pinned-rows drop-filtered-pinned-rows
                      :no-paginate-expanded-rows no-paginate-expanded-rows
                      :row-count row-count :page-count page-count
                      :manual-sorting manual-sorting :manual-filtering manual-filtering
                      :manual-grouping manual-grouping
                      :manual-pagination manual-pagination
                      :manual-expanding manual-expanding
                      :filter-from-leaf-rows filter-from-leaf-rows
                      :max-leaf-row-filter-depth max-leaf-row-filter-depth
                      :global-filter-fn global-filter-fn
                      :grouped-column-mode grouped-column-mode
                      :max-multi-sort-count max-multi-sort-count
                      :disable-multi-sort disable-multi-sort
                      :disable-sort-removal disable-sort-removal
                      :auto-reset auto-reset :sticky-header sticky-header)))
    (vuistack-table--seed id columns initial-state page-size)
    (if (and (null rows) (null (vuistack-table--table-filter id))
             (null (vuistack-table--get id :column-filters)))
        (vui-text (or empty "(no rows)") :face 'shadow)
      ;; One text vnode keeps large tables cheap to reconcile; each line
      ;; retains its row/column text properties for the commands.
      (vui-text (mapconcat #'identity
                           (vuistack-table--render-lines id columns rows props)
                           "\n")))))

(provide 'vuistack-table)
;;; vuistack-table.el ends here
