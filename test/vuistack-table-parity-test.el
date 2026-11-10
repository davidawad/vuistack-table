;;; vuistack-table-parity-test.el --- TanStack parity tests for vuistack-table -*- lexical-binding: t; -*-

;; Run: make test
;;
;; Renders tables and drives the `vuistack-table-mode' commands the way a
;; user does, one TanStack feature at a time, and holds
;; TANSTACK-PARITY.md to having no open `missing' item.

;;; Code:

(require 'ert)
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))
(require 'vuistack-table)

(defconst vuistack-table-parity-test--dir
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst vuistack-table-parity-test--cols
  '((:header "Team" :key :team)
    (:header "Name" :key :name)
    (:header "Pts" :key :pts :aggregation sum :footer aggregate)))

(defconst vuistack-table-parity-test--rows
  '((:team "red" :name "ann" :pts 3)
    (:team "blue" :name "bob" :pts 5)
    (:team "red" :name "cy" :pts 1)))

(defmacro vuistack-table-parity-test--with (props &rest body)
  "Render a `vuistack-table' with PROPS in a temp buffer, then run BODY.
`vui-refresh' re-renders it, so commands repaint as they do live."
  (declare (indent 1))
  `(with-temp-buffer
     (let* ((props ,props)
            (render (lambda ()
                      (let ((inhibit-read-only t)) (erase-buffer))
                      (vui-render (apply #'vui-component 'vuistack-table props))
                      (goto-char (point-min)))))
       (cl-letf (((symbol-function 'vui-refresh) render)
                 ((symbol-function 'message) #'ignore))
         (funcall render)
         ,@body))))

(defun vuistack-table-parity-test--text ()
  "The buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun vuistack-table-parity-test--goto (text &optional column)
  "Put point on the first line containing TEXT, at COLUMN (default TEXT)."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0))
  (when column (move-to-column column)))

(defun vuistack-table-parity-test--lines ()
  "The buffer's lines, trailing spaces trimmed."
  (mapcar #'string-trim-right (split-string (vuistack-table-parity-test--text) "\n")))

;; -- sorting --

(ert-deftest vuistack-table-parity-sort-cycles-and-multi-sorts ()
  (vuistack-table-parity-test--with
      (list :id 's :columns vuistack-table-parity-test--cols
            :rows vuistack-table-parity-test--rows)
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-sort)
    (should (equal '(2) (vuistack-table--table-sort 's)))
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-sort)
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-sort)
    (should-not (vuistack-table--table-sort 's))
    (vuistack-table-parity-test--goto "Team")
    (vuistack-table-sort)
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-sort '(4))
    (should (equal '((0) (2)) (vuistack-table--table-sort 's)))
    (should (string-match-p "Team ▲1.*Pts ▲2" (vuistack-table-parity-test--text)))
    (should (equal '("blue" "red" "red")
                   (mapcar (lambda (l) (car (split-string l)))
                           (cl-subseq (vuistack-table-parity-test--lines) 1 4))))))

(ert-deftest vuistack-table-parity-sort-desc-first-and-disable ()
  (vuistack-table-parity-test--with
      (list :id 'd :rows vuistack-table-parity-test--rows
            :columns '((:header "Name" :key :name :disable-sort t)
                       (:header "Pts" :key :pts :sort-desc-first t)))
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-sort)
    (should (equal '(1 . t) (vuistack-table--table-sort 'd)))
    (vuistack-table-parity-test--goto "Name")
    (should-error (vuistack-table-sort) :type 'user-error)))

;; -- filtering and faceting --

(ert-deftest vuistack-table-parity-column-filters-stack-with-global ()
  (vuistack-table-parity-test--with
      (list :id 'f :columns vuistack-table-parity-test--cols
            :rows vuistack-table-parity-test--rows)
    (vuistack-table-parity-test--goto "Team")
    (vuistack-table-filter-column "=red")
    (vuistack-table-parity-test--goto "Pts")
    (vuistack-table-filter-column ">2")
    (should (string-match-p "\\[filter: Team: =red; Pts: >2\\]"
                            (vuistack-table-parity-test--text)))
    (should (string-match-p "ann" (vuistack-table-parity-test--text)))
    (should-not (string-match-p "cy\\|bob" (vuistack-table-parity-test--text)))
    (should (equal '(("red" . 1) ("blue" . 1))
                   (vuistack-table-faceted-unique-values
                    (car vuistack-table-parity-test--cols)
                    (vuistack-table--faceted-rows 'f vuistack-table-parity-test--cols 0))))
    (vuistack-table-filter-reset)
    (should (string-match-p "bob" (vuistack-table-parity-test--text)))))

;; -- grouping, aggregation, footers --

(ert-deftest vuistack-table-parity-grouping-aggregates-and-expands ()
  (vuistack-table-parity-test--with
      (list :id 'g :columns vuistack-table-parity-test--cols
            :rows vuistack-table-parity-test--rows)
    (vuistack-table-parity-test--goto "Team")
    (vuistack-table-group-by-column)
    (let ((lines (vuistack-table-parity-test--lines)))
      (should (string-match-p "\\`▸ red (2) +4\\'" (nth 1 lines)))
      (should (string-match-p "\\`▸ blue (1) +5\\'" (nth 2 lines)))
      (should (string-match-p "9\\'" (nth 3 lines)))
      (should (equal "grouped by Team" (nth 4 lines))))
    (vuistack-table-parity-test--goto "red (2)")
    (vuistack-table-toggle-expand)
    (should (string-match-p "▾ red (2).*\n +red +ann +3 *\n +red +cy +1"
                            (vuistack-table-parity-test--text)))
    (vuistack-table-toggle-expand-all)
    (should (string-match-p "▾ blue" (vuistack-table-parity-test--text)))))

;; -- expanding sub-rows and detail panels --

(ert-deftest vuistack-table-parity-sub-rows-and-detail-panels ()
  (vuistack-table-parity-test--with
      (list :id 'e :columns '((:header "Name" :key :name))
            :rows '((:name "parent" :sub-rows ((:name "child"))) (:name "solo"))
            :detail (lambda (_row) "more")
            :row-can-expand (lambda (row) (equal (plist-get row :name) "solo")))
    (should-not (string-match-p "child" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "parent")
    (vuistack-table-toggle-expand)
    (should (string-match-p "▾ parent *\n    child" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "solo")
    (vuistack-table-toggle-expand)
    (should (string-match-p "▾ solo *\n  more" (vuistack-table-parity-test--text)))))

;; -- pagination --

(ert-deftest vuistack-table-parity-pagination-pages-and-resets ()
  (vuistack-table-parity-test--with
      (list :id 'p :columns '((:header "N" :key :n) (:header "#" :row-number t))
            :rows (mapcar (lambda (i) (list :n (format "r%d" i))) (number-sequence 1 5))
            :page-size 2)
    (should (string-match-p "Page 1 of 3 · 5 rows" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "r1")
    (vuistack-table-next-page)
    (should (string-match-p "r3 +3\nr4 +4" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "r3")
    (vuistack-table-next-page t)
    (should (string-match-p "Page 3 of 3" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "r5")
    (vuistack-table-filter "r")
    (should (string-match-p "Page 1 of 3" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "r1")
    (vuistack-table-set-page-size 0)
    (should-not (string-match-p "Page" (vuistack-table-parity-test--text)))))

;; -- row selection and pinning --

(ert-deftest vuistack-table-parity-row-selection ()
  (vuistack-table-parity-test--with
      (list :id 'r :columns vuistack-table-parity-test--cols
            :rows vuistack-table-parity-test--rows :enable-row-selection t)
    (vuistack-table-parity-test--goto "bob")
    (vuistack-table-toggle-select)
    (should (string-match-p "\\[-\\] Team" (vuistack-table-parity-test--text)))
    (should (string-match-p "\\[x\\] blue" (vuistack-table-parity-test--text)))
    (should (equal '((:team "blue" :name "bob" :pts 5)) (vuistack-table-selected-rows 'r)))
    (vuistack-table-parity-test--goto "cy")
    (vuistack-table-toggle-select '(4))
    (should (= 2 (length (vuistack-table-selected-rows 'r))))
    (vuistack-table-toggle-select-all)
    (should (string-match-p "\\[x\\] Team" (vuistack-table-parity-test--text)))
    (should (string-match-p "3 selected" (vuistack-table-parity-test--text)))
    (vuistack-table-clear-selection)
    (should-not (vuistack-table-selected-rows 'r))))

(ert-deftest vuistack-table-parity-row-pinning ()
  (vuistack-table-parity-test--with
      (list :id 'pin :columns vuistack-table-parity-test--cols
            :rows vuistack-table-parity-test--rows)
    (vuistack-table-parity-test--goto "cy")
    (vuistack-table-pin-row-top)
    (let ((lines (vuistack-table-parity-test--lines)))
      (should (string-match-p "\\`red +cy" (nth 1 lines)))
      (should (string-match-p "\\`─+\\'" (nth 2 lines))))
    (vuistack-table-parity-test--goto "Team")
    (vuistack-table-filter "bob")
    (should (string-match-p "cy" (vuistack-table-parity-test--text)))
    (vuistack-table-parity-test--goto "cy")
    (vuistack-table-pin-row-bottom)
    (should (string-match-p "─+\nred +cy" (vuistack-table-parity-test--text)))))

;; -- columns: visibility, ordering, pinning, sizing --

(ert-deftest vuistack-table-parity-column-commands ()
  (vuistack-table-parity-test--with
      (list :id 'c :columns '((:header "A" :key :a) (:header "B" :key :b)
                              (:header "C" :key :c :max-width 3))
            :rows '((:a "1" :b "2" :c "3")))
    (vuistack-table-parity-test--goto "B")
    (vuistack-table-hide-column)
    (should (equal "A C" (car (vuistack-table-parity-test--lines))))
    (vuistack-table-toggle-column "B")
    (vuistack-table-parity-test--goto "C")
    (vuistack-table-move-column-left)
    (should (equal "A C B" (car (vuistack-table-parity-test--lines))))
    (vuistack-table-parity-test--goto "B")
    (vuistack-table-pin-column)
    (should (equal "B A C" (car (vuistack-table-parity-test--lines))))
    (vuistack-table-scroll-columns-right)
    (should (equal "B C" (car (vuistack-table-parity-test--lines))))
    (vuistack-table-scroll-columns-left)
    (vuistack-table-parity-test--goto "C")
    (vuistack-table-widen-column 5)
    (should (equal "B A C" (car (vuistack-table-parity-test--lines))))
    (should (= 3 (alist-get 2 (vuistack-table--get 'c :widths))))
    (vuistack-table-reset-table)
    (should (equal "A B C" (car (vuistack-table-parity-test--lines))))))

;; -- header groups, footers, spanning --

(ert-deftest vuistack-table-parity-header-groups-footers-and-spans ()
  (vuistack-table-parity-test--with
      (list :id 'h
            :columns '((:header "Where" :columns ((:header "Team" :key :team :span-rows t)
                                                  (:header "Name" :key :name)))
                       (:header "Pts" :key :pts :footer aggregate :aggregation sum
                                :span-columns 1))
            :rows '((:team "red" :name "ann" :pts 3) (:team "red" :name "cy" :pts 1)))
    (should (equal '("Where" "Team Name Pts" "red  ann  3" "     cy   1" "          4")
                   (vuistack-table-parity-test--lines))))
  (vuistack-table-parity-test--with
      (list :id 'span
            :columns (list (list :header "Label" :key :l
                                 :span-columns (lambda (row) (if (plist-get row :sum) 9 1)))
                           '(:header "X" :key :x))
            :rows '((:l "a" :x "1") (:l "everything" :sum t)))
    (should (equal "everything" (nth 2 (vuistack-table-parity-test--lines))))))

;; -- cell selection, editing, state --

(ert-deftest vuistack-table-parity-cell-selection-copies-tsv ()
  (vuistack-table-parity-test--with
      (list :id 'cell :columns '((:header "A" :key :a) (:header "B" :key :b))
            :rows '((:a "x" :b "=1") (:a "y\tz" :b "w")))
    (vuistack-table-parity-test--goto "x")
    (vuistack-table-cell-select)
    (vuistack-table-parity-test--goto "w")
    (vuistack-table-cell-extend)
    (should (= 4 (length (vuistack-table-selected-cells 'cell))))
    (vuistack-table-parity-test--goto "x")
    (should (memq 'vuistack-table-cell-selected
                  (ensure-list (get-text-property (point) 'face))))
    (let ((kill-ring nil))
      (vuistack-table-copy-cells)
      (should (equal "x\t'=1\n\"y\tz\"\tw" (car kill-ring))))
    (vuistack-table-parity-test--goto "w")
    (vuistack-table-cell-select '(16))
    (should (= 3 (length (vuistack-table-selected-cells 'cell))))))

(ert-deftest vuistack-table-parity-edit-initial-state-and-on-state-change ()
  (let* ((changes nil)
         (rows (list (list :name "ann" :pts 3))))
    (vuistack-table-parity-test--with
        (list :id 'st :rows rows
              :columns (list (list :header "Name" :key :name
                                   :on-edit (lambda (row text) (plist-put row :name text)))
                             '(:header "Pts" :key :pts))
              :initial-state '(:hidden (:pts) :sort ("Name" . t))
              :on-state-change (lambda (id state) (push (cons id state) changes)))
      (should (equal "Name ▼" (car (vuistack-table-parity-test--lines))))
      (vuistack-table-parity-test--goto "ann")
      (vuistack-table-edit-cell "zed")
      (should (string-match-p "zed" (vuistack-table-parity-test--text)))
      (vuistack-table-show-all-columns)
      (should (equal '(1) (plist-get (cdr (cadr changes)) :hidden)))
      (should (eq 'st (caar changes)))
      (should (equal "Name ▼ Pts" (car (vuistack-table-parity-test--lines)))))))

;; -- the checklist --

(ert-deftest vuistack-table-parity-checklist-has-no-missing-item ()
  "Every TANSTACK-PARITY.md row is have or not applicable, with a docs link."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "TANSTACK-PARITY.md"
                                            vuistack-table-parity-test--dir))
    (let ((rows 0))
      (goto-char (point-min))
      (while (re-search-forward "^| \\(.+\\) | \\(.+\\) | \\(.+?\\) | .+ |$" nil t)
        (let ((feature (match-string 1))
              (source (match-string 2))
              (status (match-string 3)))
          (unless (member feature '("Feature" "---"))
            (cl-incf rows)
            (should (member status '("have" "not applicable")))
            (should (string-match-p "https://tanstack\\.com/table/" source)))))
      (should (> rows 40)))))

(provide 'vuistack-table-parity-test)
;;; vuistack-table-parity-test.el ends here
