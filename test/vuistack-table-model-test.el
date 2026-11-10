;;; vuistack-table-model-test.el --- Tests for vuistack-table's row and column models -*- lexical-binding: t; -*-

;; Run: make test

;;; Code:

(require 'ert)
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))
(require 'vuistack-table)

(defconst vuistack-table-model-test--cols
  '((:header "Name" :key :name)
    (:header "Amount" :key :amount)
    (:header "Tags" :key :tags)
    (:header "Date" :key :date)))

(defconst vuistack-table-model-test--rows
  '((:name "alpha" :amount 10 :tags ("x" "y") :date "2026-01-05")
    (:name "Beta" :amount 25.5 :tags ("y") :date "2026-02-10")
    (:name "gamma" :amount nil :tags nil :date "2026-03-15")))

(defun vuistack-table-model-test--names (rows)
  "The :name of each of ROWS."
  (mapcar (lambda (row) (plist-get row :name)) rows))

(defun vuistack-table-model-test--matching (index query)
  "Names of the test rows whose column INDEX passes QUERY."
  (vuistack-table-model-test--names
   (cl-remove-if-not
    (lambda (row)
      (vuistack-table--column-matches-p (nth index vuistack-table-model-test--cols)
                                        row query))
    vuistack-table-model-test--rows)))

;; -- filtering --

(ert-deftest vuistack-table-filter-language-picks-filter-functions ()
  (should (equal '(includes-string . "ab") (vuistack-table--parse-query "ab")))
  (should (equal '(equals-string . "ab") (vuistack-table--parse-query "=ab")))
  (should (equal '(equals-string-sensitive . "ab") (vuistack-table--parse-query "==ab")))
  (should (equal '(fuzzy . "ab") (vuistack-table--parse-query "~ab")))
  (should (equal '(starts-with . "ab") (vuistack-table--parse-query "^ab")))
  (should (equal '(ends-with . "ab") (vuistack-table--parse-query "ab$")))
  (should (equal '(in-number-range . "1..5") (vuistack-table--parse-query "1..5")))
  (should (equal '(in-number-range . "3..") (vuistack-table--parse-query ">=3")))
  (should (equal '(in-date-range . "2026-01-01..2026-02-01")
                 (vuistack-table--parse-query "2026-01-01..2026-02-01")))
  (should (equal '(empty . "") (vuistack-table--parse-query "=")))
  (should (equal '(not-empty . "") (vuistack-table--parse-query "!=")))
  (should (equal '(arr-includes-all . "x,y") (vuistack-table--parse-query "all:x,y"))))

(ert-deftest vuistack-table-filter-functions-match-like-tanstack ()
  (should (equal '("alpha") (vuistack-table-model-test--matching 0 "AL")))
  (should (equal '("Beta") (vuistack-table-model-test--matching 0 "=beta")))
  (should-not (vuistack-table-model-test--matching 0 "==beta"))
  (should (equal '("gamma") (vuistack-table-model-test--matching 0 "^gam")))
  (should (equal '("Beta") (vuistack-table-model-test--matching 0 "TA$")))
  (should (equal '("Beta") (vuistack-table-model-test--matching 1 ">10")))
  (should (equal '("alpha" "Beta") (vuistack-table-model-test--matching 1 ">=10")))
  (should (equal '("alpha") (vuistack-table-model-test--matching 1 "..20")))
  (should (equal '("gamma") (vuistack-table-model-test--matching 1 "=")))
  (should (equal '("alpha" "Beta") (vuistack-table-model-test--matching 1 "!=")))
  (should (equal '("alpha") (vuistack-table-model-test--matching 2 "all:x,y")))
  (should (equal '("alpha" "Beta") (vuistack-table-model-test--matching 2 "any:y,z")))
  (should (equal '("Beta") (vuistack-table-model-test--matching
                            3 "2026-02-01..2026-02-28")))
  (should (equal '("alpha" "gamma") (vuistack-table-model-test--matching 0 "~aa"))))

(ert-deftest vuistack-table-column-filter-fn-overrides-the-language ()
  (let ((named '(:key :name :filter-fn includes-string-sensitive))
        (custom (list :key :amount
                      :filter-fn (lambda (value query _row)
                                   (and value (= value (string-to-number query)))))))
    (should (vuistack-table--column-matches-p named '(:name "Beta") "Be"))
    (should-not (vuistack-table--column-matches-p named '(:name "Beta") "be"))
    (should (vuistack-table--column-matches-p custom '(:amount 10) "10"))
    (should-not (vuistack-table--column-matches-p custom '(:amount 11) "10"))))

(ert-deftest vuistack-table-global-and-column-filters-stack ()
  (let ((cols (append vuistack-table-model-test--cols
                      '((:header "Secret" :key :secret :disable-global-filter t)))))
    (should (vuistack-table--row-passes-p cols '(:name "alpha" :amount 3)
                                          '(nil "alp" "alp") '((1 . "<5"))))
    (should-not (vuistack-table--row-passes-p cols '(:name "alpha" :amount 9)
                                              '(nil "alp" "alp") '((1 . "<5"))))
    (should-not (vuistack-table--row-passes-p cols '(:name "a" :secret "zzz")
                                              '(nil "zzz" "zzz") nil))
    (should (vuistack-table--row-passes-p cols '(:name "a") '(nil "q" "q") nil
                                          (lambda (_row query) (equal query "q"))))))

(ert-deftest vuistack-table-fuzzy-score-ranks-substrings-first ()
  (should-not (vuistack-table-fuzzy-score "zz" "alpha"))
  (should (> (vuistack-table-fuzzy-score "alp" "alpha")
             (vuistack-table-fuzzy-score "aph" "alpha")))
  (let* ((state '(:filter (nil "~ab" "~ab")))
         (model (vuistack-table-row-model
                 '((:header "N" :key :n))
                 '((:n "a-x-b") (:n "zab") (:n "none"))
                 state)))
    (should (equal '("zab" "a-x-b")
                   (mapcar (lambda (e) (plist-get (plist-get (car e) :row) :n))
                           (plist-get model :entries))))))

;; -- faceting --

(ert-deftest vuistack-table-faceting-counts-values-and-bounds ()
  (let ((tags (nth 2 vuistack-table-model-test--cols))
        (amount (nth 1 vuistack-table-model-test--cols)))
    (should (equal '(("y" . 2) ("x" . 1) (nil . 1))
                   (vuistack-table-faceted-unique-values
                    tags vuistack-table-model-test--rows)))
    (should (equal '(10 . 25.5)
                   (vuistack-table-faceted-min-max amount vuistack-table-model-test--rows)))
    (should (equal 2 (cdr (assoc "y" (vuistack-table-global-faceted-unique-values
                                       vuistack-table-model-test--cols
                                       vuistack-table-model-test--rows)))))))

;; -- sorting --

(ert-deftest vuistack-table-sort-functions-order-values ()
  (let ((natural '(:key :v :sort-fn alphanumeric))
        (text '(:key :v :sort-fn text))
        (dates '(:key :v :sort-fn datetime)))
    (should (= -1 (vuistack-table--compare-values natural "a2" "a10")))
    (should (= 1 (vuistack-table--compare-values '(:key :v) "a2" "a10")))
    (should (= 0 (vuistack-table--compare-values text "ABC" "abc")))
    (should (= -1 (vuistack-table--compare-values dates "2026-01-02" "2026-01-10")))
    (should (= 1 (vuistack-table--compare-values
                  (list :key :v :sort-fn (lambda (a b) (- (length a) (length b))))
                  "ccc" "a")))))

(ert-deftest vuistack-table-multi-sort-undefined-and-invert ()
  (let ((cols '((:header "G" :key :g) (:header "N" :key :n :sort-undefined last)))
        (rows '((:g "b" :n 1) (:g "a" :n nil) (:g "a" :n 2) (:g "a" :n 1))))
    (should (equal '((:g "a" :n 2) (:g "a" :n 1) (:g "a" :n nil) (:g "b" :n 1))
                   (vuistack-table--sorted-rows cols rows '((0) (1 . t)))))
    (should (equal '(3 2 1)
                   (mapcar (lambda (r) (plist-get r :n))
                           (vuistack-table--sorted-rows
                            '((:key :n :invert-sorting t))
                            '((:n 1) (:n 3) (:n 2)) '(0)))))))

;; -- aggregation --

(ert-deftest vuistack-table-aggregations-match-tanstack-built-ins ()
  (let ((values '(4 1 3 2)))
    (should (= 10 (vuistack-table-aggregate 'sum values)))
    (should (= 4 (vuistack-table-aggregate 'count values)))
    (should (= 1 (vuistack-table-aggregate 'min values)))
    (should (= 4 (vuistack-table-aggregate 'max values)))
    (should (equal '(1 . 4) (vuistack-table-aggregate 'extent values)))
    (should (= 2.5 (vuistack-table-aggregate 'mean values)))
    (should (= 2.5 (vuistack-table-aggregate 'median values)))
    (should (= 3 (vuistack-table-aggregate 'median '(3 1 5))))
    (should (equal '(a b) (vuistack-table-aggregate 'unique '(a b a))))
    (should (= 2 (vuistack-table-aggregate 'unique-count '(a b a))))
    (should (= 4 (vuistack-table-aggregate 'first values)))
    (should (= 2 (vuistack-table-aggregate 'last values)))
    (should (= 10 (vuistack-table-aggregate 'auto values)))
    (should-not (vuistack-table-aggregate 'auto '("x")))
    (should (= 2 (vuistack-table-aggregate (lambda (vs _rows) (length vs)) '(1 2))))
    (should (equal '((sum . 10) (count . 4))
                   (vuistack-table-aggregate '(sum count) values)))))

(ert-deftest vuistack-table-aggregate-strings-and-depth ()
  (let ((money '(:key :v :money t)))
    (should (equal "1.00 – 4.00" (vuistack-table-aggregate-string money '(1 . 4))))
    (should (equal "sum 3.00; count 2"
                   (vuistack-table-aggregate-string '(:key :v) '((sum . "3.00") (count . 2)))))
    (should (equal "" (vuistack-table-aggregate-string money nil))))
  (let ((rows '((:v 100 :sub-rows ((:v 1) (:v 2))) (:v 5))))
    (should (= 105 (vuistack-table-aggregate-column '(:key :v :aggregation sum) rows)))
    (should (= 8 (vuistack-table-aggregate-column '(:key :v :aggregation sum) rows 1)))))

;; -- columns --

(ert-deftest vuistack-table-display-columns-hide-order-pin-group-scroll ()
  (let* ((cols '((:header "A") (:header "B") (:header "C") (:header "D")))
         (indices (lambda (state &optional mode)
                    (mapcar (lambda (c) (plist-get c :vuistack-table--index))
                            (vuistack-table--display-columns cols state mode)))))
    (should (equal '(0 1 2 3) (funcall indices nil)))
    (should (equal '(0 2 3) (funcall indices '(:hidden (1)))))
    (should (equal '(3 2 1 0) (funcall indices '(:order (3 2 1 0)))))
    (should (equal '(2 0 1 3) (funcall indices '(:pin-left (2)))))
    (should (equal '(1 2 3 0) (funcall indices '(:pin-right (0)))))
    (should (equal '(2 0 1 3) (funcall indices '(:grouping (2)))))
    (should (equal '(0 1 3) (funcall indices '(:grouping (2)) 'remove)))
    (should (equal '(0 2 3) (funcall indices '(:pin-left (0) :scroll 1))))
    (should (eq 'left (plist-get (car (vuistack-table--display-columns
                                       cols '(:pin-left (2))))
                                 :vuistack-table--pin)))
    (should (= 9 (plist-get (car (vuistack-table--display-columns cols '(:widths ((0 . 9)))))
                            :width)))))

(ert-deftest vuistack-table-group-columns-flatten-and-deep-keys ()
  (let* ((group '(:header "G" :columns ((:header "A" :key :a) (:header "B" :key :b))))
         (leaves (vuistack-table--leaf-columns (list group '(:header "C" :key :c)))))
    (should (equal '("A" "B" "C") (mapcar (lambda (c) (plist-get c :header)) leaves)))
    (should (eq group (car (plist-get (car leaves) :vuistack-table--path))))
    (should (equal 2 (vuistack-table--resolve-column leaves :c)))
    (should (equal 1 (vuistack-table--resolve-column leaves "b"))))
  (should (equal "Oslo" (vuistack-table--cell-value '(:path (:address :city))
                                                    '(:address (:city "Oslo")))))
  (should (equal "y" (vuistack-table--cell-value '(:path (:tags 1)) '(:tags ["x" "y"])))))

;; -- the row model --

(ert-deftest vuistack-table-row-model-groups-sorts-and-paginates ()
  (let* ((cols '((:header "Team" :key :team)
                 (:header "Pts" :key :pts :aggregation sum)))
         (rows '((:team "b" :pts 1) (:team "a" :pts 5) (:team "b" :pts 9)))
         (model (vuistack-table-row-model cols rows '(:grouping (0) :sort ((1 . t)))))
         (groups (mapcar #'car (plist-get model :entries))))
    (should (equal '("b" "a") (mapcar (lambda (g) (plist-get g :key)) groups)))
    (should (equal '("b (2)" "10") (plist-get (car groups) :cells)))
    (let ((open (vuistack-table-row-model cols rows
                                          (list :grouping '(0) :expanded t :page-size 2))))
      (should (= 5 (plist-get open :row-count)))
      (should (= 3 (plist-get open :page-count)))
      (should (= 2 (length (plist-get open :entries)))))))

(ert-deftest vuistack-table-row-model-filters-trees-from-root-or-leaves ()
  (let* ((cols '((:header "N" :key :n)))
         (rows '((:n "parent" :sub-rows ((:n "kid match") (:n "other")))))
         (state '(:filter (nil "match" "match") :expanded t))
         (ids (lambda (model) (mapcar (lambda (e) (plist-get (car e) :id))
                                      (plist-get model :entries)))))
    (should-not (funcall ids (vuistack-table-row-model cols rows state)))
    (should (equal '("0" "0.0")
                   (funcall ids (vuistack-table-row-model
                                 cols rows state '(:filter-from-leaf-rows t)))))
    (should (equal '("0" "0.0" "0.1")
                   (funcall ids (vuistack-table-row-model
                                 cols rows '(:expanded t)))))))

(ert-deftest vuistack-table-row-model-pins-rows-and-honours-manual-modes ()
  (let* ((cols '((:header "N" :key :n)))
         (rows '((:n 3) (:n 1) (:n 2)))
         (pinned (vuistack-table-row-model cols rows
                                           '(:pin-top ("2") :filter (nil "3" "3"))))
         (manual (vuistack-table-row-model cols rows '(:sort (0) :page-size 1)
                                           '(:manual-sorting t :manual-pagination t
                                             :row-count 30))))
    (should (equal '((:n 2)) (mapcar (lambda (e) (plist-get (car e) :row))
                                     (plist-get pinned :top))))
    (should (equal '((:n 3)) (mapcar (lambda (e) (plist-get (car e) :row))
                                     (plist-get pinned :entries))))
    (should (equal rows (mapcar (lambda (e) (plist-get (car e) :row))
                                (plist-get manual :entries))))
    (should (= 30 (plist-get manual :page-count)))))

(provide 'vuistack-table-model-test)
;;; vuistack-table-model-test.el ends here
