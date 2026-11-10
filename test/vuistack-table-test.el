;;; vuistack-table-test.el --- Tests for vuistack-table -*- lexical-binding: t; -*-

;; Run: make test  (or: emacs -Q --batch -L . -L <vui> -l test/vuistack-table-test.el
;;                  -f ert-run-tests-batch-and-exit)

;;; Code:

(require 'ert)
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))
(require 'vuistack-table)

(defconst vuistack-table-test--cols
  '((:header "Name" :width 8 :key :name)
    (:header "State" :width 7 :key :state)))

(defun vuistack-table-test--faces (pos text)
  "The faces at POS in TEXT, as a list."
  (ensure-list (get-text-property pos 'face text)))

(defun vuistack-table-test--render (&rest props)
  "Render a `vuistack-table' with PROPS into the current buffer; return its text."
  (vui-render (apply #'vui-component 'vuistack-table props))
  (buffer-string))

;; -- cells --

(ert-deftest vuistack-table-string-renders-nil-t-and-values ()
  (should (equal "—" (vuistack-table--string nil)))
  (should (equal "yes" (vuistack-table--string t)))
  (should (equal "42" (vuistack-table--string 42))))

(ert-deftest vuistack-table-money-string-groups-and-rounds ()
  (should (equal "1,234,567.89" (vuistack-table-money-string 1234567.891)))
  (should (equal "-1,234.50" (vuistack-table-money-string -1234.5)))
  (should (equal "0.01" (vuistack-table-money-string 0.005)))
  (should (equal "0.00" (vuistack-table-money-string -0.001)))
  (should (equal "1,200.00" (vuistack-table-money-string "$1,200")))
  (should (equal "—" (vuistack-table-money-string nil))))

(ert-deftest vuistack-table-auto-width-is-the-widest-cell-capped ()
  (let ((vuistack-table-max-auto-width 6))
    (should (equal '(5 6)
                   (vuistack-table--col-widths
                    '((:header "Name" :key :name) (:header "Note" :key :note))
                    '((:name "alpha" :note "much too long") (:name "b")))))))

;; -- rows --

(ert-deftest vuistack-table-row-line-pads-truncates-and-tags-row ()
  (let* ((row '(:name "abcdefghijkl" :state nil))
         (line (vuistack-table-row-line vuistack-table-test--cols '(8 7) row 0 nil)))
    (should (equal "abcdefg… —      " (substring-no-properties line)))
    (should (eq row (get-text-property 0 'vuistack-table-row line)))
    (should (eql 1 (get-text-property 9 'vuistack-table-column-index line)))))

(ert-deftest vuistack-table-zebra-stripes-odd-rows-only ()
  (let ((even (vuistack-table-row-line vuistack-table-test--cols '(8 7)
                                       '(:name "a") 0 t))
        (odd (vuistack-table-row-line vuistack-table-test--cols '(8 7)
                                      '(:name "b") 1 t))
        (off (vuistack-table-row-line vuistack-table-test--cols '(8 7)
                                      '(:name "b") 1 nil)))
    (should-not (memq 'vuistack-table-stripe (vuistack-table-test--faces 0 even)))
    (should (memq 'vuistack-table-stripe (vuistack-table-test--faces 0 odd)))
    (should-not (memq 'vuistack-table-stripe (vuistack-table-test--faces 0 off)))))

(ert-deftest vuistack-table-preserves-explicit-row-faces-with-zebra ()
  (let* ((row '(:name "limit" :face error))
         (line (vuistack-table-row-line vuistack-table-test--cols '(8 7) row 1 t))
         (faces (vuistack-table-test--faces 0 line)))
    (should (memq 'error faces))
    (should (memq 'vuistack-table-stripe faces))
    (should (eq row (get-text-property 0 'vuistack-table-row line)))))

(ert-deftest vuistack-table-money-cells-align-right-and-sign-color ()
  (let* ((cols '((:header "Amount" :key :amount :money t :sign-color t)))
         (pos (vuistack-table-row-line cols '(10) '(:amount 1234.5) 0 nil))
         (neg (vuistack-table-row-line cols '(10) '(:amount -3) 0 nil))
         (total (vuistack-table-row-line cols '(10) '(:amount 0 :total t) 0 nil)))
    (should (equal "  1,234.50" (substring-no-properties pos)))
    (should (memq 'vuistack-table-positive (vuistack-table-test--faces 9 pos)))
    (should (memq 'vuistack-table-negative (vuistack-table-test--faces 9 neg)))
    (should (memq 'bold (vuistack-table-test--faces 0 total)))))

(ert-deftest vuistack-table-cell-renderers-span-lines-and-stay-aligned ()
  (let* ((cols (list '(:header "name" :width 6 :key :name)
                     (list :header "chart" :width 4
                           :cell (lambda (row width)
                                   (format "%s\n%s" (make-string width ?#)
                                           (make-string (plist-get row :n) ?=))))
                     '(:header "after" :width 5 :key :after)))
         (text (vuistack-table-row-line cols '(6 4 5) '(:name "a" :n 2 :after "x")
                                        1 t t)))
    (should (equal (split-string (substring-no-properties text) "\n")
                   '("a      #### x    " "       ==        ")))
    (should (memq 'vuistack-table-stripe
                  (vuistack-table-test--faces (- (length text) 1) text)))))

(ert-deftest vuistack-table-actions-ride-on-cells ()
  (let* ((hit nil)
         (cols (list (list :header "Name" :width 4 :key :name
                           :action (lambda (row) (setq hit (plist-get row :name))))))
         (line (vuistack-table-row-line cols '(4) '(:name "a") 0 nil)))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (should (vuistack-table-activate))
      (should (equal "a" hit))
      (should (equal '(:name "a") (vuistack-table-row-at-point))))))

;; -- KPI-rule indicator cells --

(ert-deftest vuistack-table-judgment-follows-direction-and-delta ()
  (should (eq 'good (vuistack-table-judgment "higher_is_better" 2)))
  (should (eq 'bad (vuistack-table-judgment 'up -1)))
  (should (eq 'good (vuistack-table-judgment 'lower-is-better -1)))
  (should (eq 'neutral (vuistack-table-judgment 'down 0)))
  (should (eq 'neutral (vuistack-table-judgment 'hold 5)))
  (should (eq 'neutral (vuistack-table-judgment 'up nil))))

(ert-deftest vuistack-table-indicator-faces-follow-metric-class ()
  (should (eq 'vuistack-table-guardrail-pass (vuistack-table-indicator-face 'good)))
  (should (eq 'vuistack-table-guardrail-breach (vuistack-table-indicator-face 'bad)))
  (should-not (vuistack-table-indicator-face 'neutral))
  (should-not (vuistack-table-indicator-face nil))
  (should (eq 'vuistack-table-bad
              (vuistack-table-indicator-face
               '(:metric-class goal :direction lower-is-better :delta 2))))
  (should (eq 'vuistack-table-good
              (vuistack-table-indicator-face '(:metric-class "goal" :judgment good))))
  (should-not (vuistack-table-indicator-face '(:metric-class operational :judgment good))))

(ert-deftest vuistack-table-indicator-column-colors-its-cell ()
  (let* ((cols (list (list :header "KPI" :width 4 :key :v
                           :indicator (lambda (row) (plist-get row :j)))))
         (line (vuistack-table-row-line cols '(4) '(:v 1 :j bad) 0 nil)))
    (should (memq 'vuistack-table-guardrail-breach
                  (vuistack-table-test--faces 0 line)))))

;; -- header, sort, filter --

(ert-deftest vuistack-table-header-marks-sort-and-filter ()
  (let ((line (vuistack-table-header-line vuistack-table-test--cols '(8 7)
                                          '(0 . t) "x")))
    (should (equal "Name ▼   State    [filter: x]" (substring-no-properties line)))
    (should (eq 'vuistack-table-header (get-text-property 0 'face line)))))

(ert-deftest vuistack-table-sorts-numbers-numerically-and-text-lexically ()
  (let ((cols '((:header "N" :key :n) (:header "S" :key :s)))
        (rows '((:n 10 :s "b") (:n 9 :s "c") (:n 100 :s "a"))))
    (should (equal '(9 10 100)
                   (mapcar (lambda (r) (plist-get r :n))
                           (vuistack-table--sorted-rows cols rows '(0)))))
    (should (equal '("c" "b" "a")
                   (mapcar (lambda (r) (plist-get r :s))
                           (vuistack-table--sorted-rows cols rows '(1 . t)))))
    (should (eq rows (vuistack-table--sorted-rows cols rows nil)))))

(ert-deftest vuistack-table-filter-parses-column-scopes ()
  (should (equal '(1 "warn" "state: warn")
                 (vuistack-table--parse-filter vuistack-table-test--cols "state: warn")))
  (should (equal '(nil "a:b" "a:b")
                 (vuistack-table--parse-filter vuistack-table-test--cols "a:b")))
  (should-not (vuistack-table--parse-filter vuistack-table-test--cols "  ")))

(ert-deftest vuistack-table-filter-rows-matches-case-insensitively ()
  (let ((rows '((:name "Alpha" :state "ok") (:name "Beta" :state "WARN"))))
    (should (equal '((:name "Beta" :state "WARN"))
                   (vuistack-table--filter-rows vuistack-table-test--cols rows
                                                '(nil "warn" "warn"))))
    (should-not (vuistack-table--filter-rows vuistack-table-test--cols rows
                                             '(0 "warn" "Name: warn")))))

;; -- the component --

(ert-deftest vuistack-table-renders-header-rows-and-empty-text ()
  (with-temp-buffer
    (let ((text (vuistack-table-test--render
                 :columns vuistack-table-test--cols
                 :rows '((:name "a" :state "ok") (:name "b" :state "warn")))))
      (should (string-match-p "\\`Name     State  \na        ok     \nb        warn"
                              (substring-no-properties text)))))
  (with-temp-buffer
    (should (equal "none" (substring-no-properties
                           (vuistack-table-test--render
                            :columns vuistack-table-test--cols :empty "none"))))))

(ert-deftest vuistack-table-native-hscroll-sticks-left-pinned-cells ()
  (with-temp-buffer
    (let ((window (selected-window)))
      (unwind-protect
          (progn
            (vuistack-table-test--render
             :id 'sticky :columns vuistack-table-test--cols
             :rows '((:name "label" :state "value"))
             :initial-state '(:pin-left (0)))
            (set-window-buffer window (current-buffer))
            (set-window-hscroll window 0)
            (vuistack-table--sticky-refresh)
            (should-not vuistack-table--sticky-overlays)
            ;; Sticks as soon as the window scrolls, before the pinned
            ;; column has fully left the view.
            (set-window-hscroll window 3)
            (vuistack-table--sticky-refresh)
            (should vuistack-table--sticky-overlays)
            (set-window-hscroll window 9)
            (vuistack-table--sticky-refresh)
            (let ((overlay (car vuistack-table--sticky-overlays)))
              (should (overlayp overlay))
              (should (equal "label   " (substring-no-properties
                                        (overlay-get overlay 'display))))
              ;; Nothing scrolled or changed: the overlays are kept.
              (vuistack-table--sticky-refresh)
              (should (eq overlay (car vuistack-table--sticky-overlays)))))
        (set-window-hscroll window 0)
        (mapc #'delete-overlay vuistack-table--sticky-overlays)
        (setq vuistack-table--sticky-overlays nil)))))

(ert-deftest vuistack-table-sticky-header-follows-window-start ()
  (with-temp-buffer
    (vuistack-table-test--render
     :id 'sticky-header :columns vuistack-table-test--cols
     :rows '((:name "a" :state "ok") (:name "b" :state "warn"))
     :sticky-header t)
    (vuistack-table--sticky-refresh)
    (let* ((entry (car vuistack-table--sticky-registry))
           (body (1+ (nth 0 entry)))
           (header (vuistack-table--sticky-header body)))
      (should (string-match-p "Name" header))
      (should-not (vuistack-table--sticky-header (point-min))))))

(ert-deftest vuistack-table-sort-and-filter-commands-are-per-table ()
  (with-temp-buffer
    (let ((rows '((:name "b" :state "ok") (:name "a" :state "warn"))))
      (cl-letf (((symbol-function 'vui-refresh) #'ignore))
        (vuistack-table-test--render :id 'one :columns vuistack-table-test--cols
                                     :rows rows)
        (goto-char (point-min))
        (vuistack-table-sort)
        (should (equal '(0) (vuistack-table--table-sort 'one)))
        (vuistack-table-sort)
        (should (equal '(0 . t) (vuistack-table--table-sort 'one)))
        (should-not (vuistack-table--table-sort 'other))
        (vuistack-table-filter "State: warn")
        (should (equal '(1 "warn" "State: warn") (vuistack-table--table-filter 'one)))
        (let ((inhibit-read-only t)) (erase-buffer))
        (should (string-match-p
                 "\\`Name ▼   State    \\[filter: State: warn\\]\na        warn *\\'"
                 (substring-no-properties
                  (vuistack-table-test--render :id 'one
                                               :columns vuistack-table-test--cols
                                               :rows rows))))
        (goto-char (point-min))
        (vuistack-table-filter-clear)
        (should-not (vuistack-table--table-filter 'one))
        (vuistack-table-reset-state)
        (should-not (vuistack-table--table-sort 'one))))))

(ert-deftest vuistack-table-has-no-personal-dependencies ()
  "The package stands alone: none of its files names a david-* library."
  (dolist (file (directory-files
                 (file-name-directory (locate-library "vuistack-table.el"))
                 t "\\`vuistack-table.*\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((case-fold-search nil))
        (should-not (re-search-forward "david-" nil t))))))

(provide 'vuistack-table-test)
;;; vuistack-table-test.el ends here
