;;; vuistack-table-filter.el --- Filter functions and faceting for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; TanStack Table's filterFns, a small filter query language over them,
;; the row predicate the filtered row model uses, and faceting (unique
;; values with counts, min/max) for filter completion.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vuistack-table-cell)

;; -- filter functions --

(defun vuistack-table--time (value)
  "VALUE as float seconds: a number, a Lisp time or a date string; or nil."
  (cond ((numberp value) value)
        ((and (consp value) (numberp (car value))) (float-time value))
        ((and (stringp value) (not (string-empty-p (string-trim value))))
         (ignore-errors (float-time (date-to-time value))))))

(defun vuistack-table--value-strings (col row)
  "The strings a filter on COL tests for ROW.
A list value with no :format gives one string per element (so the arr
filter functions see elements); anything else its formatted cell."
  (let ((value (vuistack-table--cell-value col row)))
    (if (and (consp value) (proper-list-p value) (not (plist-get col :format)))
        (mapcar #'vuistack-table--string value)
      (list (funcall (or (plist-get col :format) #'vuistack-table--string) value)))))

(defun vuistack-table--split-list (text)
  "TEXT split on commas into trimmed, non-empty strings."
  (split-string text "[ \t]*,[ \t]*" t "[ \t]+"))

(defun vuistack-table--range (text parse)
  "TEXT `LO..HI' as (LO . HI) read by PARSE (either may be nil), or nil."
  (when (string-match "\\`\\(.*?\\)\\.\\.\\(.*\\)\\'" text)
    (let ((lo (string-trim (match-string 1 text)))
          (hi (string-trim (match-string 2 text))))
      (unless (and (string-empty-p lo) (string-empty-p hi))
        (let ((lo-value (unless (string-empty-p lo) (funcall parse lo)))
              (hi-value (unless (string-empty-p hi) (funcall parse hi))))
          (when (and (or (string-empty-p lo) lo-value)
                     (or (string-empty-p hi) hi-value))
            (cons lo-value hi-value)))))))

(defun vuistack-table--in-range-p (number range inclusive)
  "Whether NUMBER lies in RANGE (LO . HI), INCLUSIVE or not; nil ends are open."
  (and number
       (or (null (car range)) (if inclusive (>= number (car range)) (> number (car range))))
       (or (null (cdr range)) (if inclusive (<= number (cdr range)) (< number (cdr range))))))

(defun vuistack-table--parse-number (text)
  "TEXT as a number when it is one, else nil."
  (and (string-match-p "\\`[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\'" text)
       (string-to-number text)))

(defun vuistack-table-fuzzy-score (needle haystack)
  "Score of NEEDLE as a case-insensitive subsequence of HAYSTACK, or nil.
Higher is better: a substring scores most, then consecutive and early
character matches (TanStack's fuzzy filter ranks with match-sorter)."
  (let ((needle (downcase needle))
        (hay (downcase haystack)))
    (if (string-empty-p needle)
        0
      (let ((substring (string-search needle hay)))
        (if substring
            (+ 100 (* 4 (length needle)) (if (zerop substring) 10 0))
          (let ((pos 0) (score 0) (last -2))
            (catch 'miss
              (dotimes (i (length needle))
                (let ((found (cl-position (aref needle i) hay :start pos)))
                  (unless found (throw 'miss nil))
                  (setq score (+ score (if (= found (1+ last)) 3 1) (if (zerop found) 2 0))
                        last found
                        pos (1+ found))))
              score)))))))

(defconst vuistack-table-filter-functions
  '((includes-string . vuistack-table--filter-includes-string)
    (includes-string-sensitive . vuistack-table--filter-includes-string-sensitive)
    (equals-string . vuistack-table--filter-equals-string)
    (equals-string-sensitive . vuistack-table--filter-equals-string-sensitive)
    (equals . vuistack-table--filter-equals)
    (weak-equals . vuistack-table--filter-weak-equals)
    (starts-with . vuistack-table--filter-starts-with)
    (ends-with . vuistack-table--filter-ends-with)
    (arr-includes . vuistack-table--filter-arr-includes)
    (arr-has . vuistack-table--filter-arr-includes)
    (arr-includes-all . vuistack-table--filter-arr-includes-all)
    (arr-includes-some . vuistack-table--filter-arr-includes-some)
    (in-number-range . vuistack-table--filter-in-number-range)
    (between-inclusive . vuistack-table--filter-in-number-range)
    (between . vuistack-table--filter-between)
    (in-date-range . vuistack-table--filter-in-date-range)
    (empty . vuistack-table--filter-empty)
    (not-empty . vuistack-table--filter-not-empty)
    (fuzzy . vuistack-table--filter-fuzzy))
  "Built-in filter functions by name, after TanStack's filterFns.
Each takes (COLUMN ROW QUERY) and returns non-nil when ROW passes.  A
column's :filter-fn may name one, or be a function (VALUE QUERY ROW).")

(defun vuistack-table--filter-includes-string (col row query)
  "Case-insensitive substring QUERY in COL's cell for ROW."
  (let ((needle (downcase query)))
    (cl-some (lambda (s) (string-search needle (downcase s)))
             (vuistack-table--value-strings col row))))

(defun vuistack-table--filter-includes-string-sensitive (col row query)
  "Case-sensitive substring QUERY in COL's cell for ROW."
  (cl-some (lambda (s) (string-search query s)) (vuistack-table--value-strings col row)))

(defun vuistack-table--filter-equals-string (col row query)
  "COL's cell for ROW equals QUERY, ignoring case."
  (cl-some (lambda (s) (string-equal-ignore-case query s))
           (vuistack-table--value-strings col row)))

(defun vuistack-table--filter-equals-string-sensitive (col row query)
  "COL's cell for ROW equals QUERY exactly."
  (member query (vuistack-table--value-strings col row)))

(defun vuistack-table--filter-equals (col row query)
  "COL's raw value for ROW is `equal' to QUERY.
QUERY is read as a number when the value is one."
  (let ((value (vuistack-table--cell-value col row)))
    (equal value (if (numberp value) (vuistack-table--parse-number query) query))))

(defun vuistack-table--filter-weak-equals (col row query)
  "COL's value for ROW and QUERY are equal as strings or as numbers."
  (let ((value (vuistack-table--cell-value col row)))
    (or (equal (vuistack-table--string value) query)
        (and (numberp value) (eql value (vuistack-table--parse-number query))))))

(defun vuistack-table--filter-starts-with (col row query)
  "COL's cell for ROW starts with QUERY, ignoring case."
  (cl-some (lambda (s) (string-prefix-p query s t)) (vuistack-table--value-strings col row)))

(defun vuistack-table--filter-ends-with (col row query)
  "COL's cell for ROW ends with QUERY, ignoring case."
  (cl-some (lambda (s) (string-suffix-p query s t)) (vuistack-table--value-strings col row)))

(defun vuistack-table--filter-arr-includes (col row query)
  "COL's list value for ROW has an element equal (ignoring case) to QUERY."
  (vuistack-table--filter-equals-string col row query))

(defun vuistack-table--filter-arr-includes-all (col row query)
  "COL's list value for ROW holds every comma-separated item of QUERY."
  (let ((strings (mapcar #'downcase (vuistack-table--value-strings col row))))
    (cl-every (lambda (item) (member (downcase item) strings))
              (vuistack-table--split-list query))))

(defun vuistack-table--filter-arr-includes-some (col row query)
  "COL's list value for ROW holds some comma-separated item of QUERY."
  (let ((strings (mapcar #'downcase (vuistack-table--value-strings col row))))
    (cl-some (lambda (item) (member (downcase item) strings))
             (vuistack-table--split-list query))))

(defun vuistack-table--filter-in-number-range (col row query)
  "COL's number for ROW lies in QUERY `LO..HI', both ends inclusive."
  (let ((range (vuistack-table--range query #'vuistack-table--parse-number)))
    (and range (vuistack-table--in-range-p
                (vuistack-table--number (vuistack-table--cell-value col row)) range t))))

(defun vuistack-table--filter-between (col row query)
  "COL's number for ROW lies strictly inside QUERY `LO..HI'."
  (let ((range (vuistack-table--range query #'vuistack-table--parse-number)))
    (and range (vuistack-table--in-range-p
                (vuistack-table--number (vuistack-table--cell-value col row)) range nil))))

(defun vuistack-table--filter-in-date-range (col row query)
  "COL's date for ROW lies in QUERY `FROM..TO' (dates, inclusive)."
  (let ((range (vuistack-table--range query #'vuistack-table--time)))
    (and range (vuistack-table--in-range-p
                (vuistack-table--time (vuistack-table--cell-value col row)) range t))))

(defun vuistack-table--filter-empty (col row _query)
  "COL's value for ROW is nil or blank."
  (let ((value (vuistack-table--cell-value col row)))
    (or (null value) (and (stringp value) (string-blank-p value)))))

(defun vuistack-table--filter-not-empty (col row query)
  "COL's value for ROW is not empty (see `vuistack-table--filter-empty')."
  (not (vuistack-table--filter-empty col row query)))

(defun vuistack-table--filter-fuzzy (col row query)
  "QUERY fuzzily matches COL's cell for ROW (`vuistack-table-fuzzy-score')."
  (cl-some (lambda (s) (vuistack-table-fuzzy-score query s))
           (vuistack-table--value-strings col row)))

(defun vuistack-table--parse-query (query)
  "QUERY of the filter language as (FILTER-FN . ARGUMENT).
  text      includes-string        ~text  fuzzy
  =text     equals-string          ==text equals-string-sensitive
  ^text     starts-with            text$  ends-with
  >N >=N <N <=N, LO..HI            in-number-range (dates: in-date-range)
  =         empty                  !=     not-empty
  all:a,b   arr-includes-all       any:a,b arr-includes-some"
  (let ((range-number (vuistack-table--range query #'vuistack-table--parse-number)))
    (cond
     ((equal query "=") (cons 'empty ""))
     ((equal query "!=") (cons 'not-empty ""))
     ((string-match "\\`\\(>=\\|<=\\|>\\|<\\)[ \t]*\\(.+\\)\\'" query)
      (let ((op (match-string 1 query))
            (number (vuistack-table--parse-number (match-string 2 query))))
        (if (null number)
            (cons 'includes-string query)
          (cons 'in-number-range
                (pcase op
                  (">=" (format "%s.." number))
                  ("<=" (format "..%s" number))
                  (">" (list 'gt number))
                  ("<" (list 'lt number)))))))
     (range-number (cons 'in-number-range query))
     ((vuistack-table--range query #'vuistack-table--time) (cons 'in-date-range query))
     ((string-prefix-p "==" query) (cons 'equals-string-sensitive (substring query 2)))
     ((string-prefix-p "=" query) (cons 'equals-string (substring query 1)))
     ((string-prefix-p "~" query) (cons 'fuzzy (substring query 1)))
     ((and (string-prefix-p "^" query) (> (length query) 1))
      (cons 'starts-with (substring query 1)))
     ((and (string-suffix-p "$" query) (> (length query) 1))
      (cons 'ends-with (substring query 0 -1)))
     ((string-prefix-p "all:" query) (cons 'arr-includes-all (substring query 4)))
     ((string-prefix-p "any:" query) (cons 'arr-includes-some (substring query 4)))
     (t (cons 'includes-string query)))))

(defun vuistack-table--column-matches-p (col row query)
  "Whether ROW passes the filter QUERY on column COL.
COL's :filter-fn decides when set (a function of VALUE QUERY ROW, or the
name of a built-in in `vuistack-table-filter-functions'); otherwise
QUERY is read by `vuistack-table--parse-query'."
  (let ((fn (plist-get col :filter-fn)))
    (cond
     ((and fn (symbolp fn) (alist-get fn vuistack-table-filter-functions))
      (funcall (alist-get fn vuistack-table-filter-functions) col row query))
     ((functionp fn) (funcall fn (vuistack-table--cell-value col row) query row))
     (t
      (pcase-let ((`(,name . ,argument) (vuistack-table--parse-query query)))
        (if (consp argument)              ; strict > or <
            (let ((number (vuistack-table--number (vuistack-table--cell-value col row))))
              (and number (if (eq (car argument) 'gt)
                              (> number (cadr argument))
                            (< number (cadr argument)))))
          (funcall (alist-get name vuistack-table-filter-functions) col row argument)))))))

(defun vuistack-table--global-columns (columns)
  "The COLUMNS a global filter searches: all but :disable-global-filter ones."
  (cl-remove-if (lambda (col) (plist-get col :disable-global-filter)) columns))

(defun vuistack-table--row-passes-p (columns row global column-filters &optional global-fn)
  "Whether ROW passes GLOBAL and every one of COLUMN-FILTERS.
GLOBAL is (COLUMN-INDEX QUERY DISPLAY) or nil: a nil index searches the
globally filterable COLUMNS, or calls GLOBAL-FN (ROW QUERY) when given.
COLUMN-FILTERS is an alist of column index -> query."
  (and (or (null global)
           (let ((index (nth 0 global)) (query (nth 1 global)))
             (cond ((integerp index)
                    (and (nth index columns)
                         (vuistack-table--column-matches-p (nth index columns) row query)))
                   (global-fn (funcall global-fn row query))
                   (t (cl-some (lambda (col) (vuistack-table--column-matches-p col row query))
                               (vuistack-table--global-columns columns))))))
       (cl-every (lambda (pair)
                   (let ((col (nth (car pair) columns)))
                     (or (null col) (vuistack-table--column-matches-p col row (cdr pair)))))
                 column-filters)))

;; -- faceting --

(defun vuistack-table-faceted-unique-values (column rows)
  "Alist of COLUMN's distinct values in ROWS -> count, most frequent first.
List values count each element (TanStack getFacetedUniqueValues)."
  (let ((counts nil))
    (dolist (row rows)
      (let ((value (vuistack-table--cell-value column row)))
        (dolist (v (if (and (consp value) (proper-list-p value)) value (list value)))
          (let ((cell (assoc v counts)))
            (if cell (setcdr cell (1+ (cdr cell))) (push (cons v 1) counts))))))
    (cl-stable-sort (nreverse counts) #'> :key #'cdr)))

(defun vuistack-table-faceted-min-max (column rows)
  "(MIN . MAX) of COLUMN's numeric values in ROWS, or nil."
  (let ((numbers (delq nil (mapcar (lambda (row)
                                     (vuistack-table--number
                                      (vuistack-table--cell-value column row)))
                                   rows))))
    (and numbers (cons (apply #'min numbers) (apply #'max numbers)))))

(defun vuistack-table-global-faceted-unique-values (columns rows)
  "Alist of every globally filterable cell string in ROWS -> count."
  (let ((counts nil))
    (dolist (row rows)
      (dolist (col (vuistack-table--global-columns columns))
        (dolist (s (vuistack-table--value-strings col row))
          (let ((cell (assoc s counts)))
            (if cell (setcdr cell (1+ (cdr cell))) (push (cons s 1) counts))))))
    (cl-stable-sort (nreverse counts) #'> :key #'cdr)))

(provide 'vuistack-table-filter)
;;; vuistack-table-filter.el ends here
