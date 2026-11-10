;;; vuistack-table-cell.el --- Cell values and formatting for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; Pure helpers every other vuistack-table file builds on: a cell's raw
;; value, its default string, grouped money, and padding to a width.

;;; Code:

(require 'cl-lib)

(defgroup vuistack-table nil
  "Sortable, filterable zebra table component for vui.el."
  :group 'tools
  :prefix "vuistack-table-")

(defun vuistack-table--string (value)
  "VALUE for a cell: nil is an em dash, t is yes, else `format' %s."
  (cond ((null value) "—") ((eq value t) "yes") (t (format "%s" value))))

(defun vuistack-table--money-value (value)
  "Return numeric VALUE or its first signed numeric token, or nil."
  (cond ((numberp value) value)
        ((and (stringp value)
              (string-match-p
               "\\`[+-]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\'"
               value))
         (string-to-number value))
        ((and (stringp value)
              (string-match "[-+−]?[0-9][0-9,]*\\(?:\\.[0-9]+\\)?" value))
         (let ((token (replace-regexp-in-string "," "" (match-string 0 value))))
           (string-to-number (replace-regexp-in-string "−" "-" token))))))

(defun vuistack-table--number (value)
  "VALUE as a number (see `vuistack-table--money-value'), or nil."
  (vuistack-table--money-value value))

(defun vuistack-table-money-string (value)
  "Format numeric VALUE with grouping and exactly two decimals.
Non-numeric and nil values use `vuistack-table--string'."
  (let ((number (vuistack-table--money-value value)))
    (if (null number)
        (vuistack-table--string value)
      (let* ((scaled (floor (+ (* 100 (abs number)) 0.5)))
             (whole (/ scaled 100))
             (fraction (% scaled 100))
             (groups nil)
             (remaining whole)
             (sign (if (and (< number 0) (> scaled 0)) "-" "")))
        (while (>= remaining 1000)
          (push (% remaining 1000) groups)
          (setq remaining (/ remaining 1000)))
        (format "%s%s.%02d" sign
                (concat (number-to-string remaining)
                        (mapconcat (lambda (group) (format ",%03d" group))
                                   groups ""))
                fraction)))))

(defun vuistack-table--dig (value path)
  "VALUE followed down PATH: keywords index plists, integers lists and
vectors, anything else alists."
  (dolist (key path value)
    (setq value (cond ((integerp key)
                       (if (vectorp value)
                           (and (< -1 key (length value)) (aref value key))
                         (and (listp value) (nth key value))))
                      ((and (keywordp key) (plistp value)) (plist-get value key))
                      ((consp value) (alist-get key value nil nil #'equal))))))

(defun vuistack-table--cell-value (col row)
  "The raw value of column COL for ROW: its :get, :key or :path (deep keys)."
  (cond ((plist-get col :get) (funcall (plist-get col :get) row))
        ((plist-get col :key) (plist-get row (plist-get col :key)))
        ((plist-get col :path) (vuistack-table--dig row (plist-get col :path)))))

(defun vuistack-table--formatter (col)
  "The (VALUE -> string) formatter of column COL."
  (or (plist-get col :format)
      (and (plist-get col :money) #'vuistack-table-money-string)
      #'vuistack-table--string))

(defun vuistack-table--pad (text width)
  "TEXT truncated with an ellipsis to WIDTH, then space-padded to it."
  (let ((text-width (string-width text)))
    (if (<= text-width width)
        (concat text (make-string (- width text-width) ?\s))
      (truncate-string-to-width text width nil ?\s "…"))))

(provide 'vuistack-table-cell)
;;; vuistack-table-cell.el ends here
