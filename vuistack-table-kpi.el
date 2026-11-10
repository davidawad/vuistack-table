;;; vuistack-table-kpi.el --- KPI-rule indicator faces for vuistack-table -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <davidawad@users.noreply.github.com>
;; SPDX-License-Identifier: MIT

;; This file is part of vuistack-table.

;;; Commentary:
;;
;; The face an :indicator cell gets.  A metric moving in its favorable
;; direction is good, against it bad, no movement or no direction
;; neutral; the metric class (goal, guardrail, operational) picks the
;; face family, and operational metrics and neutral judgments are never
;; colored.

;;; Code:

(require 'vuistack-table-cell)

(defface vuistack-table-good
  '((t :inherit success))
  "Indicator cell of a goal metric that moved the right way."
  :group 'vuistack-table)

(defface vuistack-table-bad
  '((t :inherit error))
  "Indicator cell of a goal metric that moved the wrong way."
  :group 'vuistack-table)

(defface vuistack-table-guardrail-pass
  '((t :inherit success :weight bold))
  "Indicator cell of a guardrail metric that passes.
Bold, so a guardrail never reads as a goal metric trending well."
  :group 'vuistack-table)

(defface vuistack-table-guardrail-breach
  '((t :inherit error :weight bold))
  "Indicator cell of a guardrail metric in breach."
  :group 'vuistack-table)

(defun vuistack-table--normalize-direction (direction)
  "DIRECTION (string or symbol) as `higher-is-better', `lower-is-better'
or `neutral'.  Accepts higher_is_better/lower_is_better/target and
up/down/hold; anything else is `neutral'."
  (let ((d (cond ((null direction) nil)
                 ((symbolp direction) (symbol-name direction))
                 (t (format "%s" direction)))))
    (cond ((member d '("higher_is_better" "higher-is-better" "up"))
           'higher-is-better)
          ((member d '("lower_is_better" "lower-is-better" "down"))
           'lower-is-better)
          (t 'neutral))))

(defun vuistack-table--normalize-metric-class (metric-class)
  "METRIC-CLASS (string or symbol) as `goal', `guardrail', `operational' or nil."
  (let ((m (cond ((null metric-class) nil)
                 ((symbolp metric-class) (symbol-name metric-class))
                 (t (format "%s" metric-class)))))
    (cond ((equal m "goal") 'goal)
          ((equal m "guardrail") 'guardrail)
          ((equal m "operational") 'operational))))

(defun vuistack-table-judgment (direction delta)
  "Whether DELTA reads as `good', `bad' or `neutral' given DIRECTION.
A nil, zero or non-numeric DELTA is `neutral', and so is any DELTA under
a neutral DIRECTION (see `vuistack-table--normalize-direction')."
  (let ((direction (vuistack-table--normalize-direction direction)))
    (cond ((or (not (numberp delta)) (zerop delta)) 'neutral)
          ((eq direction 'higher-is-better) (if (> delta 0) 'good 'bad))
          ((eq direction 'lower-is-better) (if (< delta 0) 'good 'bad))
          (t 'neutral))))

(defun vuistack-table-kpi-face (metric-class judgment)
  "The face for JUDGMENT (good/bad/neutral) of a METRIC-CLASS metric.
A goal (the default for nil or unknown METRIC-CLASS) uses the good/bad
faces, a guardrail the bold pass/breach faces, and an operational
metric is never colored.  A neutral JUDGMENT is never colored."
  (pcase (or (vuistack-table--normalize-metric-class metric-class) 'goal)
    ('operational nil)
    ('guardrail
     (pcase judgment
       ('good 'vuistack-table-guardrail-pass)
       ('bad 'vuistack-table-guardrail-breach)))
    (_
     (pcase judgment
       ('good 'vuistack-table-good)
       ('bad 'vuistack-table-bad)))))

(defun vuistack-table-indicator-face (spec)
  "The face for indicator SPEC, or nil.
SPEC is a judgment symbol (good/bad/neutral), a plist (:judgment J
:metric-class C), or (:direction D :delta N :metric-class C) judged by
`vuistack-table-judgment'.  The default class is guardrail."
  (when spec
    (let* ((plist (if (symbolp spec) (list :judgment spec) spec))
           (class (or (plist-get plist :metric-class) 'guardrail))
           (judgment
            (or (plist-get plist :judgment)
                (vuistack-table-judgment (plist-get plist :direction)
                                         (plist-get plist :delta)))))
      (vuistack-table-kpi-face class judgment))))

(provide 'vuistack-table-kpi)
;;; vuistack-table-kpi.el ends here
