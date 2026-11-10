;;; render.el --- Render a vuistack-table of fake data to SVG -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -L . -L .deps/vui.el -l docs/showcase/render.el OUT.svg
(require 'vuistack-table)
(require 'cl-lib)

(defconst render--fg "#d4d4d4")
(defconst render--bg "#1e1e1e")
(defconst render--palette
  '((vuistack-table-header :fg "#9cdcfe" :bold t)
    (vuistack-table-stripe :bg "#262626")
    (vuistack-table-positive :fg "#6a9955")
    (vuistack-table-negative :fg "#f48771")
    (vuistack-table-footer :fg "#dcdcaa" :bold t)
    (vuistack-table-cell-selected :bg "#264f78")
    (vuistack-table-good :fg "#6a9955")
    (vuistack-table-bad :fg "#f48771"))
  "Explicit colours, so the output does not depend on the Emacs theme.")

(defun render--style (faces)
  "Merge the palette entries of FACES into a plist."
  (let (fg bg bold)
    (dolist (f faces)
      (when (symbolp f)
        (let ((e (cdr (assq f render--palette))))
          (setq fg (or (plist-get e :fg) fg)
                bg (or (plist-get e :bg) bg)
                bold (or (plist-get e :bold) bold)))))
    (list fg bg bold)))

(defun render--esc (s)
  (replace-regexp-in-string "[&<>]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (_ "&gt;"))) s))

(defun render--svg (file)
  "Write the current buffer to FILE as SVG."
  (let* ((cw 8.4) (lh 19) (pad 16)
         (lines (split-string (buffer-string) "\n"))
         (lines (cl-remove-if (lambda (l) (string-empty-p (string-trim l))) lines))
         (w (+ (* 2 pad) (* cw (apply #'max (mapcar #'length lines)))))
         (h (+ (* 2 pad) (* lh (length lines))))
         (out nil) (y 0))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((bol (line-beginning-position)) (eol (line-end-position)) (pos bol))
        (unless (string-empty-p (string-trim (buffer-substring-no-properties bol eol)))
          (while (< pos eol)
            (let* ((next (min eol (or (next-single-property-change pos 'face nil eol) eol)))
                   (st (render--style (ensure-list (get-text-property pos 'face))))
                   (txt (buffer-substring-no-properties pos next))
                   (x (+ pad (* cw (- pos bol)))) (yy (+ pad (* lh y))))
              (when (nth 1 st)
                (push (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%d\" fill=\"%s\"/>"
                              x yy (* cw (length txt)) lh (nth 1 st)) out))
              (unless (string-empty-p (string-trim txt))
                (push (format "<text x=\"%.1f\" y=\"%.1f\" fill=\"%s\"%s xml:space=\"preserve\">%s</text>"
                              x (+ yy 14) (or (nth 0 st) render--fg)
                              (if (nth 2 st) " font-weight=\"bold\"" "")
                              (render--esc txt))
                      out))
              (setq pos next)))
          (cl-incf y)))
      (forward-line 1))
    (with-temp-file file
      (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\"><rect width=\"100%%\" height=\"100%%\" rx=\"8\" fill=\"%s\"/><g font-family=\"Menlo, DejaVu Sans Mono, monospace\" font-size=\"14\">%s</g></svg>\n"
                      (ceiling w) (ceiling h) render--bg
                      (mapconcat #'identity (nreverse out) ""))))))

(defconst render--rows
  '((:product "Aurora lamp"   :region "North" :units 1240 :revenue 38440.0  :delta 0.12)
    (:product "Birch desk"    :region "South" :units 310  :revenue 62000.0  :delta -0.04)
    (:product "Cedar shelf"   :region "North" :units 980  :revenue 29400.0  :delta 0.31)
    (:product "Dune rug"      :region "East"  :units 450  :revenue 21150.5  :delta -0.18)
    (:product "Ember kettle"  :region "East"  :units 2210 :revenue 55250.0  :delta 0.07)
    (:product "Fjord chair"   :region "South" :units 640  :revenue 44800.0  :delta 0.22)
    (:product "Grove planter" :region "West"  :units 1530 :revenue 18360.0  :delta -0.02)
    (:product "Harbor clock"  :region "West"  :units 275  :revenue 12375.0  :delta 0.0)))

(let ((buf (generate-new-buffer "*showcase*")))
  (with-current-buffer buf
    (vui-render
     (vui-component 'vuistack-table
       :id 'showcase
       :columns '((:header "Product" :key :product)
                  (:header "Region" :key :region)
                  (:header "Units" :key :units :aggregation sum :footer aggregate)
                  (:header "Revenue" :key :revenue :money t :width 11 :aggregation sum :footer aggregate)
                  (:header "Change" :key :delta :sign-color t
                           :format (lambda (v) (format "%+.0f%%" (* 100 v)))))
       :rows render--rows
       :zebra t
       :enable-row-selection t
       :page-size 10
       :initial-state '(:sort ((:revenue . desc)))))
    (render--svg (car command-line-args-left))))
(setq command-line-args-left nil)
