;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defun ld-goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))

; ---

(defun ld-goto-percent-text (percent)
  "Move the cursor to the character,
  which is <percent>% far from the top character."
  (interactive "n(text) Goto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

; ---

(defun ld-goto-percent-line (percent)
  "Move the cursor to the line,
  which is <percent>% far from the top line."
  (interactive "n(line) Goto percent: ")
  (goto-line (/ (* percent (count-lines (point-min) (point-max)))
                100)))

(provide 'ld-goto-simple)

;;; ld-goto-simple.el ends here

