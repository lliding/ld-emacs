;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defvar ld-cursor-position-stack nil
  "Cursor position stack.")

(defun ld-cursor-position-1-store ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun ld-cursor-position-1-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun ld-cursor-position-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq ld-cursor-position-stack (cons (list (current-buffer) (point)) ld-cursor-position-stack)))

(defun ld-cursor-position-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null ld-cursor-position-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar ld-cursor-position-stack))
    (goto-char (cadar ld-cursor-position-stack))
    (setq ld-cursor-position-stack (cdr ld-cursor-position-stack))))

(provide 'ld-goto-cursor-stack)

;;; ld-goto-cursor-stack.el ends here
