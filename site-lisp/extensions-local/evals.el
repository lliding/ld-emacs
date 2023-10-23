;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defun ld-eval-elisp-to-next-line ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (newline-and-indent)
    (insert (format "%S" value))))

(defun ld-eval-elisp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(provide 'evals)

;;; evals.el ends here
