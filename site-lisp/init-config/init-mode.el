;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; bind ext to a specific mode
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.c\\'" . c-mode)
                    ("\\.org\\'" . org-mode)

                    ("\\.py\\'" . python-mode)

                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.json\\'" . jsonian-mode)

                    ("\\.css\\'" . web-mode)
                    ("\\.wxss\\'" . web-mode)
                    ("\\.js\\'" . web-mode)
                    ("\\.vue" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsx\\'" . web-mode)
                    ("\\.wxml" . web-mode)

                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(autoload 'cmake-mode "cmake-mode")

(require 'jsonian)
(autoload 'jsonian-mode "jsonian-mode")

(autoload 'markdown-mode "markdown-mode")

(autoload 'web-mode "web-mode")

(provide 'init-mode)

;;; init-mode.el ends here
