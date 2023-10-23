;; -*- coding: utf-8; -*-
;;; Require:
(require 'jsonian)
(require 'lua-mode)

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
                    ("\\.org\\'" . org-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)

                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)

                    ("\\.inc\\'" . asm-mode)

                    ("\\.py\\'" . python-mode)
                    ("SConstruct". python-mode)

                    ("\\.lua\\'" . lua-mode)

                    ("\\.json\\'" . jsonian-mode)

                    ("\\.go\\'" . go-mode)

                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)

                    ("\\.pdf\\'" . pdf-view-mode)

                    ("\\.ts\\'" . typescript-mode)
                    ("\\.tsx\\'" . typescript-mode)

                    ("\\.js.erb\\'" . web-mode)
                    ("\\.js\\'" . web-mode)
                    ("\\.wxs\\'" . web-mode)

                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsx\\'" . web-mode)

                    ;; ("\\.rs$" . rust-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

;;; Mode load.
(autoload 'cmake-mode "cmake-mode")

(autoload 'css-mode "css-mode")

(autoload 'go-mode "go-mode")

(autoload 'jsonian-mode "jsonian-mode")

(autoload 'lua-mode "lua-mode")
(setq lua-indent-level 2)

(autoload 'markdown-mode "markdown-mode")

(autoload 'python-mode "init-python")

(autoload 'web-mode "web-mode")

;; (autoload 'rust-mode "rust-mode")

(provide 'init-mode)

;;; init-mode.el ends here
