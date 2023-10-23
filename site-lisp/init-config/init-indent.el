;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun adjust-languages-indent (n)
  (setq-local tab-width n)

  (when (or (derived-mode-p 'c-mode))
    (setq-local c-basic-offset n))

  (when (or (derived-mode-p 'js-mode)
            (derived-mode-p 'js2-mode)
            (derived-mode-p 'web-mode))
    (setq-local javascript-indent-level n)
    (setq-local js-indent-level n)
    (setq-local js2-basic-offset n)

    (setq-local css-indent-offset n)

    (setq-local web-mode-attr-indent-offset n)
    (setq-local web-mode-attr-value-indent-offset n)
    (setq-local web-mode-code-indent-offset n)
    (setq-local web-mode-css-indent-offset n)
    (setq-local web-mode-markup-indent-offset n)
    (setq-local web-mode-sql-indent-offset n)
    ))

(dolist (hook (list 'c-mode-hook
                    'c++-mode-hook
                    'java-mode-hook
                    'haskell-mode-hook
                    'asm-mode-hook
                    'sh-mode-hook
                    'haskell-cabal-mode-hook
                    'ruby-mode-hook
                    'qml-mode-hook
                    'scss-mode-hook
                    'python-mode-hook
                    ))
  (add-hook hook #'(lambda ()
                     (adjust-languages-indent 4)
                     )))

(dolist (hook (list 'lua-mode-hook
                    'org-mode-hook
                    'js-mode-hook
                    'web-mode-hook
                    'yaml-mode-hook
                    ))
  (add-hook hook #'(lambda ()
                     (adjust-languages-indent 2)
                     )))

(provide 'init-indent)

;;; init-indent.el ends here
