;;; Require
(require 'company)
(require 'company-ctags)

;;; Code:
(global-company-mode)

(with-eval-after-load 'company
  (company-ctags-auto-setup))

; remove company backend company-dabbrev-code
(setq company-backends
      (mapcar (lambda (backend)
                (cond
                 ((eq backend 'company-dabbrev-code) nil)
                 ((listp backend)
                  (remove 'company-dabbrev-code backend))
                 (t backend)))
              company-backends))

(provide 'init-company-mode)

;;; init-company-mode.el ends here
