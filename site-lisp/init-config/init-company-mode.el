;;; Require
(require 'company)
(require 'company-ctags)

;;; Code:
(global-company-mode)

(with-eval-after-load 'company
  (company-ctags-auto-setup))

(provide 'init-company-mode)

;;; init-company-mode.el ends here
