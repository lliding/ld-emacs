;; -*- coding: utf-8; -*-
;;; Require:
(require 'highlight-parentheses)

;;; Code:
(setq hl-paren-colors '("DarkOrange" "DeepSkyBlue" "DarkRed"))
(add-hook 'find-file-hook 'highlight-parentheses-mode t)

(provide 'init-highlight-parentheses)

;;; init-highlight-parentheses.el ends here
