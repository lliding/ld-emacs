;; -*- coding: utf-8; -*-
;;; Require:
(require 'which-key)

;;; Code:
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-max-description-length nil) ;; show entire key name

(which-key-mode)

(which-key-setup-side-window-bottom)

(provide 'init-which-key)

;;; init-which-key.el ends here
