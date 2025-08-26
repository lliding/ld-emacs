;; -*- coding: utf8; -*-
;;; Require:
(require 'projectile)

;;; Code:
(projectile-mode +1)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'init-projectile)

;;; init-projectile.el ends here
