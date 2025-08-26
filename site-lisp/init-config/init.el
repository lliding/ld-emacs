;;; -*- coding: utf-8-unix; lexical-binding: t; -*-

(let (
      ;; Make startup faster by reducing the frequency of garbage collection.
      ;; default (* 800 1000) bytes
      (gc-cons-threshold most-positive-fixnum)
      ;; default 0.1
      (gc-cons-percentage 0.6))

  ;; keep frame size
  (setq frame-inhibit-implied-resize t)

  ;; from local extensions
  ;; firstly loaded part
  (require 'init-font)
  (require 'lazy-load)
  (require 'init-generic)
  (require 'ld-tools)

  (require 'init-auto-save)
  (require 'init-coding-system)
  (require 'init-company-mode)
  (require 'init-dired)
  (require 'init-highlight-parentheses)
  (require 'init-indent)
  (require 'init-mode)
  (require 'init-org-todo)
  (require 'init-org)
  (require 'init-proxy)
  (require 'init-time)
  (require 'init-undo-tree)
  (require 'init-yasnippet)
  
  (when *emacs30*
    (require 'init-theme)
    (require 'init-which-key))

  ;; restore session
  (require 'init-session)
  (emacs-session-restore)

  ;; finally load other plugins dynamically
  (require 'init-shortcut)
)

(provide 'init)

;;; init.el ends here
