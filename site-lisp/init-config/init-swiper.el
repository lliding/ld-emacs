;; -*- coding: utf-8; -*-
;;; Require:
(require 'ivy)
(require 'counsel)
(require 'swiper)

;;; Code:
(setq ivy-use-virtual-buffers t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-count-format "(%d/%d) ")

;; Map commands to their minimum required input length.
;; That is the number of characters prompted for before fetching
;; candidates.  The special key t is used as a fallback.
(setq ivy-more-chars-alist '((t . 2)))

(ivy-mode 1)

(setq counsel-rg-base-command
  `("rg"
    "--max-columns" "0"
    "--with-filename"
    "--no-heading"
    "--line-number"
    "--color" "never"
    "%s"
    ,@(and (memq system-type '(ms-dos windows-nt))
           (list "--path-separator" "/" "."))))

(counsel-mode 1)

(provide 'init-swiper)

;; init-swiper.el ends here
