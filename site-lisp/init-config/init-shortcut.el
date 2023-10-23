;; -*- coding: utf-8; -*-

;;; There are 3 sections:
;;; - unset keys
;;; - extensions
;;; - extensions-local
;;; - shortcut on built-in function

;;; ------------ unset keys

; originally
; 'C-i' is TAB
; 'C-r' is isearch-backward
(lazy-load-unset-keys
 '("C-z" "C-\\" "C-'" "C-i" "C-r"))



;;; ------------ extensions

;; ------ ace-window
(lazy-load-global-keys
 '(
   ("M-o" . ace-window)
   )
 "init-ace-window")

;; ------ avy
(lazy-load-global-keys
 '(
   ("M-g c" . avy-goto-char)
   ("M-g w" . avy-goto-word-1)
   ("M-g s" . avy-goto-word-0)
   ("M-g l l" . avy-goto-line)
   ("M-g j" . avy-next)
   ("M-g k" . avy-prev)
   )
 "init-avy")

;; ------ citre
(lazy-load-global-keys
 '(
   ("C-x c c" . citre-mode)
   ("C-x c j" . citre-jump)
   ("C-x c J" . citre-jump-back)
   ("C-x c p" . citre-ace-peek)
   ("C-x c u" . citre-update-this-tags-file)
   )
 "init-citre")

;; ------ theme
(lazy-load-global-keys
 '(
   ("<f5>" . ld-modus-themes-toggle)
   )
 "init-theme")

;; ------ multiple cursors
(lazy-load-global-keys
 '(
   ("C-M-." . mc/mark-next-like-this)
   ("C-M-," . mc/unmark-next-like-this)
   ("M-<" . mc/mark-previous-like-this)
   ("M->" . mc/unmark-previous-like-this)
   ("C-M-<" . mc/edit-beginnings-of-lines)
   ("M-<mouse-1>" . mc/add-cursor-on-click)
   )
 "multiple-cursors")

;; ------ swiper
(lazy-load-set-keys
 '(
   ("C-s" . swiper-isearch)
   ("C-c s" . counsel-rg)
   ))



;;; ------------ extensions-local

(lazy-load-set-keys
 '(
   ;; dired-display-buffer
   ("o" . dired-display-buffer)
   ;; dired-narrow
   ("/" . dired-narrow)
   ;; dired-subtree
   ("<tab>" . dired-subtree-cycle)
   ("SPC" . dired-subtree-toggle)
   ("C-p" . dired-subtree-previous-sibling)
   ("C-n" . dired-subtree-next-sibling)
   ("r" . dired-subtree-revert)
   )
 dired-mode-map)

(lazy-load-global-keys
 '(
   ("C-c e e" . toggle-echo-keys)
   ("C-c e c" . echo-keys-clean)
   )
 "echo-keys") ;show every pressed keys

(lazy-load-global-keys
 '(
   ("C-c x e" . ld-eval-elisp-to-next-line)
   )
 "evals") ;execute code

(lazy-load-global-keys
 '(
   ("C-<" . ld-un-indent)
   ("C->" . ld-indent)
   )
 "force-indent") ;control 4 spaces indent manually

(lazy-load-global-keys
 '(
   ("C-c \\" . goto-last-change)
   )
 "goto-last-change")

(lazy-load-global-keys
 '(
   ("M-g l p" . goto-line-preview)
   )
 "goto-line-preview")

(lazy-load-global-keys
 '(
   ("C-c m h a" . highlight-indentation-mode)
   ("C-c m h c" . highlight-indentation-current-column-mode)
   )
 "highlight-indentation")

(lazy-load-global-keys
 '(
   ("C-r i" . ld-indent-buffer)
   ("C-r r" . ld-rename-file-and-buffer)
   ("C-r d" . ld-delete-file-and-buffer)
   ("C-r e" . ld-revert-buffer-no-confirm)
   ("C-i r" . ld-find-file-in-root) ; open file with root by sudo
   )
 "ld-buffer-operations")

(lazy-load-global-keys
 '(
   ("C-;" . ld-cursor-position-1-store) ;store cursor position
   ("C-'" . ld-cursor-position-1-jump) ;jump to cursor position
   ("C-c ," . ld-cursor-position-stack-push) ;push cursor position to stack
   ("C-c ." . ld-cursor-position-stack-pop) ;pop corsor position from stack
   )
 "ld-goto-cursor-stack")

(lazy-load-global-keys
 '(
   ("M-N" . ld-delete-one-block-backward)
   ("M-M" . ld-delete-one-block-forward)
   )
 "ld-delete-block") ;delete a block (eg. a word) forward and backward

(lazy-load-global-keys
 '(
   ("C-i r" . ld-find-file-in-root) ; open file with root by sudo
   )
 "ld-file-operations")

(lazy-load-global-keys
 '(
   ("M-g l p" . ld-goto-percent-line)
   ("M-g t p" . ld-goto-percent-text)
   ("M-g t c" . ld-goto-column)
   )
 "ld-goto-simple")

(lazy-load-global-keys
 '(
   ("M-p" . ld-move-text-up)
   ("M-n" . ld-move-text-down)
   ("C-c l d" . ld-duplicate-current-line-or-region)
   ("C-c l D" . ld-duplicate-and-comment-current-line-or-region)
   ("C-c l k" . ld-delete-current-line)
   ("C-c m l" . ld-mark-line)
   )
 "ld-text-operations")

(lazy-load-global-keys
 '(
   ("C-c w t" . ld-toggle-one-window)
   )
 "ld-toggle-one-window") ;maxmize current window and size back

(lazy-load-global-keys
 '(("C-x j" . neotree-toggle))
 "init-neotree")

(lazy-load-global-keys
 '(
   ("M-j" . watch-next-window-up-line)   ; 'up' to see previous content
   ("M-k" . watch-next-window-down-line) ; 'down' to see further content
   ("M-J" . watch-next-window-up)
   ("M-K" . watch-next-window-down)
   )
 "scroll-next-window")

(lazy-load-global-keys
 '(
   ("<f4>" . emacs-session-save)
   )
 "init-session")



;;; ------------ shortcut on built-in function

;; ------ org related
(lazy-load-set-keys
 '(
   ("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l r" . org-list-repair)
   ))

;; ------ move cursors in current buffer
(lazy-load-set-keys
 '(
   ("M-g b k" . beginning-of-buffer)
   ("M-g b j" . end-of-buffer)
   ("M-g h j" . forward-paragraph)
   ("M-g h k" . backward-paragraph)
   ("M-g l y" . backward-up-list)         ;向左跳出 LIST
   ("M-g l o" . up-list)                  ;向右跳出 LIST
   ("M-g l u" . backward-down-list)       ;向左跳进 LIST
   ("M-g l i" . down-list)                ;向右跳进 LIST
   ("M-g f a" . beginning-of-defun)       ;函数开头
   ("M-g f e" . end-of-defun)             ;函数末尾
   ))

;; ------ other
(lazy-load-set-keys
 '(
   ("C-z r" . global-hl-line-mode)
   ("C-z l" . display-line-numbers-mode)
   ("M--" . text-scale-decrease)
   ("M-=" . text-scale-increase)
   ("M-," . bury-buffer)
   ("M-." . unbury-buffer)
   ("C-c m m" . set-mark-command) ; replace C-Space for Chinese input method
   ("M-;" . comment-dwim)
   ("C-c r" . recentf-open-files)
   ))



(provide 'init-shortcut)

;;; init-shortcut.el ends here
