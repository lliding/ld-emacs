;; -*- coding: utf-8; -*-

;;; There are 4 sections:
;;; - unset keys
;;; - extensions
;;; - extensions-local
;;; - shortcut for built-in function

;;; ------------ unset keys

; originally
; 'C-i' is TAB
(lazy-load-unset-keys
 '("C-z" "C-\\" "C-'"))



;;; ------------ extensions

;; ------ avy
(lazy-load-global-keys
 '(
   ("C-; c" . avy-goto-char)
   ("C-; w" . avy-goto-word-1)
   ("C-; s" . avy-goto-word-0)
   ("C-; h" . avy-goto-line)
   ("C-; j" . avy-next)
   ("C-; k" . avy-prev)
   )
 "init-avy")

;; ------ citre
(lazy-load-global-keys
 '(
   ("C-z c" . citre-mode)
   ("C-' j" . citre-jump)
   ("C-' J" . citre-jump-back)
   ("C-' p" . citre-ace-peek)
   ("C-' u" . citre-update-this-tags-file)
   )
 "init-citre")

;; ------ theme
(lazy-load-global-keys
 '(
   ("C-x t s" . ld-modus-themes-toggle)
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



;;; ------------ extensions-local

(lazy-load-global-keys
 '(
   ("C-z e e" . toggle-echo-keys)
   ("C-z e c" . echo-keys-clean)
   )
 "echo-keys") ;show every pressed keys

(lazy-load-global-keys
 '(
   ("C-c x e" . ld-eval-elisp-to-next-line)
   )
 "evals") ;execute code

(lazy-load-global-keys
 '(
   ("C-<" . ld-unindent)
   ("C->" . ld-indent)
   )
 "force-indent") ;control 4 spaces indent manually

(lazy-load-global-keys
 '(
   ("C-; l" . goto-line-preview)
   ("C-; r" . goto-line-preview-relative)
   )
 "goto-line-preview")

(lazy-load-global-keys
 '(
   ("C-z h i a" . highlight-indentation-mode)
   ("C-z h i c" . highlight-indentation-current-column-mode)
   )
 "highlight-indentation")

(lazy-load-global-keys
 '(
   ("C-c I" . ld-indent-buffer)
   ("C-x R r" . ld-rename-file-and-buffer)
   ("C-x D" . ld-delete-file-and-buffer)
   ("C-x e" . ld-revert-buffer-no-confirm)
   ("C-x R R" . ld-find-file-in-root)
   )
 "ld-file-and-buffer-operations")

(lazy-load-global-keys
 '(
   ("C-; p n" . ld-goto-percent-line)
   ("C-; p t" . ld-goto-percent-text)
   ("C-; f" . ld-goto-column)
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
   ("C-M-J" . scroll-next-window-up-line)
   ("C-M-H" . scroll-next-window-up)
   ("C-M-K" . scroll-next-window-down-line)
   ("C-M-L" . scroll-next-window-down)
   ("C-M-U" . scroll-previous-window-up-line)
   ("C-M-Y" . scroll-previous-window-up)
   ("C-M-I" . scroll-previous-window-down-line)
   ("C-M-O" . scroll-previous-window-down)
   )
 "scroll-adjacent-window")

(lazy-load-global-keys
 '(
   ("<f4>" . emacs-session-save)
   )
 "init-session")



;;; ------------ shortcut for built-in function

;; ------ org related
(lazy-load-set-keys
 '(
   ("C-c o c" . org-capture)
   ("C-c o a" . org-agenda)
   ("C-c o l r" . org-list-repair)
   ))

;; ------ switch between windows
(lazy-load-set-keys
 '(
   ("M-k" . windmove-up)
   ("M-j" . windmove-down)
   ("M-i" . windmove-left)
   ("M-o" . windmove-right)
   ("M-K" . shrink-window)
   ("M-J" . enlarge-window)
   ("M-O" . shrink-window-horizontally)
   ("M-I" . enlarge-window-horizontally)
   ))

;; ------ other
(lazy-load-set-keys
 '(
   ("C-z h l" . global-hl-line-mode)
   ("C-z n m" . display-line-numbers-mode)
   ("M--" . text-scale-decrease)
   ("M-=" . text-scale-increase)
   ("M-N" . bury-buffer)
   ("M-P" . unbury-buffer)
   ("M-;" . comment-dwim)
   ("C-c r" . recentf-open-files)
   ))



(provide 'init-shortcut)

;;; init-shortcut.el ends here
