;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; disable some bars
(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1))
  (menu-bar-mode -1))

;; map Win key and Hyper key to Super and Hyper for emacs on Windows.
(when *win64*
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  (w32-register-hot-key [s-])
  ;; (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key
  (w32-register-hot-key [H-]))

;; Restore emacs session.
;(setq initial-buffer-choice t)
;(run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; turn on word-wrap in all buffers
(global-visual-line-mode t)
(setq word-wrap-by-category t) ;; enhanced CJK word wrap since Emacs28

;; warning in opening large file
(setq large-file-warning-threshold (* 20 1024 1024))

;; line number and column number
(global-display-line-numbers-mode t) ;; show line numbers in every mode
(column-number-mode t) ;; show column number
; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

;; parentheses config
(show-paren-mode t) ;; show bracket pairing
(electric-pair-mode t) ;; automatic completion of parentheses

;; highlight current line
(global-hl-line-mode t)

;; auto reload file content
(global-auto-revert-mode t)

;; delete selected text when input on it
(delete-selection-mode t)

;; record recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; treat OneWord or one_word as one word
(global-subword-mode t)

;; disable cursor blink
(blink-cursor-mode -1)

;; decompress a file when open it
(auto-compression-mode t)

;; disable the ring bell
(setq ring-bell-function 'ignore)

;; set `text-mode' as default major-mode
(setq default-major-mode 'text-mode)

;; use Posix format for time string
(setq system-time-locale "C")

;; replace yes/no with y/n
;; but since emacs 28 user can use (use-short-answers t) to do it
(fset 'yes-or-no-p 'y-or-n-p)

;; do not show welcome page
(setq inhibit-startup-screen t)

;; use single space as sentence end, default is two
(setq sentence-end-double-space nil)

;; make key prompt faster
(setq echo-keystrokes 0.1)

;; increase IO performance
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

;; scrolling config
(setq scroll-margin 3
      scroll-conservatively 101
      auto-window-vscroll nil)

;; clean scratch buffer content
; a non-clean scratch buffer will disturb session restore
(setq initial-scratch-message "")

;; exit emacs without confirmation to kill running processes
(setq confirm-kill-processes nil)

;; resize frame in pixel
(setq frame-resize-pixelwise t)

;; show a big square when cursor is on 'tab'
(setq x-stretch-cursor t)

;; delete duplicate minibuffer history
(setq history-delete-duplicates t)

;; allow scroll in isearch
(setq isearch-allow-scroll t)

;; don't ask me when close emacs with running process
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;; don't ask when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; (setq byte-compile-warnings
;;       (quote (
;;               ;; 显示的警告
;;               free-vars                 ;不在当前范围的引用变量
;;               unresolved                ;不知道的函数
;;               callargs                  ;函数调用的参数和定义的不匹配
;;               obsolete                  ;荒废的变量和函数
;;               noruntime                 ;函数没有定义在运行时期
;;               interactive-only          ;正常不被调用的命令
;;               make-local ;调用 `make-variable-buffer-local' 可能会不正确的
;;               mapcar     ;`mapcar' 调用
;;               ;;
;;               ;; 抑制的警告
;;               (not redefine)        ;重新定义的函数 (比如参数数量改变)
;;               (not cl-functions)    ;`CL' 包中的运行时调用的函数
;;               )))

(provide 'init-generic)

;;; init-generic.el ends here
