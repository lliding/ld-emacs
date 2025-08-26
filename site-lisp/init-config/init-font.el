;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; font
(when (and window-system *win64*)
  (let ((default-font (font-spec :name "LXGW WenKai Mono"))
        (cn-font (font-spec :name "LXGW WenKai Mono")))
    (set-face-attribute 'default nil :font default-font :height 120)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset cn-font)))
  (set-face-font 'fixed-pitch "LXGW WenKai Mono"))

(provide 'init-font)

;;; init-font.el ends here
