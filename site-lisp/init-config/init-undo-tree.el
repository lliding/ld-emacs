;; -*- coding: utf-8; -*-
;;; Require:
(require 'undo-tree)

;;; Code:
(global-undo-tree-mode)

;; ;; --- undo-tree
;; (lazy-load-local-keys
;;  '(
;;    ("C-/" . undo-tree-undo)
;;    ("C-?" . undo-tree-redo)
;;    )
;;  undo-tree-map
;;  "undo-tree")

(provide 'init-undo-tree)

;;; init-undo-tree.el ends here
