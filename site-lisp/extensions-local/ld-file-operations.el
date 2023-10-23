;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defun ld-find-file-in-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (require 'tramp)
  (tramp-cleanup-all-connections)
  (find-file (concat "/sudo:root@localhost:" file)))

(provide 'ld-file-operations)

;;; ld-file-operations.el ends here
