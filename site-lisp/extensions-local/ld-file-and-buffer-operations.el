;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defun ld-indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (cond
   ;; judge by mode
   ((derived-mode-p 'python-mode)
    (message "Don't indent python buffer. It will mess up the code syntax."))
   ((derived-mode-p 'yaml-mode)
    (message "Don't indent yaml buffer. It will mess up the code syntax."))
   ;; judge by buffer name
   ((string-suffix-p ".yml" (buffer-name) t)
    (message "Don't indent yaml buffer. It will mess up the code syntax."))
   ((string-suffix-p ".yaml" (buffer-name) t)
    (message "Don't indent yaml buffer. It will mess up the code syntax."))
   (t
    (save-excursion
      (indent-region (point-min) (point-max) nil)
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))))))

; ---

(defun ld-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

; ---

(defun ld-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Sure to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

; ---

(defun ld-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

; ---

(defun ld-unmark-all-buffers ()
  "Unmark all have marked buffers."
  (interactive)
  (let ((current-element (current-buffer)))
    (save-excursion
      (dolist (element (buffer-list))
        (set-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

; ---

(defun ld-find-file-in-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (require 'tramp)
  (tramp-cleanup-all-connections)
  (find-file (concat "/sudo:root@localhost:" file)))

(provide 'ld-buffer-operations)

;;; ld-buffer-operations.el ends here
