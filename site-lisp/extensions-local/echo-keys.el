;; -*- coding: utf-8; -*-
;;; Require:
(require 'cl-lib)

;;; Code:
(defcustom echo-keys-last-record nil
  "Last command processed by 'echo-keys'."
  :type 'string
  :group 'echo-keys)

(defcustom echo-keys-last-record-count 0
  "Number of times the `echo-keys-last-record` command was repeated."
  :type 'integer
  :group 'echo-keys)

(defcustom echo-key-window-width  40
  "Default width of the *echo-keys* window."
  :type 'integer
  :group 'echo-keys)

(defcustom echo-key-password-protection nil
  "Temporarily disable echo key for password input."
  :type 'boolean
  :group 'echo-keys)

(defcustom echo-key-coallesce-repeats t
  "If 't', show <key> <command> [<echo-keys-last-record-count> times].
  If 'nil', show <key> <commands> n lines."
  :type 'boolean
  :group 'echo-keys)

(defun echo-keys ()
  (let ((deactivate-mark deactivate-mark)
        (keys            (this-command-keys)))
    (when (and keys
               (not (eq (current-buffer) (get-buffer "*echo-keys*")))
               (not echo-key-password-protection))
      (save-excursion
       (with-current-buffer (get-buffer-create "*echo-keys*")
         (goto-char (point-max))
         (if (eql this-command 'self-insert-command)
             (let ((desc (key-description keys)))
               (if (= 1 (length desc))
                   (insert desc)
                   (insert " " desc " "))
               (setf echo-keys-last-record this-command
                     echo-keys-last-record-count 1))
             (if (and echo-key-coallesce-repeats
                      (eql echo-keys-last-record this-command))
                 (progn
                   (incf echo-keys-last-record-count)
                   ;; update the last line
                   (forward-line -1)
                   (if (= 2 echo-keys-last-record-count)
                       (progn
                         (end-of-line)
                         (insert (format " [%d times]" echo-keys-last-record-count)))
                       (save-match-data
                        (when (re-search-forward " \\[\\([0-9]+\\) times\\]" nil t)
                          (delete-region (match-beginning 1) (match-end 1))
                          (goto-char (match-beginning 1))
                          (insert (format "%d" echo-keys-last-record-count)))))
                   (forward-line 1))
                 (progn
                   (insert (if (eq 'self-insert-command echo-keys-last-record)
                               "\n"
                               "")
                           (format "%-12s %s\n"
                                   (key-description keys)
                                   this-command))
                   (setf echo-keys-last-record this-command
                         echo-keys-last-record-count 1))))
         (dolist (window (window-list))
           (when (eq (window-buffer window) (current-buffer))
             (with-selected-window window
               ;; We need to use both to get the effect.
               (set-window-point window (point))
               (end-of-buffer)))))))))

(defun toggle-echo-keys ()
  "Toggle displaying the *echo-key* buffer."
  (interactive)
  (if (member 'echo-keys (default-value 'pre-command-hook))
      (let ((echo-buffer (get-buffer "*echo-keys*")))
        (remove-hook 'pre-command-hook 'echo-keys)
        (dolist (window (window-list))
          (when (eq (window-buffer window) echo-buffer)
            (delete-window window))))
      (progn
        (delete-other-windows)
        (split-window nil (- (window-width) echo-key-window-width) t)
        (other-window 1)
        (switch-to-buffer (get-buffer-create "*echo-keys*"))
        (unless (eq major-mode 'echo-keys-mode)
          (echo-keys-mode))
        (toggle-truncate-lines +1)
        (set-window-dedicated-p (selected-window) t)
        (other-window 1)
        (add-hook 'pre-command-hook 'echo-keys))))

(defadvice echo-key--read-passwd--disable (before read-passwd)
  (message "echo-key--read-passwd--disable")
  (setf echo-key-password-protection t))

(defadvice echo-key--read-passwd--enable (after read-passwd)
    (message "echo-key--read-passwd--enable")
  (setf echo-key-password-protection nil))

(defun echo-keys-clean ()
  "Erase the `*echo-keys*' buffer."
  (interactive)
  (with-current-buffer "*echo-keys*"
    (erase-buffer)))

(defvar echo-keys-mode-map
  (let ((ek-mode-map (make-sparse-keymap)))
    (define-key ek-mode-map (kbd "C-c e e") #'toggle-echo-keys)
    (define-key ek-mode-map (kbd "C-c e c") #'echo-keys-clean)
    ek-mode-map))

(define-derived-mode echo-keys-mode fundamental-mode "Echo-keys"
  "Major mode for echo-keys.")

(provide 'echo-keys)

;;; echo-keys.el ends here.
