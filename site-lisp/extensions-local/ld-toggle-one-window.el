;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defvar ld-toggle-one-window-config-of-window nil
  "The window configuration used for `toggle-one-window'.")

(defun ld-toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
      (if toggle-one-window-config-of-window
          (progn
            (set-window-configuration toggle-one-window-config-of-window)
            (setq toggle-one-window-config-of-window nil))
        (message "No other windows exist."))
    (setq toggle-one-window-config-of-window (current-window-configuration))
    (delete-other-windows)))

(provide 'ld-toggle-one-window)

;;; ld-toggle-one-window.el ends here