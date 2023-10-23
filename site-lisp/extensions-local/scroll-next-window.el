;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;'up' to see previous content
;'down' to see further content
(defun scroll-next-window-up ()
  (interactive)
  (scroll-next-window-internal "up"))

(defun scroll-next-window-down ()
  (interactive)
  (scroll-next-window-internal "down"))

(defun scroll-next-window-up-line ()
  (interactive)
  (scroll-next-window-internal "up" 1))

(defun scroll-next-window-down-line ()
  (interactive)
  (scroll-next-window-internal "down" 1))

(defun scroll-next-window-internal (direction &optional line)
  (save-excursion
    ;; Switch to next window.
    (other-window 1)
    ;; Do scroll operation.
    (ignore-errors
      (if (string-equal direction "up")
          (if line
              (scroll-up line)
            (scroll-up))
        (if line
            (scroll-down line)
          (scroll-down))))
    ;; Switch back to current window.
    (other-window -1)
    ))

(provide 'scroll-next-window)

;;; scroll-next-window.el ends here
