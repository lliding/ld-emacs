;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;'up' to see previous content
;'down' to see further content
(defun scroll-next-window-up ()
  (interactive)
  (scroll-next-window-internal "up"))

(defun scroll-previous-window-up ()
  (interactive)
  (scroll-previous-window-internal "up"))

(defun scroll-next-window-down ()
  (interactive)
  (scroll-next-window-internal "down"))

(defun scroll-previous-window-down ()
  (interactive)
  (scroll-previous-window-internal "down"))

(defun scroll-next-window-up-line ()
  (interactive)
  (scroll-next-window-internal "up" 1))

(defun scroll-previous-window-up-line ()
  (interactive)
  (scroll-previous-window-internal "up" 1))

(defun scroll-next-window-down-line ()
  (interactive)
  (scroll-next-window-internal "down" 1))

(defun scroll-previous-window-down-line ()
  (interactive)
  (scroll-previous-window-internal "down" 1))

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

(defun scroll-previous-window-internal (direction &optional line)
  (save-excursion
    ;; Switch to next window.
    (other-window -1)
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
    (other-window 1)
    ))

(provide 'scroll-adjacent-window)

;;; scroll-adjacent-window.el ends here
