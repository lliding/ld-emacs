;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(defcustom dired-display-buffer-switch-window t
  "Switch focus to the newly created buffer window. nil to disable."
  :type 'boolean
  )

(defun ld-display-buffer (buffer-or-name alist direction &optional size pixelwise)
  "BUFFER:  The buffer that will be displayed.
ALIST:  See the doc-string of `display-buffer' for more information.
DIRECTION:  Must use one of these symbols:  'left 'right 'below 'above
SIZE:  See the doc-string for `split-window'.
PIXELWISE:  See the doc-string for `split-window'.
There are three possibilities:
-  (1) If a window on the frame already displays the target buffer,
then just reuse the same window.
-  (2) If there is already a window in the specified direction in relation
to the selected window, then display the target buffer in said window.
-  (3) If there is no window in the specified direction, then create one
in that direction and display the target buffer in said window."
  (let* ((buffer
          (if (bufferp buffer-or-name)
              buffer-or-name
            (get-buffer buffer-or-name)))
         (window
          (cond
           ((get-buffer-window buffer (selected-frame)))
           ((window-in-direction direction))
           (t
            (split-window (selected-window) size direction pixelwise)))))
    ;; (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (window--display-buffer buffer window 'window alist)
    (if dired-display-buffer-switch-window
        (select-window window))
    ))

(defun dired-display-buffer (&optional direction alist)
  "Display a dired-mode buffer or a file underneath point in a dired-mode buffer."
  (interactive)
  (let* ((file-or-dir (or (and (eq major-mode 'dired-mode) (dired-get-file-for-visit))
                          (read-directory-name "Directory:  ")))
         (buffer (find-file-noselect file-or-dir))
         (direction
          (if direction
              direction
            (let ((char (read-char-exclusive (concat
                                              "["
                                              (propertize "l" 'face '(:foreground "red"))
                                              "]"
                                              (propertize "eft" 'face '(:foreground "blue"))
                                              " | ["
                                              (propertize "r" 'face '(:foreground "red"))
                                              "]"
                                              (propertize "ight" 'face '(:foreground "blue"))
                                              " | ["
                                              (propertize "a" 'face '(:foreground "red"))
                                              "]"
                                              (propertize "bove" 'face '(:foreground "blue"))
                                              " | ["
                                              (propertize "b" 'face '(:foreground "red"))
                                              "]"
                                              (propertize "elow" 'face '(:foreground "blue"))))))
              (cond
               ((eq char ?l)
                'left)
               ((eq char ?r)
                'right)
               ((eq char ?a)
                'above)
               ((eq char ?b)
                'below)
                  ;;; FIXME:  @lawlist may add a loop similar to `org-capture'
                  ;;; whereby a new `read-char-exclusive' will be initiated if
                  ;;; a user did not initially choose a valid option (l/r/a/b).
               (t
                (let ((debug-on-quit nil)
                      (msg (concat "dired-display-buffer:  "
                                   "You did not select l/r/a/b "
                                   "-- exiting.")))
                  (signal 'quit `(,msg)))))))))
    (ld-display-buffer buffer alist direction)))

(provide 'dired-display-buffer)

;;; dired-display-buffer.el ends here
