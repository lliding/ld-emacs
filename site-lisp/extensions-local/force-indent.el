;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:

;; use variable 'tab-width' value as indent size

(defun force-indent-line ()
  (let (col)
    (save-excursion
      (back-to-indentation)
      (setq col (+ (current-column) tab-width))
      (indent-line-to col))
    (when (< (current-column) col)
      (back-to-indentation))))

(defun indent-line ()
  (interactive)
  (let ((bt (save-excursion
              (back-to-indentation)
              (current-column))))
    (cond
     ((< (current-column) bt)
      (back-to-indentation))
     ((looking-at "\\s-*\n")
      (let ((col (save-excursion
                   (forward-line -1)
                   (back-to-indentation)
                   (current-column))))
        (if (< (current-column) col)
            (indent-line-to col)
          (force-indent-line))))
     (t
      (force-indent-line)))))

(defun un-indent-line ()
  (interactive)
  (let (col)
    (save-excursion
      (back-to-indentation)
      (setq col (- (current-column) tab-width))
      (when (>= col 0)
        (indent-line-to col)))))

(defun indent-region (start stop)
  (interactive "r")
  (setq stop (copy-marker stop))
  (goto-char start)
  (while (< (point) stop)
    (unless (and (bolp) (eolp))
      (force-indent-line))
    (forward-line 1)))

(defun un-indent-region (start stop)
  (interactive "r")
  (setq stop (copy-marker stop))
  (goto-char start)
  (while (< (point) stop)
    (unless (and (bolp) (eolp))
      (un-indent-line))
    (forward-line 1)))

(defun ld-indent ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (indent-region (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (indent-line)))

(defun ld-un-indent ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (un-indent-region (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (un-indent-line)))

(provide 'force-indent)

;;; force-indent.el ends here