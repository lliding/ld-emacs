;; -*- coding: utf-8; -*-
;;; Require:
(require 'subword)

;;; Code:
(defun ld-delete-one-block-forward ()
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun ld-delete-one-block-backward ()
  (interactive)
  (if (bobp)
      (message "Beginning of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-backward (string (char-syntax (char-before))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-backward)
              (point))))
      (kill-region (point) (max syntax-move-point subword-move-point)))))

(provide 'ld-delete-block)

;;; ld-delete-block.el ends here