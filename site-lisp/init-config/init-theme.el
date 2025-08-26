;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
;; In all of the following, WEIGHT is a symbol such as `semibold',
;; `light', `bold', or anything mentioned in `modus-themes-weights'.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `italic', `WEIGHT'
      modus-themes-prompts '(italic bold)

      ;; The `modus-themes-completions' is an alist that reads two
      ;; keys: `matches', `selection'.  Each accepts a nil value (or
      ;; empty list) or a list of properties that can include any of
      ;; the following (for WEIGHT read further below):
      ;;
      ;; `matches'   :: `underline', `italic', `WEIGHT'
      ;; `selection' :: `underline', `italic', `WEIGHT'
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      ;; The `modus-themes-headings' is an alist: read the manual's
      ;; node about it or its doc string.  Basically, it supports
      ;; per-level configurations for the optional use of
      ;; `variable-pitch' typography, a height value as a multiple of
      ;; the base font size (e.g. 1.5), and a `WEIGHT'.
      modus-themes-headings
      '((1 . (variable-pitch 1.5))
        (2 . (1.3))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))

(defun ld-modus-operandi ()
  "Light theme."
  (interactive)
  ;; load theme firstly and then do user customization
  ;; otherwise modus-themes will override the face user had assigned
  (load-theme 'modus-operandi :no-confirm))

(defun ld-modus-vivendi-tinted ()
  "Dark theme."
  (interactive)
  ;; load theme firstly and then do user customization
  ;; otherwise modus-themes will override the face user had assigned
  (load-theme 'modus-vivendi-tinted :no-confirm))

(defun ld-modus-themes-toggle ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (ld-modus-vivendi-tinted)
    (ld-modus-operandi)))

;; active light theme
(if window-system
    (ld-modus-operandi)
  (ld-modus-vivendi-tinted))

(provide 'init-theme)

;;; init-theme.el ends here
