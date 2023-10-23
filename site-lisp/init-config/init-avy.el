;;; Require
(require 'avy)

;;; Code:
;; Home row only (the default).
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Any lower-case letter a-z.
; (setq avy-keys (number-sequence ?a ?z))

;; Any lower-case letter or number.  Numbers are specified in the keyboard
;; number-row order, so that the candidate following '9' will be '0'.
; (setq avy-keys (nconc (number-sequence ?a ?z)
;                       (number-sequence ?1 ?9)
;                       '(?0)))

(provide 'init-avy)

;;; init-avy.el ends here
