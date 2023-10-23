;; -*- coding: utf-8; -*-
;;; Require:
(require 'org)

;;; Code:
(setq org-agenda-files '("~/Documents/ld_org_todo"))

(setq org-capture-templates
      '(
        ("t" "todo" entry
         (file+datetree "~/Documents/ld_org_todo/todo.org")
         "* TODO [#C] %?\n\n%i"
         :empty-lines 1 :tree-type month)
        ("w" "waiting" entry
         (file+datetree "~/Documents/ld_org_todo/waiting.org")
         "* TODO [#C] %?"
         :empty-lines 1 :tree-type month)
        ))

(setq org-refile-targets
      '(
        ("~/Documents/ld_org_todo/todo.org" :maxlevel . 4)
        ("~/Documents/ld_org_todo/waiting.org" :maxlevel . 4)
        (nil :maxlevel . 4)
        ))

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)"
                                    "|" "DONE(d)" "ABORTED(a)")))
(setq org-todo-keyword-faces '(("TODO" . "red")
                               ("DOING" . "blue")
                               ("DONE" . "forest green")
                               ("ABORTED" . "gray")))

(provide 'init-org-todo)

;;; init-org-todo.el ends here.
