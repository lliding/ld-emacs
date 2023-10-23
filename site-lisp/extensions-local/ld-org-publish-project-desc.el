;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:
(setq
 org-publish-project-alist
 (let* ((ld-site-path "~/Documents/ld_org_article/")
        (ld-site-pub-path "~/Public/ld_org_article_publish/")
        (get-content (lambda (x)
                       (with-temp-buffer
                         (insert-file-contents (concat ld-site-path x))
                         (buffer-string))))
        (ld-site-postamble (funcall get-content "template/postamble.html"))
        (ld-site-preamble (funcall get-content "template/preamble.html"))
        (ld-site-head (funcall get-content "template/head.html")))
   `(
     ("blog"
      :base-directory ,(concat ld-site-path "article/blog/")
      :base-extension "org"
      :publishing-directory ,(concat ld-site-pub-path "article/blog/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "blog"

      :html-doctype "html5"
      :html-head ,ld-site-head
      :html-preamble ,ld-site-preamble
      :html-postamble ,ld-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("wiki"
      :base-directory ,(concat ld-site-path "article/wiki/")
      :base-extension "org"
      :publishing-directory ,(concat ld-site-pub-path "article/wiki/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "wiki"

      :html-doctype "html5"
      :html-head ,ld-site-head
      :html-preamble ,ld-site-preamble
      :html-postamble ,ld-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("translation"
      :base-directory ,(concat ld-site-path "article/translation/")
      :base-extension "org"
      :publishing-directory ,(concat ld-site-pub-path "article/translation/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :auto-sitemap t
      :sitemap-filename "sitemap-index.org"
      :sitemap-title "translation"

      :html-doctype "html5"
      :html-head ,ld-site-head
      :html-preamble ,ld-site-preamble
      :html-postamble ,ld-site-postamble
    ;;   :htmlized-source t

      :with-toc t
      )
     ("site"
      :base-directory ,(concat ld-site-path "article/site/")
      :base-extension "org"
      :publishing-directory ,(concat ld-site-pub-path "article/site/")
      :publishing-function org-html-publish-to-html
      :recursive t
      :headline-levels 4

      :html-doctype "html5"
      :html-head ,ld-site-head
      :html-preamble ,ld-site-preamble
      :html-postamble ,ld-site-postamble
    ;;   :htmlized-source t

      :with-toc nil
      )
     ("static"
      :base-directory ,(concat ld-site-path "article_static/")
      ;; :base-extension "css\\|js\\|ico\\|png\\|jpg\\|gif\\|zip\\|7z\\|rar\\|pdf"
      :base-extension ".*"
      :publishing-directory ,(concat ld-site-pub-path "/article_static")
      :publishing-function org-publish-attachment
      :recursive t
      )
     ("all" :components ("blog" "wiki" "site" "translation" "static"))
     )))

(provide 'ld-org-publish-project-desc)

;;; ld-org-publish-project-desc.el ends here
