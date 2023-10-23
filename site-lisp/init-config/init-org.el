;; -*- coding: utf-8; -*-
;;; Require:
(require 'org)
(require 'ox-publish)

;;; Code:
(defun ld-org-update-lastupdate-property ()
  "If '#+LASTUPDATE' is in org file, update it to the current date/time."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+LASTUPDATE:" (point-max) t)
        (progn
          (setq lastupdate-point (point))
          (if (not (equal lastupdate-point (line-end-position)))
              (delete-region lastupdate-point (line-end-position)))
          (insert (format-time-string " %Y/%m/%d %H:%M")))))))

(add-hook 'before-save-hook #'ld-org-update-lastupdate-property)

(setq org-startup-indented nil) ;; enable org-indent-mode at start, default nil.

(setq org-goto-auto-isearch nil)
(setq org-support-shift-select t)

(setq org-use-sub-superscripts '{})
(setq org-export-with-sub-superscripts '{})

;; If it is not nil, strings below will be interpreted in exporting as
;;  Org     HTML     LaTeX    UTF-8
;; -----+----------+--------+-------
;;  \-    &shy;      \-
;;  --    &ndash;    --         –
;;  ---   &mdash;    ---        —
;;  ...   &hellip;   \ldots     …
(setq org-export-with-special-strings nil)

;; (set-face-attribute 'org-level-1 nil
;;                     :background "#fdf0ff"
;;                     :foreground "#000000"
;;                     :overline "#bcbcbc"
;;                     :bold t
;;                     :height 1.3)
;; (set-face-attribute 'org-level-2 nil
;;                     :foreground "#8f0075"
;;                     :overline "#bcbcbc"
;;                     :bold t
;;                     :height 1.1)
;; (set-face-attribute 'org-level-3 nil
;;                     :foreground "#093060"
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-4 nil
;;                     :foreground "#184034"
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-5 nil
;;                     :foreground "#61284f"
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-6 nil
;;                     :foreground "#3f3000"
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-7 nil
;;                     :foreground "#5f0000"
;;                     :weight 'semi-bold)
;; (set-face-attribute 'org-level-8 nil
;;                     :foreground "#541f4f"
;;                     :weight 'semi-bold)

;; (set-face-attribute 'org-block-begin-line nil
;;                     :background "#f0f0f0"
;;                     :foreground "#505050"
;;                     :extend t)
;; (set-face-attribute 'org-block nil
;;                     :background "#f8f8f8"
;;                     :foreground "#000000"
;;                     :extend t
;;                     )
;; (set-face-attribute 'org-block-end-line nil
;;                     :background "#f0f0f0"
;;                     :foreground "#505050"
;;                     :extend t)

;; (set-face-attribute 'org-code nil
;;                     :background "#f0f0f0"
;;                     :foreground "#005a5f"
;;                     :extend t)

;; (set-face-attribute 'org-verbatim nil
;;                     :background "#f0f0f0"
;;                     :foreground "#8f0075"
;;                     :extend t)

(defun ld-org-custom-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.
MAKE FINAL HTML TO BE COMPATIBLE WITH highlight.js"
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code (org-html-format-code src-block info))
          (label (let ((lbl (and (org-element-property :name src-block)
                                 (org-export-get-reference src-block info))))
                   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n<pre class=\"src src-%s\"%s><code class=\"language-%s\">%s</code></pre>"
                 lang label lang code))))))

(advice-add 'org-html-src-block :override 'ld-org-custom-html-src-block)

(defun ld-org-custom-html-format-list-item (contents type checkbox info
                                                     &optional term-counter-id
                                                     headline)
  "Format a list item into HTML."
  (let ((class (if checkbox
                   (format " class=\"%s\""
                           (symbol-name checkbox)) ""))
        (checkbox (concat (org-html-checkbox checkbox info)
                          (and checkbox " ")))
        (br (org-html-close-tag "br" nil info))
        (extra-newline (if (and (org-string-nw-p contents) headline) "\n" "")))
    (concat
     (pcase type
       (`ordered
        (let* ((counter term-counter-id)
               (extra (if counter (format " value=\"%s\"" counter) "")))
          (concat
           (format "<li%s%s>" class extra)
           (when headline (concat headline br)))))
       (`unordered
        (let* ((id term-counter-id)
               (extra (if id (format " id=\"%s\"" id) "")))
          (concat
           (format "<li%s%s>" class extra)
           (when headline (concat headline br)))))
       (`descriptive
        (let* ((term term-counter-id))
          (setq term (or term "(no term)"))
          ;; Check-boxes in descriptive lists are associated to tag.
          (concat (format "<dt%s>%s</dt>"
                          class (concat checkbox term))
                  "<dd><p>"))))
     (unless (eq type 'descriptive) checkbox)
     extra-newline
     (and (org-string-nw-p contents) (org-trim contents))
     extra-newline
     (pcase type
       (`ordered "</li>")
       (`unordered "</li>")
       (`descriptive "</p></dd>")))))

(advice-add 'org-html-format-list-item :override 'ld-org-custom-html-format-list-item)

(defun ld-org-export-publish()
  "Publish org and then generate sitemap.xml file."
  (interactive)
  ;; import project settings
  (require 'ld-org-publish-project-desc)
  ;; publish site
  (org-publish-all)
  ;; create sitemap for search engine
  (let (
        ;; FULL PATH to doc root. MUST end with a slash.
        (ld-site-pub-path-article-root "~/Public/ld_org_article_publish/article/")
        ;; file name of sitemap file, relative to webroot.
        ;; file name format: <sitemap-file-name>.xml
        (ld-site-sitemap-file-name "sitemap.xml")
        ;; site domain name
        (ld-site-domain-name "lishouzhong.com")
        ;; gzip it or not. t for true, nil for false.
        (ld-site-sitemap-gzip-it-p nil))

    (print (concat "begin: " (format-time-string "%Y-%m-%dT%T")))

    ;; rename file to backup ~ if already exist
    (let (f1 f2)
      (setq f1 (concat ld-site-pub-path-article-root ld-site-sitemap-file-name))
      (setq f2 (concat f1 ".gz"))
      (when (file-exists-p f1)
        (rename-file f1 (concat f1 "~") t))
      (when (file-exists-p f2)
        (rename-file f2 (concat f2 "~") t)))

    ;; create sitemap buffer
    (let (article-file-path sitemap-buffer)
      (setq article-file-path
            (concat ld-site-pub-path-article-root ld-site-sitemap-file-name))
      (setq sitemap-buffer (find-file article-file-path))
      (erase-buffer)
      (set-buffer-file-coding-system 'unix)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">")

      (require 'find-lisp)

      (let ((process-sitemap-content
             (lambda (article-file-path dest-buffer)
               (when (not (string-match "/zzz" article-file-path)) ; dir/file starting with zzz are not public
                 (with-temp-buffer
                   (insert-file-contents article-file-path nil nil nil t)
                   (goto-char 1)
                   (when (not (search-forward "<meta http-equiv=\"refresh\"" nil "noerror"))
                     (with-current-buffer dest-buffer
                       (insert "<url><loc>")
                       (insert (concat
                                "http://"
                                ld-site-domain-name
                                "/"
                                (substring article-file-path
                                           (length (expand-file-name ld-site-pub-path-article-root)))))
                       (insert "</loc></url>\n"))))))))
        (mapc
         (lambda (x) (funcall process-sitemap-content x sitemap-buffer))
         (find-lisp-find-files ld-site-pub-path-article-root "\\.html$")))

      (insert "</urlset>")

      (save-buffer)

      (when ld-site-sitemap-gzip-it-p
        (shell-command (concat "gzip " article-file-path))))

    (print (concat "finished: " (format-time-string "%Y-%m-%dT%T")))))

(provide 'init-org)

;;; init-org.el ends here
