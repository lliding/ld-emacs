(require 'cl-lib)

(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：add-to-list 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

;; get emacs version and operating system type
(defvar *emacs27* (>= emacs-major-version 27))
(defvar *is-a-mac* (eq system-type 'darwin))
(defvar *win64* (eq system-type 'windows-nt))
(defvar *cygwin* (eq system-type 'cygwin))
(defvar *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defvar *unix* (or *linux*
                   (eq system-type 'usg-unix-v)
                   (eq system-type 'berkeley-unix)))

(defvar ld-emacs-root-dir (file-truename "~/ld-emacs/site-lisp"))

(if *win64*
    (add-subdirs-to-load-path ld-emacs-root-dir))

(if *linux*
    (add-subdirs-to-load-path "/usr/share/emacs/ld"))

(require 'init)
