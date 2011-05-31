;;; init.el --- the primary initialization script

;; ELPA packages
;; -------------

(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; el-get packages
;; ---------------

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(setq el-get-sources
      '(el-get switch-window yasnippet
       (:name magit
              :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
       (:name zenburn-theme :type http
              :url "https://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el")))

(el-get 'wait)

;; Local packages
;; --------------

(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "site-lisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Initialization
;; --------------

(set-frame-font "Menlo-11")
(require 'zenburn-theme)
