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
      '(switch-window
        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
        (:name zenburn-theme :type http
               :url "https://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el")))

(el-get)

;; Local packages
;; --------------

(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "site-packages/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Initialization
;; --------------

(set-frame-font "Menlo-11")
(require 'zenburn-theme)

;; Show column number
(column-number-mode)

;; Set up org-mode latex export
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

;; To specify a specific LaTeX class, add the following to the top of your org file:
;;   #LaTeX_CLASS: <class>
;; Where <class> can be one of: article, koma-article.
;; Some additional options that you may want to include:
;;   #+LaTeX_CLASS_OPTIONS: [a4paper, twoside, twocolumn]
;; And for koma-script, to type-set table captions properly:
;;   #+LaTeX_CLASS_OPTIONS: [captions=tableheading]

(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))
(add-to-list 'org-export-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
