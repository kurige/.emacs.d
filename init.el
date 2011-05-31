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

;; Attempt #1

; Use hunspell instead of ispell
; http://www.emacswiki.org/emacs/InteractiveSpell#toc6
;(setq ispell-program-name "/usr/local/bin/hunspell")
;(require 'rw-language-and-country-codes)
;(require 'rw-ispell)
;(require 'rw-hunspell)
;(setq ispell-dictionary "en_US")
;(custom-set-variables
; '(rw-hunspell-default-dictionary "en_US")
; '(rw-hunspell-dicpath-list (quote ("/Users/chris/.emacs.d/dict/")))
; '(rw-hunspell-make-dictionary-menu t)
; '(rw-hunspell-use-rw-ispell t)
;)

;; Attempt #2

;(eval-after-load "ispell"
;   (progn
;     (setq ispell-dictionary "american"
;           ispell-extra-args '("-a" "-i" "utf-8")
;           ispell-silently-savep t
;           ispell-dictionary-alist
;           '(("nil" ; "nil" default
;              "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-i" "utf-8") nil utf-8)
;             ("american" ; Yankee English
;              "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-i" "utf-8") nil utf-8)
;             ("british" ; British English
;              "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_GB" "-i" "utf-8") nil utf-8)
;             ("german"
;              "[a-zäöüßA-ZÄÖÜ]" "[^a-zäöüßA-ZÄÖÜ]" "[']" t ("-d" "de_DE" "-i" "utf-8") nil utf-8)
;             ("german8"
;              "[a-zäöüßA-ZÄÖÜ]" "[^a-zäöüßA-ZÄÖÜ]" "[']" t ("-d" "de_DE" "-i" "utf-8") nil utf-8)
;             ))))
;
;(setq-default ispell-program-name "/usr/local/bin/hunspell")
