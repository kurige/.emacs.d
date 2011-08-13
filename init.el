;;; init.el --- the primary initialization script

;; Common lisp goodies, loop
(require 'cl)

;; elpa packages
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

;; Detect if el-get is installed, and install it if it's not.
;; (Unfortunately this requires an internet connection at program start.)
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; Now either el-get is `require'd already, or has been `load'ed by the el-get
;; installer, so there's no need to do either ourselves.

;; Set local recipes.
(setq
el-get-sources
'((:name buffer-move         ; Have to add our own keys
         :after (lambda ()
                  (global-set-key (kbd "<C-S-up>")    'buf-move-up)
                  (global-set-key (kbd "<C-S-down>")  'buf-move-down)
                  (global-set-key (kbd "<C-S-left>")  'buf-move-left)
                  (global-set-key (kbd "<C-S-right>") 'buf-move-right)))
  
  (:name smex                ; A better (ido like) M-x
         :after (lambda ()
                  (setq smex-save-file "~/.emacs.d/.smex-items")
                  (global-set-key (kbd "M-x") 'smex)
                  (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
  
  ;;(:name magit               ; Git meets Emacs, and a binding
  ;;       :after (lambda ()
  ;;                (global-set-key (kbd "C-x C-z") 'magit-status)))
  
  (:name goto-last-change    ; Move pointer back to last change
         :after (lambda ()
                  ;; When using AZERTY keyboard, consider C-x C-_
                  (global-set-key (kbd "C-x C-/") 'goto-last-change)))
  
  (:name zenburn-theme :type http
       :url "https://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el")))

;; Aditional recipes
(setq
 my:el-get-packages
 '(el-get         ; el-get is self-hosting
   psvn           ; SVN mode
   escreen        ; Screen for emacs, "C-\ C-h"
   switch-window  ; Takes over "C-x o"
   auto-complete  ; Complete as you type, using overlays
   color-theme))  ; Nice looking emacs

;; Some recipes require external tools to be installed.
;; Note: el-get-install requires git, so we assume it's already available.

;; Disabled because `emacs-goodies-el seems to completely lock up
;;(when (el-get-executable-find "cvs")
;;  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; The debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn        ; M-x svn-status
                   yasnippet)  ; powerful snippet mode
        do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; Install new packages and init already installed packages.
(el-get 'sync my:el-get-packages)

;; Local packages
;; --------------

(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "site-packages/"))))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Initialization
;; --------------

;; Choose a good default font based on operating system.

;; The system-type variable:
;; Value is symbol indicating type of operating system you are using.
;; Special values:
;; `gnu'         compiled for a GNU Hurd system.
;; `gnu/linux'   compiled for a GNU/Linux system.
;; `darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;; `ms-dos'      compiled as an MS-DOS application.
;; `windows-nt'  compiled as a native W32 application.
;; `cygwin'      compiled using the Cygwin library.
;; Anything else indicates some sort of Unix system.

;; Visual settings...
;; ------------------

(setq inhibit-splace-screen t) ; No splash screen.
(tool-bar-mode -1)             ; No tool-bar with icons.
(scroll-bar-mode -1)           ; No scroll bars.
(line-number-mode 1)           ; Always show line numbers.
(column-number-mode 1)         ; Always show column numbers.

;; On mac, the menu is always drawn. This prevents it from being empty.
(unless (string-match "apple-darwin" system-configuration) (menu-bar-mode -1))

;; Choose a good font for the current system.
(set-face-font 'default (cond ((string= "windows-nt" system-type) "Consolas-10") ;; Windows font
                              ((string= "darwin" system-type) "Menlo-11")        ;; Mac font
                              (t "Monospace-10")))                               ;; Default font (TODO: Pick something way way better.)

(global-hl-line-mode)  ; highlight the current line
(global-linum-mode 1)  ; add line numbers on the left

;; Avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; Copy/Paste/Cut with C-c and C-v and C-x. (Also, C-RET.)
(cua-mode)

;; Mac specific key remapping and misc. settings.
;; Command -> Meta and Option -> nothing.
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, so that copy/paste works as expected.
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; "winner-mode" provides C-<left> to get back to previous window layout.
(winner-mode 1)

;; Whenever an external process changes a file under emacs, and there are no
;; unsaved changes in the corresponding buffer, just revert it's content to
;; reflect what's currently on disk.
(global-auto-revert-mode 1)

;; Use ido for minibuffer completion.
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)

;; Default key to switch buffer is C-x b, but that's not easy enough.
;; To kill emacs either close its frame from the window manager or do
;; M-x kill-emacs. Don't need a nice shortcut for a once-a-day action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-C") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; C-x C-j opens dired with the cursor right on the file you're editing.
(require 'dired-x)

;; F11 for full screen. (Currently only MacOSX.)
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; Use the beautiful zenburn theme.
(require 'zenburn-theme)

;; Set up org-mode latex export
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

;; Expand text based on text already in buffers using TAB
(global-set-key "\M- " 'hippie-expand)

;; Will allow you to type just "y" instead of "yes" when you exit
(fset 'yes-or-no-p 'y-or-n-p)

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
