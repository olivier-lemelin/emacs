
;; Core.el
;; =======

;; Package initialization
;; ----------------------

(require 'package)

;; list the packages you want
(setq package-list '(magit evil cedet ecb company company-c-headers cmake-mode helm yasnippet helm-gtags clean-aindent-mode ws-butler smartparens sr-speedbar monokai-theme slime skewer-mode web-mode flycheck tabbar projectile smex sublimity undo-tree aggressive-indent flx-ido irony company-irony emmet-mode rainbow-mode expand-region ox-ioslide auctex simple-mpc ))

;; List the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Git integration
;; ---------------

; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; Bytecode compilation
;; --------------------

;(require 'find-lisp)

;(mapc
; (lambda (x) (byte-compile-file x))
; (find-lisp-find-files
;  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) default-directory))
;  "\\.el$"))
