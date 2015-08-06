(require 'package)

(load "~/.emacs.d/core.el")
(load "~/.emacs.d/style.el")
(load "~/.emacs.d/keyboard.el")
(load "~/.emacs.d/generalprogramming.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/cplusplus.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/latex.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Customizing the package
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom functions
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "mode-ecriture")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/agenda/main.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
