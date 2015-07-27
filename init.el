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

