
;; General programming
;; -------------------

;; Smart parenthesis
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Smart indentation
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(add-hook 'prog-mode-hook 'dtrt-indent-mode)

;; Yasnippets Templating system
(require 'yasnippet)
(yas-global-mode 1)

;(add-to-list 'yas-root-directory "~/.emacs.d/snippets/")
(yas-initialize)

;; Checks error on the fly
(require 'flycheck)

;; Helm and gtags
(require 'helm)
(require 'helm-config)
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Semantic mode
(add-hook 'prog-mode-hook 'semantic-mode)

;; Projectile mode: Adding project file completion
;; -----------------------------------------------

(projectile-global-mode)

; Always cache data
(setq projectile-enable-caching t)

; Invalidate remote files cache once in a while.
(setq projectile-file-exists-remote-cache-expire (* 10 60))


;; Indentation
;; -----------

 ; automatically indent when press RET
(global-set-key (kbd "RET") 'newline-and-indent) 

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

; Force indentation to follow perfectly
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'js-mode-hook #'aggressive-indent-mode)
(add-hook 'c-mode-hook #'aggressive-indent-mode)
(global-aggressive-indent-mode 1)



