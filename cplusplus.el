
;; C++
;; ---

;; Irony mode
;; ----------
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; EDE
(require 'ede)
(global-ede-mode)

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Company CLANG
; (setq company-backends (delete 'company-semantic company-backends))
; (define-key c-mode-map  [(tab)] 'company-complete)
; (define-key c++-mode-map  [(tab)] 'company-complete)

;; Sets a default C style
(setq
 c-default-style "linuxgnu" ;; set style to "linux"
 )
