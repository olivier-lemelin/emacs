
;; Style and Editing
;; -----------------

;; Remove the large bar on top of the screen.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Have the cursor be a bar instead of a cursor.
(setq-default cursor-type 'bar)

;; Activates the line numbers
(linum-mode)

;; Loads the awesome Monokai theme!
(load-theme 'monokai t)

;; Overwrite region when pasting over selected text.
(delete-selection-mode 1)

;; Navigate files quickly
(sr-speedbar-open)

;; Tabs in Emacs
(tabbar-mode)

;; Undo tree mode
(undo-tree-mode)

;; Use spaces instead of tabs
;(setq-default indent-tabs-mode nil)

;; M-x enhancement
;; ---------------
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Sublimity for scrolling and minimap
;; -----------------------------------

;(require 'sublimity)
;(require 'sublimity-scroll)

;(sublimity-mode 1)

;; Fuzzy matching
;; --------------

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Expand region with simple selecting
;; -----------------------------------

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
