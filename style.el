
;; Style and Editing
;; -----------------

;; Remove the large bar on top of the screen.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Have the cursor be a bar instead of a cursor.
(setq-default cursor-type 'bar)

;; Activates the line numbers
(global-linum-mode 1)

;; Loads the awesome Monokai theme!
(load-theme 'monokai t)

;; Overwrite region when pasting over selected text.
(delete-selection-mode 1)

;; Navigate files quickly
(sr-speedbar-open)

;; Tabs in Emacs
(tabbar-mode)

;; Undo tree mode
(undo-tree-mode 1)

;; Nya nya nya - nya nya nya ...
(nyan-mode)

;; Display colors over the HTML colors
(rainbow-mode)

;; Saves the last used buffers.
;(desktop-save-mode 1)
;(setq desktop-restore-eager 10)

;; (defun rrix/desktop-save ()
;;       "Write the desktop save file to ~/.emacs.d"
;;       (desktop-save (concat (getenv "HOME")
;;                             "/.emacs.d/")))
;; (if (not (boundp 'rrix/desktop-save-timer))
;;     (setq rrix/desktop-save-timer
;;           (run-with-idle-timer 300 t 'rrix/desktop-save)))

;; Skip splash buffer
(setq inhibit-splash-screen t)

;; Use evil
;(require 'evil)
;(evil-mode 1)
;(setq evil-emacs-state-cursor '("red" box))
;(setq evil-normal-state-cursor '("green" box))
;(setq evil-visual-state-cursor '("orange" box))
;(setq evil-insert-state-cursor '("red" bar))
;(setq evil-replace-state-cursor '("red" bar))
;(setq evil-operator-state-cursor '("red" hollow))

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

(require 'hydra)

(winner-mode)
(require 'hydra-examples)
(require 'transpose-frame)
(defhydra hydra-window (global-map "C-x w" :color red :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _a_ce  _u_ndo  _r_edo
Switch: _p_revious _n_ext buffer"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
  ("t" transpose-frame "'")
  ("u" winner-undo)
  ("r" winner-redo)
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ("i" ace-maximize-window "ace-one" :color blue)
  ("b" ido-switch-buffer "buf")
  ("p" previous-buffer)
  ("n" next-buffer))

(defhydra hydra-zoom (global-map "C-c z")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(sauron-start-hidden)

(setq sunshine-location "Montreal, Quebec")
(setq sunshine-show-icons t)
(setq sunshine-units 'metric)
