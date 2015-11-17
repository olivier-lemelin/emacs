(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Personal information
;; --------------------
(setq user-full-name "Olivier Lemelin"
      user-mail-address "lemelin.olivier@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES AND SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list the packages you want
(setq package-list '(magit evil elscreen cedet switch-window ecb company company-c-headers cmake-mode helm yasnippet helm-gtags clean-aindent-mode ws-butler smartparens sr-speedbar monokai-theme slime skewer-mode web-mode flycheck tabbar projectile smex sublimity undo-tree aggressive-indent flx-ido irony company-irony emmet-mode rainbow-mode expand-region ox-ioslide auctex simple-mpc doremi doremi-cmd doremi-frm doremi-mac nyan-mode hydra ace-window transpose-frame sunshine dtrt-indent auto-complete org-gcal lua-mode auctex avy neotree password-store git-timemachine w3m wanderlust twittering-mode icicles nasm-mode emms notmuch smtpmail elfeed spaceline auctex slime go-mode smooth-scrolling helm-projectile diminish helm-ag))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We want colored output in ESHELL.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Remove the large bar on top of the screen.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Have the cursor be a bar instead of a cursor.
(setq-default cursor-type 'bar)

;; Loads the awesome Monokai theme!
(load-theme 'monokai t)

;; Overwrite region when pasting over selected text.
(delete-selection-mode 1)

;; Set font size
(set-face-attribute 'default nil :height 100)

;; Nya nya nya - nya nya nya ...
(nyan-mode)

;; Display colors over the HTML colors
(rainbow-mode)

;; Rainbow delimiters - Color parentheses according to depth
(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)

;; skip splash buffer
(setq inhibit-splash-screen t)

;; Display column number.
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Complete anything
;; -----------------
(setq company-idle-delay 0.4)
(add-hook 'after-init-hook 'global-company-mode)

;; Y or N
;; ------
(fset 'yes-or-no-p 'y-or-n-p)

;; Save the history accross reboots
;; --------------------------------
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Backup files should be stored centrally
;; ---------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Onscreen - Multiple screens
;; ---------------------------
(elscreen-start)

;; Restoration
;; -----------

;; http://stackoverflow.com/questions/803812/emacs-reopen-buffers-from-last-session-on-startup
(defvar emacs-configuration-directory
    "~/.emacs.d/"
    "The directory where the emacs configuration files are stored.")
(defvar elscreen-tab-configuration-store-filename
    (concat emacs-configuration-directory ".elscreen")
    "The file where the elscreen tab configuration is stored.")

(defun elscreen-store ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save emacs-configuration-directory)
        (with-temp-file elscreen-tab-configuration-store-filename
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

(push #'elscreen-store kill-emacs-hook)

(defun elscreen-restore ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                          (insert-file-contents elscreen-tab-configuration-store-filename)
                          (buffer-string))))))
            (while screens
                (setq screen (car (car screens)))
                (setq buffers (split-string (cdr (car screens)) ":"))
                (if (eq screen 0)
                    (switch-to-buffer (car buffers))
                    (elscreen-find-and-goto-by-buffer (car buffers) t t))
                (while (cdr buffers)
                    (switch-to-buffer-other-window (car (cdr buffers)))
                    (setq buffers (cdr buffers)))
                (setq screens (cdr screens))))))

(elscreen-restore)

;; HELM
;; ----
(require 'helm-config)
(helm-mode 1)

(helm-autoresize-mode t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

;; Projectile
;; ----------
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Icicles
;; -------
(icy-mode 1)

;; AVY - Quick navigation
;; ----------------------
(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;; Line wrap globally
(global-visual-line-mode 1)

;; Navigate the Windows
;; --------------------
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; Neotree
;; -------
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Desktop save mode
;; -----------------
(desktop-save-mode 1)
(setq desktop-restore-eager 10)
(setq desktop-auto-save-timeout 300)
(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|KILL\\)")

;; Make buffer names unique
;; ------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Git integration
;; ---------------
(global-git-gutter-mode +1)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Undo tree mode
(global-undo-tree-mode)

;; M-x enhancement
;; ---------------
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Fuzzy matching
;; --------------
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Evil
;; ----
(require 'evil)
(evil-mode 1)
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Dired integration
;; -----------------
(setq dired-dwim-target t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Password Manager
;; ----------------
(setq password-store-password-length 24)

;;----------------------------------------------------------
;; ---- BEGIN Email client ----
;;----------------------------------------------------------
(require 'smtpmail)

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; default
(setq mu4e-maildir "~/Mail")

(setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
(setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
(setq mu4e-trash-folder   "/Gmail/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/Sent Mail"   . ?s)
	 ("/Trash"       . ?t)
	 ("/All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "lemelin.olivier@gmail.com"
 user-full-name  "Olivier Lemelin"
 mu4e-compose-signature
 (concat
  "Olivier Lemelin\n"
  "http://olivierlemelin.ca\n"))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;;----------------------------------------------------------
;; ---- END Email client ----
;;----------------------------------------------------------

;; Web browsing
;; ------------

(require 'w3m)
(setq w3m-use-cookies t)

;; Twitter
;; -------

(require 'twittering-mode)
(setq twittering-use-master-password t)

;; Multimedia System
;; -----------------

(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; Expand region with simple selecting
;; -----------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'hydra)

(sauron-start-hidden)

(setq sunshine-location "Montreal, Quebec")
(setq sunshine-show-icons t)
(setq sunshine-units 'metric)

;; Keyboard
;; --------

(global-set-key [S-dead-grave] "`")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smooth scrolling
;; ----------------
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

;; Delete whitespace on save
;; -------------------------
(setq-default indent-tabs-mode t)
(setq tab-width 4) ; or any other preferred value
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(yas-initialize)

;; Checks error on the fly
(require 'flycheck)

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

;; Markdown
;; --------

(add-to-list 'load-path "~/.emacs.d/markdown-mode/")

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Web Development
;; ---------------

;; Web mode!
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))

;; Live Editing
(skewer-setup)

;; Flycheck: Checks errors on the fly (requires npm -g install jshint)
(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; Auto-complete mode
(add-hook 'js2-mode-hook 'auto-complete-mode)

;; Emmet mode (Zen coding)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; To generate slides in Org-Mode
;; ------------------------------

(require 'ox-ioslide)

;; LISP
;; ----

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
(diminish 'yas/minor-mode)

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
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   (quote
    ("196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/agenda/main.org")))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
