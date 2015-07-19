
(defun set-prose-mode ()
  "Sets up the prose mode for the french language."
  (interactive)
  ; Word wrap
  (setq-default fill-column 80)
  (org-mode)
  ;Text Mode
  ;Flyspell
  (flyspell-mode)
  (visual-line-mode)
  (ispell-change-dictionary "francais"))  


