;;; init-ui.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Themes
(load-theme 'modus-vivendi t)


;; A minor-mode menu for mode-line
;(use-package minions
;  :hook (after-init . minions-mode))


;; Icons
(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :functions font-available-p
  :config
  ;; Install nerd fonts automatically only in GUI
  ;; For macOS, may install via "brew install font-symbols-only-nerd-font"
  (when (and (display-graphic-p)
             (not (font-available-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))


(provide 'init-ui)

;;; init-ui.el ends here
