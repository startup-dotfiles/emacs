;;; init-ui.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Nerd Font Icons
(use-package nerd-icons

  :init 
  ;; The Nerd Font you want to use in GUI
  ;; Make sure you have at least one Nerd Font installed on your system,
  ;; or run `M-x nerd-icons-install-fonts' to install the "Symbols Nerd Font Mono" (default) for you.
  ;; NOTE: Windows users still need to install it manually.
  (setopt nerd-icons-font-family 
          "JetBrainsMono Nerd Font") ;; TODO: Fix the hardcoding issue   
  
  :functions utils/font-available-p
  :config
  ;; Install nerd fonts automatically only in GUI
  ;; For macOS, may install via "brew install font-symbols-only-nerd-font"
  (when (and (display-graphic-p)
             (not (utils/font-available-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))

;; A minor-mode menu for mode-line
;(use-package minions
;  :hook (after-init . minions-mode))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-ui)
;;; init-ui.el ends here
