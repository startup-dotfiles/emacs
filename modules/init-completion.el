;;; init-completion.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Consulting completing-read
(use-package consult  
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  :config
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
)


;; VERTical Interactive COmpletion
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))


;; Emacs completion style that matches multiple regexps in any order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))


;; Enrich existing commands with completion annotations
(use-package marginalia
  :hook (after-init . marginalia-mode))


;; Add icons to completion candidates
(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))


;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :commands embark-prefix-help-command
  :bind (("s-."   . embark-act)
         ("C-s-." . embark-act)
         ("M-."   . embark-dwim)        ; overrides `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("M-." . my-embark-preview))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))


;; Consult integration for Embark
(use-package embark-consult
  :bind (:map minibuffer-mode-map
         ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; COmpletion in Region FUnction
;(use-package corfu
;  :autoload (corfu-quit)
;  :functions (persistent-scratch-save)
;  :custom
;  (corfu-auto t)
;  (corfu-auto-prefix 2)
;  (corfu-count 12)
;  (corfu-preview-current nil)
;  (corfu-on-exact-match nil)
;  (corfu-auto-delay 0.2)
;  (corfu-popupinfo-delay '(0.4 . 0.2))
;  (global-corfu-modes '((not erc-mode
;                             circe-mode
;                             help-mode
;                             gud-mode
;                             vterm-mode)
;                        t))
;  :custom-face
;  (corfu-border ((t (:inherit region :background unspecified))))
;  :bind ("M-/" . completion-at-point)
;  :hook ((after-init . global-corfu-mode)
;         (global-corfu-mode . corfu-popupinfo-mode)
;         (global-corfu-mode . corfu-history-mode))
;  :config
;  ;;Quit completion before saving
;  (add-hook 'before-save-hook #'corfu-quit)
;  (advice-add #'persistent-scratch-save :before #'corfu-quit))


;; Corfu popup on terminal
;(unless (display-graphic-p)
;  (use-package corfu-terminal
;    :hook (global-corfu-mode . corfu-terminal-mode)))


;; Completion At Point Extensions
(use-package cape 
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :commands (cape-file cape-elisp-block cape-keyword)  
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))





(provide 'init-completion)

;;; init-completion.el ends here
