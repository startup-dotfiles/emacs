;;; init-base.el --- Better default configurations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Save Place
(skyz-emacs/use-package saveplace
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; rest of use-package args...
  :hook (after-init . save-place-mode)
  :init (setq save-place-limit 600))


;; Recent files
(skyz-emacs/use-package recentf
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; rest of use-package args...
  :hook (after-init . recentf-mode)
  :bind (("C-x C-r" . recentf-open-files))
  
  :init (setq recentf-max-saved-items 300
              recentf-max-menu-items 15
              recentf-auto-cleanup 'mode
              recentf-exclude nil))


;; History
(skyz-emacs/use-package savehist
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; rest of use-package args...
  :hook (after-init . savehist-mode)
  :init 
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 300
        savehist-save-minibuffer-history t
        savehist-autosave-interval 300)
  (setq savehist-additional-variables
      '(kill-ring                         ; clipboard
        register-alist                    ; macros
        mark-ring global-mark-ring        ; marks
        search-ring regexp-search-ring    ; searches
        extended-command-history)))



(provide 'init-base)

;;; init-base.el ends here
