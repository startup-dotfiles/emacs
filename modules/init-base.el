;;; init-base.el --- Better default configurations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Save Place
;; Remembering the last place you visited in a file.
(skyz-emacs/use-package saveplace
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; Rest of use-package args...
  :hook (after-init . save-place-mode)
  :init (setopt save-place-limit 600))


;; Recent files
;; Remembering recently edited files.
(skyz-emacs/use-package recentf
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; Rest of use-package args...
  :hook (after-init . recentf-mode)
  :bind (("C-x C-r" . recentf-open-files))
  
  :init (setopt recentf-max-saved-items 300
                recentf-max-menu-items 15
                recentf-auto-cleanup 'mode
                recentf-exclude nil))


;; History
;; Presevers the minibuffer history.
(skyz-emacs/use-package savehist
  :ensure nil
  :straight (:type built-in)
  :elpaca nil ;;!compat

  ;; Rest of use-package args...
  :hook (after-init . savehist-mode)
  :init 
  (setopt enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 300
          savehist-save-minibuffer-history t
          savehist-autosave-interval 300)
  (setopt savehist-additional-variables
          '(kill-ring                                   ; clipboard
            register-alist                              ; macros
            mark-ring global-mark-ring                  ; marks
            search-ring regexp-search-ring              ; searches
            command-history extended-command-history))) ; commands




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-base)
;;; init-base.el ends here
