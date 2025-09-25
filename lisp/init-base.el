;;; init-base.el --- Better default configurations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Save Place
(use-package saveplace
  :straight (:type built-in)
  
  :hook (after-init . save-place-mode))


;; Recent files
(use-package recentf
  :straight (:type built-in)

  :hook (after-init . recentf-mode)
  :bind (("C-x C-r" . recentf-open-files))
  
  :init (setq recentf-max-saved-items 300))


;; History
(use-package savehist
  :straight (:type built-in)
  
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))




(provide 'init-base)

;;; init-base.el ends here
