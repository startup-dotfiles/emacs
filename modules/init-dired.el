;;; init-dired.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Refs: 
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html

;;; Code:


;; Basic dired-mode config
(use-package dired
  :straight (:type built-in)
  :ensure nil

  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)
  
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  
  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first"))


;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :after dired
  :bind (:map dired-mode-map
         ("S" . hydra-dired-quick-sort/body)))


;; Show git info in dired
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))


;; Colorful dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))


;; Shows icons in dired
(use-package nerd-icons-dired
  :after dired
  :diminish
  :hook (dired-mode . nerd-icons-dired-mode))


;; Allow rsync from dired buffers
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))



(provide 'init-dired)

;;; init-dired.el ends here
