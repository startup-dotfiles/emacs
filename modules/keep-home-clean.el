;;; keep-home-clean.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq make-backup-files nil)    ; Stop creating  ~ backup files    (~xxx)
(setq auto-save-default nil)    ; Stop creating ## auto save files (#xxx#)
(setq create-lockfiles nil)     ; Stop creating .# lock files      (.#xxx)


(skyz-emacs/use-package no-littering
  :ensure t
  :straight t
  :elpaca (:wait t) ;;!compat: block 
  ;:ensure (:wait t)

  ; :demand t
  :init (eval-and-compile
          (setq no-littering-etc-directory 
                (or skyz-emacs/etc-directory
                    (expand-file-name "etc/" user-emacs-directory)))
          (setq no-littering-var-directory
                (or skyz-emacs/var-directory
                    (expand-file-name "cache/" user-emacs-directory)))))

;;; Recent files
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))


;;; Backup files
(no-littering-theme-backups)


;;; Lock files
;; To put lock files into a single repository, you could use something like the following.
(let ((dir (no-littering-expand-var-file-name "lock-files/")))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t))))



(provide 'keep-home-clean)

;;; keep-home-clean.el ends here
