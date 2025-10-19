;;; keep-home-clean.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setopt make-backup-files nil)    ; Stop creating  ~ backup files    (~xxx)
(setopt auto-save-default nil)    ; Stop creating ## auto save files (#xxx#)
(setopt create-lockfiles nil)     ; Stop creating .# lock files      (.#xxx)

;; Keep ~/.config/emacs clean
(skyz-emacs/use-package no-littering
  :ensure t
  :straight t
  :elpaca (:wait t) ;;!compat: block 

  ; :demand t
  :init (eval-and-compile
          (setq no-littering-etc-directory 
                (or skyz-emacs/etc-directory
                    (expand-file-name "etc/" user-emacs-directory)))
          (setq no-littering-var-directory
                (or skyz-emacs/var-directory
                    (expand-file-name "cache/" user-emacs-directory)))))

;; Wait to every queued elpaca order to finish
;(elpaca-wait)



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


;; TEMP: Remove soon
;; Benchmark your Emacs initialization
;(skyz-emacs/use-package benchmark-init
;  :ensure t
;  :straight t
;  :elpaca (:wait t)
;
;  :init (benchmark-init/activate)
;  ;; To disable collection of benchmark data after init is done.
;  :hook (after-init . benchmark-init/deactivate))



(provide 'keep-home-clean)

;;; keep-home-clean.el ends here
