;;; init.el --- Initialization File -*- no-byte-compile: t lexical-binding: t -*-

;;; Commentary:
;;
;; This file is loaded after the package system and GUI is initialized.
;;
;; More infos:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html

;;; Code:


;;
;; Speed up Startup Process
;;


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)


;;; Initial phase

;; Changing the location of the "custom file".
;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))


;; Add `lisp/' and `config/' to `load-path'. 
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))


;;; Packages phase

;; Gathering Statistics (from use-package)
(customize-set-variable 'use-package-compute-statistics t) ; M-x use-package-report

;; Sets default package repositories
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")        
       ("org" . "https://orgmode.org/elpa/")
       ("elpa" . "https://elpa.gnu.org/packages/")
       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Prefer GNU sources and stable versions before development versions from MELPA.
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it from melpa
 

;; `package.el' is the built-in package manager in Emacs 24.
;; However, if you prefer to use `straight.el' or `elpaca.el' to install and manage packages,
;; you can uncomment one of the lines below to enable it.
(require 'bootstrap-straight)
;; (require 'bootstrap-elpaca)

;; Use the `no-littering' package to fix built-in and third-party package path variables 
(require 'keep-home-clean)

;; Install the packages listed in the `package-selected-packages' list.
(add-to-list 'package-selected-packages 'modus-themes)
(add-to-list 'package-selected-packages 'slime)

(add-to-list 'package-selected-packages 'dired-quick-sort)
(add-to-list 'package-selected-packages 'dired-git-info)
(add-to-list 'package-selected-packages 'nerd-icons-dired)
(add-to-list 'package-selected-packages 'diredfl)
(add-to-list 'package-selected-packages 'dired-rsync)

(add-to-list 'package-selected-packages 'org-modern)

(add-to-list 'package-selected-packages 'esh-help)
(add-to-list 'package-selected-packages 'eshell-z)

(skyz-package-install-selected-packages)
;;(elpaca-wait)



;;; Configuration phase

(require 'skyz-defaults-config)
(require 'skyz-startup-config)

(require 'init-base)
(require 'init-font)
(require 'init-dired)
(require 'init-org)
(require 'init-eshell)


;;; Optional configuration


;; Themes
(load-theme 'modus-vivendi t)

;; Common Lisp
(setq inferior-lisp-program "sbcl")




(defun start/display-startup-time ()
  (let* ((secs (float-time (time-subtract after-init-time before-init-time)))
         (ms (* 1000 secs)))
    (message "Emacs loaded in %d ms with %d garbage collections."
             (round ms) gcs-done)))

(add-hook 'emacs-startup-hook #'start/display-startup-time)


;;; init.el ends here
