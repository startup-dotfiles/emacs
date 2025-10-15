;;; init.el --- Initialization File -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;;
;; This file is loaded after the package system and GUI is initialized.
;;
;; Refs:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html

;;; Code:

;; -----------------------------------------------------------------------------
;; * Startup optimization
;; -----------------------------------------------------------------------------
;; Increase garbage collection threshold to speed up startup.
(setq gc-cons-threshold most-positive-fixnum)
;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

;; -----------------------------------------------------------------------------
;; * Load better defaults and customization
;; -----------------------------------------------------------------------------
;; Load skyz-emacs better defaults and custom settings.
(require 'skyz-better-defaults)
(require 'skyz-custom-settings)

;; -----------------------------------------------------------------------------
;; * Load skyz-emacs' core libs 
;; -----------------------------------------------------------------------------
;; Load skyz-emacs core and configuration modules.
(require 'skyz-core)
(skyz-emacs/load-package-manager)




















;;; Packages phase

;; Gathering Statistics (from use-package)
(setopt use-package-compute-statistics t) ; M-x use-package-report

;; Prefer GNU sources and stable versions before development versions from MELPA.
(setopt package-archive-priorities
        '(("gnu"    . 99)   ; prefer GNU packages
          ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
          ("stable" . 70)   ; prefer "released" versions from melpa
          ("melpa"  . 0)))  ; if all else fails, get it from melpa
 

;; Use the `no-littering' package to fix built-in and third-party package path variables 
(require 'keep-home-clean)

;; Install the packages listed in the `package-selected-packages' list.
(add-to-list 'package-selected-packages 'slime)

(when (eq 'built-in skyz-emacs/package-manager)
  (require 'pkg-list-ui)
  (require 'pkg-list-dired)
  (require 'pkg-list-org)
  (require 'pkg-list-eshell)
  (require 'pkg-list-completion))

;; (package-install-selected-packages :noconfirm)
(skyz-emacs/package-install-selected-packages)



;;; Configuration phase

(require 'init-base)

(require 'init-ui)
(require 'init-dired)
(require 'init-org)
(require 'init-eshell)
(require 'init-completion)

;;; Optional configuration

;; Themes
(skyz-emacs/load-theme)
;(load-theme 'modus-vivendi t)
;(load-theme 'modus-vivendi-deuteranopia t)
;(load-theme 'doom-one t)
;(load-theme 'moe-dark t)

;; Common Lisp
(setq inferior-lisp-program "sbcl")




(defun start/display-startup-time ()
  (let* ((secs (float-time (time-subtract after-init-time before-init-time)))
         (ms (* 1000 secs)))
    (message "Emacs loaded in %d ms with %d garbage collections."
             (round ms) gcs-done)))
(add-hook 'emacs-startup-hook #'start/display-startup-time)






;; Prevents `elpaca-after-init-hook` from running more than once.
;; https://github.com/progfolio/elpaca/wiki/Caveats-with-after-init-hook-and-emacs-startup-hook
(when (eq 'elpaca skyz-emacs/package-manager)
  (setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
  (elpaca-wait))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(provide 'init)
;;; init.el ends here
