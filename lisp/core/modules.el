;;; core/modules.el --- -*- lexical-binding: t -*-

;;; Commentary:

;; .
;; └─ modules/  
;;    ├─ emacs/             (modules-category)
;;    │  ├─ dired/          (modules-group)
;;    │  │  ├─ default.el
;;    │  │  ├─ config.el            
;;    │  │  ├─ packages.el
;;    │  │  └─ ...
;;    │  └─ ...
;;    ├─ editor/  
;;    ├─ terminal/
;;    └─ ...

;;; Code:

;;;; ---------------------------------------------------------------------------
;;;; * MODULE variables/constants
;;;; ---------------------------------------------------------------------------

(defvar skyz-emacs/modules-category-alist '()
  "List of string corresponding to categories.
 A category is a subdirectory of `skyz-emacs/modules-directory'.")

(defvar skyz-emacs/modules-group-alist '()
  "List of string corresponding to groups. 
A group is a subdirectory of `skyz-emacs/modules-category-alist'.")

;; Standard Module Files
;; When you load a module, it will attempt to automatically load these standard
;; files in a specific order and according to certain rules. You may continue to
;; add other configuration files, but they might need to be loaded manually.

(defvar skyz-emacs/module-init-file "init.el"
  "This file is loaded before all other module files and is used to
configure actions that must be performed early in the setup.")

(defvar skyz-emacs/module-default-file "default.el"
  "This file is used to configure and tweak the behavior and
related options of Emacs built-in packages.")

(defvar skyz-emacs/module-packages-file "packages.el"
  "The file is used to declare all the packages that the module will install.
It is usually required and is loaded after
 `skyz-emacs/module-default-file'.")

(defvar skyz-emacs/module-config-file "config.el"
  "The file is used to configure and load 3rd-party packages.
This file is usually required and is loaded after
`skyz-emacs/module-packages-file'.")

(defvar skyz-emacs/module-autoload-file "autoload.el"
  "The file is used to store that module’s functions, to be loaded when
they are used. It will use ;;;###autoload to mark them.")

(defvar skyz-emacs/module-keybinds-file "keybinds.el"
  "This file is used to configure the module's keyboard shortcuts.
It is usually not needed; you can configure directly in 
`skyz-emacs/module-config-file'.")


;;;; ---------------------------------------------------------------------------
;;;; * MODULE functions
;;;; ---------------------------------------------------------------------------

;; TODO:
;; - skyz-emacs/modules-scan ()
;; - skyz-emacs/modules-enable-module  (category group)
;; - skyz-emacs/modules-disable-module (category group)
;; - skyz-emacs/modules-module-enabled-p



;;;; ---------------------------------------------------------------------------
;;;; * MODULE commands
;;;; ---------------------------------------------------------------------------





















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-modules)
;;; modules.el ends here

; (require feature &optional filename noerror)
; (provide feature &optional subfeatures)

; (load file &optional noerror nomessage nosuffix must-suffix)
; (safe-load)
; (load-file file)

; after-load-alist
; (eval-after-load      file from)
; (with-eval-after-load file &rest body)
