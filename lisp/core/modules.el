;;; core/modules.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defvar skyz-emacs/modules-category-alist '()
  "List of string corresponding to categories. A category is
a subdirectory of `skyz-emacs/modules-directory'.")

(defvar skyz-emacs/modules-group-alist '()
  "List of string corresponding to groups. A group is
a subdirectory of `skyz-emacs/modules-category-alist'.")

(defvar skyz-emacs/modules-group-init-file "init.el"
  "The file for module early initialization config files.")

(defvar skyz-emacs/modules-group-config-file "config.el"
  "The file for module configuration files.")

(defvar skyz-emacs/modules-group-packages-file "packages.el"
  "The file for module package configuration files.")

(defvar skyz-emacs/modules-group-autoload-file "autoload.el"
  "The file for module autoload configuration files.")

(defvar skyz-emacs/modules-group-keybindings-file "keybindings.el"
  "The file for module keybindings configuration files.")





















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-modules)
;;; modules.el ends here
