;; early-init.el
;;
;; This file is loaded before the package system and GUI is initialized.
;; So customizations related to GUI features will not work reliably in early-init.el.
;;
;; More info:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Files.html

(setq package-enable-at-startup nil)

; Set package install location (from ELPA)
(setq package-user-dir "~/.local/share/emacs/elpa")
