;;; utils/common/init-const.el --- Common constants -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provide some general-purpose utility constants with no external dependencies.

;;; Code:


;; -----------------------------------------------------------------------------
;; * System info
;; -----------------------------------------------------------------------------
;; Some constants related to system information.

(defconst sys/linux-p
  (eq system-type 'gnu/linux)
  "Are you running on a GNU/Linux device?")

(defconst sys/win32-p
  (eq system-type 'windows-nt)
  "Are you running on a Windows device?")

(defconst sys/macos-p
  (eq system-type 'darwin)
  "Are you running on a MacOS device?")

(defconst sys/android-p
  (eq system-type 'android)
  "Are you running on a Android device?")

(defconst sys/linux-x-p
  (and (eq window-system 'x) sys/linux-p)
  "Are you running under X11 on a GNU/Linux device?")

(defconst sys/linux-pgtk-p
  (and (eq window-system 'pgtk) sys/linux-p)
  "Are you running under pure GTK on GNU/Linux device?")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-const)
;;; init-const.el ends here 
