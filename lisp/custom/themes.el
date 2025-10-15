;;; custom/themes.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; TODO: skyz-emacs/theme
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-custom.el

(defgroup custom-themes nil
  "Configuration options related to themes for skyz-emacs."
  :group 'skyz-emacs)

;;;; ---------------------------------------------------------------------------
;;;; * Theme settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/theme-alist
  '((default     . modus-vivendi)
    (modus-dark  . modus-vivendi)
    (modus-light . modus-operandi)
    (doom-dark   . doom-one)
    (doom-light  . doom-one-light))
  "List of themes mapped to internal themes."
  :group 'custom-themes
  :type '(alist  :key-type (symbol :tag "Theme")
                 :value-type (symbol :tag "Internal theme")))

(defcustom skyz-emacs/auto-themes
  '(("8:00"  . modus-operandi)
    ("19:00" . modus-vivendi))
  "List of themes mapped to the time they should be loaded."
  :group 'custom-themes
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom skyz-emacs/system-themes
  '((dark  . modus-vivendi)
    (light . modus-operandi))
  "List of themes related the system appearance."
  :group 'custom-themes
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))
  
(defcustom skyz-emacs/theme 'default
  "The color theme."
  :group 'custom-themes
  :type `(choice
          (const :tag "auto-theme"   auto)   ; load from auto-themes
          (const :tag "system-theme" system) ; load from system-themes
          (const :tag "random-theme" random) ; load from themes-alist
          ,@(mapcar
             (lambda (item)
               (let ((name (car item)))
                 (list 'const
                       :tag (symbol-name name)
                       name)))
             skyz-emacs/theme-alist)))
                 









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'custom-themes)
;;; themes.el ends here
