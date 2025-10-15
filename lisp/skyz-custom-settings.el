;;; skyz-custom-settings.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defgroup skyz-emacs nil
  "Skyz-emacs customization."
  :group 'emacs
  :link '(url-link :tag "Homepage" "https://github.com/range4-skyz/emacs"))

;;;; ---------------------------------------------------------------------------
;;;; * User Info settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/full-name user-full-name 
  "Set user full name."
  :group 'skyz-emacs
  :type 'string)
  

(defcustom skyz-emacs/mail-address user-mail-address
  "Set user mail address."
  :group 'skyz-emacs
  :type 'string)  


;;;; ---------------------------------------------------------------------------
;;;; * UI settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/logo (expand-file-name
                           (if (display-graphic-p) "logo.png" "banner.txt")
                           skyz-emacs/assets-directory)
  "Set skyz-emacs logo."
  :group 'skyz-emacs
  :type 'string)
  
;; TODO: skyz-emacs/font

;;;; ---------------------------------------------------------------------------
;;;; * Directory settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/org-directory (expand-file-name "~/org") 
  "Set org directory."
  :group 'skyz-emacs
  :type 'string)


;;;; ---------------------------------------------------------------------------
;;;; * Server settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/enable-sever t
  "Enable `server-mode' or not."
  :group 'skyz-emacs
  :type 'boolean)
  

(defcustom skyz-emacs/proxy "127.0.0.1:8888"
  "Set HTTP/HTTPS proxy"
  :group 'skyz-emacs
  :type 'string)
  

(defcustom skyz-emacs/socks-proxy "127.0.0.1:8888"
  "Set SOCKS proxy"
  :group 'skyz-emacs
  :type 'string)
  

;;;; ---------------------------------------------------------------------------
;;;; * Subgroups custom settings
;;;; ---------------------------------------------------------------------------

(require 'custom-packages "custom/packages")
(require 'custom-themes   "custom/themes")


;;;; ---------------------------------------------------------------------------
;;;; * Setup custom-file
;;;; ---------------------------------------------------------------------------

;; Changing the location of the "custom file".
;; Move customization variables to a separate file and load it, avoid filling
;; up `init.el' with unnecessary variables
(setq custom-file (locate-user-emacs-file "custom-settings.el"))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'skyz-custom-settings)
;;; skyz-custom-settings.el ends here

; (customize-set-variable variable value &optional comment)

; (custom-set-variables &rest args)
; (custom-set-faces     &rest args)

