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



;;; Set public API
;; Refs:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html
;; Posts:
;; - https://macowners.club/posts/setq-vs-customize-set-variable/
;; - https://emacsredux.com/blog/2025/04/06/goodbye-setq-hello-setopt/
;; - https://emacs-china.org/t/user-option-setq-setopt-custom-set-default-customize-set-value-customize-set-variable/26000/4

; (setq         [symbol value]...)                                           before 1.12
; (setq-default [variable value]...)                                         before 18
; (set           symbol newval) -> return newval                             before 21.1
; (set-default  [symbol value]...)                                           before 18
; (setq-local   [variable value]...) (buffer-local)                          before 24.3
; (setopt       [variable value]...)                                         before 29.1


;;; Customize Commands (autoload)
;; You can set user options in the Customize interface or run the
;; customize-* commands directly.

; (customize) -> Enter Customize interface
; (customize-saved) -> Enter Customize inferface (all saved options and faces)
; (customize-set-variable  variable value &optional comment) -> return value  before 21.1
; (customize-set-value     variable value &optional comment) -> return value  before 21.1
; (customize-save-variable variable value &optional comment) -> return value
; ...


;; Applying Customizations

; (custom-set-variables &rest args) (used by `custom-file')                  before 21.1
; (custom-set-faces     &rest args) (used by `custom-file')
; (custom-theme-set-variables theme &rest args)
; (custom-theme-set-faces     theme &rest args)

; (custom-set-variables &rest args) === (custom-theme-set-variables 'user &rest args)
; (custom-set-faces     &rest args) === (custom-theme-set-faces     'user &rest args)

; (custom-set-default  variable value) (`defcustom' :set default function)  from custom.el


;; ----------------------------------------------------------- ;;
;;       Functions        |      Customize interface state     ;;
;; ----------------------------------------------------------- ;;
;; setq                   |      CHANGED outside Customize     ;;
;; set-default            |      CHANGED outside Customize     ;;
;; setopt                 |      CHANGED outside Customize     ;;
;; customize-set-variable |     SET for current session only   ;;
;; ----------------------------------------------------------- ;;


;; (user-pacakge xx
;;    :custom -> use `customize-set-variable' under the hood)
;; (defcustom xx
;;    :set    -> use `custom-set-default' under the hood)
