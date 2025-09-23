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


;; Optimize Garbage Collection for Startup
(setq gc-cons-threshold most-positive-fixnum)

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)


;; Display startup time  
(defun start/display-startup-time ()
  (let* ((secs (float-time (time-subtract after-init-time before-init-time)))
         (ms (* 1000 secs)))
    (message "Emacs loaded in %d ms with %d garbage collections."
             (round ms) gcs-done)))

(add-hook 'emacs-startup-hook #'start/display-startup-time)


;;
;; Pacakge Settings
;;

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

;; For blazingly fast startup times, this line makes startup miles faster
(setq package-quickstart t) 

;; Gathering Statistics (from use-package)
;; (setq use-package-compute-statistics t) ; M-x use-package-report



;;
;; Configure Load Path
;;

; Most of the actual config is in sections in "lisp/"
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'straight-bootstrap)
(require 'init-font)


;; 
;; Basic Configuration
;;

;; Remove some unnecessary GUI elements
(menu-bar-mode -1)         ; Disable the menu bar
(scroll-bar-mode -1)       ; Disable the scroll bar
(tool-bar-mode -1)         ; Disable the tool bar


;; Better defaults
(setq inhibit-startup-screen t)        ; Disable welcome screen
(setq make-backup-files nil)           ; Stop creating ~ backup files
(setq auto-save-default nil)           ; Stop creating # auto save files
(setq delete-by-moving-to-trash t)     ; Deleting files go to OS's trash folder
(setq set-mark-command-repeat-pop t)   ; Repeating C-SPC after popping mark pops it again

;; (setq-default major-mode 'text-mode)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)


;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Changing the location of the "custom file"
;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; Basic modes
(show-paren-mode 1)             ; Enable show paren mode
(electric-indent-mode -1)       ; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)          ; Turns on automatic parens pairing
(delete-selection-mode 1)       ; Select text and delete it by typing.
(recentf-mode 1)                ; Remembering recently edited files

(global-auto-revert-mode 1)                    ; Automatically reload file and show changes if the file has changed
(setq global-auto-revert-non-file-buffers t)   ; Revert Dired and other buffers

(global-visual-line-mode 1)                    ; Enable truncated lines
(global-display-line-numbers-mode 1)           ; Display line numbers
(setq display-line-numbers-type 'relative)     ; Relative line numbers

;; Remembering the last place you visited in a file
;; When you enable this mode, you will be able to use `M-n' and `M-p' key in 
;; almost every minibuffer (and shell) prompt to call up the inputs you used
;; previously for the current command
(savehist-mode 1)
(setq history-length 25)

;; Emacs 25+, use `save-place-mode' to remember the last place you visited in a file. 
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))

(blink-cursor-mode -1)          ; Don't blink cursor


(use-package emacs
  :custom  
  
  ;; Remove some unnecessary GUI elements 
  ;;(menu-bar-mode nil)         ; Disable the menu bar
  ;;(scroll-bar-mode nil)       ; Disable the scroll bar
  ;;(tool-bar-mode nil)         ; Disable the tool bar
  ;;(inhibit-startup-screen t)  ; Disable welcome screen

  ;;(delete-selection-mode t)   ; Select text and delete it by typing.
  ;;(electric-indent-mode nil)  ; Turn off the weird indenting that Emacs does by default.
  ;;(electric-pair-mode t)      ; Turns on automatic parens pairing

  ;;(blink-cursor-mode nil)     ; Don't blink cursor
  ;;(global-auto-revert-mode t) ; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ; Dired don't create new buffer
  ;;(recentf-mode t) ; Enable recent file mode

  ;;(global-visual-line-mode t)            ; Enable truncated lines
  ;;(display-line-numbers-type 'relative)  ; Relative line numbers
  ;;(global-display-line-numbers-mode t)   ; Display line numbers

  (mouse-wheel-progressive-speed nil) ; Disable progressive speed when scrolling
  (scroll-conservatively 10) ; Smooth scrolling
  ;;(scroll-margin 8)

  ;;(make-backup-files nil) ; Stop creating ~ backup files
  ;;(auto-save-default nil) ; Stop creating # auto save files

  ;;(tab-width 4)
  
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally


  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  ;;(setq custom-file (locate-user-emacs-file "custom-vars.el"))
  ;;(load custom-file 'noerror 'nomessage)


  :bind (([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
        ;; Zooming In/Out
        ("C-+" . text-scale-increase)
        ("C--" . text-scale-decrease))
)



;; https://www.vernon-grant.com/Emacs/tmux-emacs-and-the-system-clipboard-on-wayland.html
;; Copy and Paste in Wayland



;; Themes
;;(straight-use-package 'moe-theme)
;;(load-theme 'moe-dark t)
(straight-use-package 'modus-themes)
(load-theme 'modus-vivendi t)


;; Common Lisp
(straight-use-package 'slime)
(setq inferior-lisp-program "sbcl")




;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)


;; Make gc pauses faster by decreasing the threshold.
;;(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
