;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.
;;
;; More info:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Files.html


;;; Code:


;; 0. site-start.el, default.el, $XDG_CONFIG_PATH/emacs/*
;; 1. .emacs, .emacs.el, ~/.emacs.d/init.el, $XDG_CONFIG_PATH/emacs/init.el
;; 2. ~/.emacs.d/early-init.el, $XDG_CONFIG_PATH/emacs/early-init.el
(setq site-run-file nil)           ;; Prevent loading site-start.el 
(setq inhibit-default-init nil)    ;; Prevent loading default.el 



;; Tweaking the garbage collector
;; Reduce the number of times the garbage collector will run during the startup process.
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)


; Resetting garbage collection and file-name-handler values.
(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-file-name-handler-alist file-name-handler-alist)
        (default-vc-handled-backends vc-handled-backends))
    (setq file-name-handler-alist nil
          vc-handle-backends nil)
    ;; Recover handlers after startup
    (add-hook 'after-init-hook
              (lambda ()
                (setq gc-cons-threshold  134217728  ; 128mb
                      gc-cons-precentage 0.1     
                      file-name-handler-alist default-file-name-handler-alist
                      vc-handled-backends default-vc-handled-backends)
                (garbage-collect)) t) ))



;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)


;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; Disable `package' in favor of `straight' or `elpaca'.
(setq package-enable-at-startup nil
      package-archives nil
      package-initialize nil
      pacakge-quickstart nil)



;; Resizing the Emacs frame can be an expensive part of changing the
;; font. Inhibit this to reduce startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


;; Remove some unnecessary GUI elements (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; Remove some unnecessary GUI elements
(menu-bar-mode -1)                 ; Disable the menu bar
(tool-bar-mode -1)                 ; Disable the tool bar
(scroll-bar-mode -1)               ; Disable the scroll bar (vertical)
(horizontal-scroll-bar-mode -1)    ; Disable the scroll bar (horizontal)


;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-default-init t)


;; Prevent flash of unstyled mode line
(setq mode-line-format nil)


;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources t)


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every `*.elc' file.
(setq load-prefer-newer noninteractive)


;; Native compilation cache
;; When using Emacs 29+, the location of the native compilation cache can be changed using a function
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "cache/eln-cache/" user-emacs-directory))))


;; Debug 
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
	    debug-on-error t))



(provide 'early-init)

;;; early-init.el ends here
