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


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)


;; 1. Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; 2. Disable `package' in favor of `straight' or `elpaca'.
(setq package-enable-at-startup nil)


(require 'use-package-ensure)      ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed


;; (setq file-name-handler-alist nil)


;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
;; (setq load-prefer-newer t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)


;; Redirect cache directories (for example, `eln-cache', `elpa') to the `cache' directory.
(defvar cache-dir (expand-file-name "cache/" user-emacs-directory))
(unless (file-directory-p cache-dir)
  (make-directory cache-dir t))

(startup-redirect-eln-cache (expand-file-name "eln-cache" cache-dir))                    ; eln-cache/
(setq package-user-dir (expand-file-name "elpa" cache-dir))                              ; elpa/
(setq url-configuration-directory (expand-file-name "url" cache-dir))                    ; url/
(setq transient-history-file (expand-file-name "transient/history.el" cache-dir))        ; transient/
(setq recentf-save-file (expand-file-name "recentf" cache-dir))                          ; recentf
(setq savehist-file (expand-file-name "history" cache-dir))                              ; history
(setq save-place-file (expand-file-name "places" cache-dir))                             ; places


;; Resizing the Emacs frame can be an expensive part of changing the
;; font. Inhibit this to reduce startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)



(provide 'early-init)

;;; early-init.el ends here
