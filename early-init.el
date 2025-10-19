;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.


;;; Code:

;; -----------------------------------------------------------------------------
;; * Load Paths
;; -----------------------------------------------------------------------------
;; Load the paths to skyz-emacs lisp files.
(load (concat (file-name-directory load-file-name) "lisp/skyz-load-paths")
      nil (not init-file-debug))

;; ---------------------------------------------------------------------------
;; * Early Startup Optimizations
;; ---------------------------------------------------------------------------
;; Temporarily raise the GC threshold to reduce garbage collections during
;; startup, then restore it to a normal value after startup to avoid long-term
;; high memory usage.

;; Tweaking the garbage collector
;; Reduce the number of times the garbage collector will run during the startup process.
;; Defer garbage collection further back in the startup process.
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
                      file-name-handler-alist 
                      (delete-dups (append file-name-handler-alist
                                           default-file-name-handler-alist))
                      vc-handled-backends default-vc-handled-backends)
                (garbage-collect)) 101)))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024)) ;; 64kb

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes redisplay faster, but might produce incorrect reordering
;; of bidirectional text with embedded parentheses (and other bracket characters
;; whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setopt cursor-in-non-selected-windows nil
        highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setopt fast-but-imprecise-scrolling t)

;; This inhibits fontification while receiving input, which should help a little with
;; scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be an expensive part of changing the font.
;; Inhibit this to reduce startup times with fonts that are larger than the system default.
(setopt frame-inhibit-implied-resize t
        frame-resize-pixelwise t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; ---------------------------------------------------------------------------
;; * UI elements & features
;; ---------------------------------------------------------------------------
;; Disable and suppress certain UI elements and features before UI initialization
;; to speed up startup and avoid flicker.

;; Remove some unnecessary GUI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; [COMPAT] Old-style syntax
;; Prefer using setopt to set user options (Emacs 29.1+)
(setopt menu-bar-mode nil)              ; Disable the menu bar               
(setopt tool-bar-mode nil)              ; Disable the tool bar               
(setopt scroll-bar-mode nil)            ; Disable the scroll bar (vertical)  
(setopt horizontal-scroll-bar-mode nil) ; Disable the scroll bar (horizontal)
;(menu-bar-mode -1)               ;;!(COMPAT (<=29.1))
;(tool-bar-mode -1)               ;;!(COMPAT (<=29.1))
;(scroll-bar-mode -1)             ;;!(COMPAT (<=29.1))
;(horizontal-scroll-bar-mode -1)  ;;!(COMPAT (<=29.1))

;; Suppress GUI features
(setopt use-file-dialog nil)
(setopt use-dialog-box nil)
(setopt inhibit-startup-screen t   ; Inhibit startup screens and messages
        inhibit-startup-message t  ; Inhibit the startup message (alias)
        inhibit-splash-screen t)   ; Inhibit the startup screen  (alias)
(setopt inhibit-default-init t)    ; Inhibit loading `default' library     
;(setopt inhibit-startup-buffer-menu t)

;; ---------------------------------------------------------------------------
;; * Package Manager 
;; ---------------------------------------------------------------------------

;; `package.el' is the built-in package manager since Emacs 24.1.
;; Package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'. You must prevent `package.el' loading packages
;; prior to your init-file loading. Also, if you use third-party package managers
;; such as `straight.el' and `elpaca.el', disabling them is also necessary.
(setopt package-enable-at-startup nil)
(setopt package-archives nil
        package-pinned-packages nil)

;; Change `package.el's default storage location for third-party packages.
(setopt package-user-dir
        (expand-file-name "package/" skyz-emacs/var-directory))
(setopt package-gnupghome-dir
        (expand-file-name "gnupg/" package-user-dir))

;; `use-package' is a built-in macro since Emacs 29.1 for organizing package configuration,
;; not a package manager. It provides an :ensure keyword which, when non-nil, 
;; will try to install the package if it’s missing (by default using package.el).
;; To allow explicit control of the installation process later, the default is nil.
(setopt use-package-always-ensure nil) ;; -> :ensure nil
(setopt use-package-enable-imenu-support t) ;; M-x imenu

;; Speed up package loading and loads package only when needed.
(setopt package-quickstart t)
(setopt use-package-always-defer t)  ;; -> :defer t

;; ---------------------------------------------------------------------------
;; * Native Compilation
;; ---------------------------------------------------------------------------

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; Native compilation cache
;; When using Emacs 29+, the location of the native compilation cache can be
;; changed using a function.
;; https://github.com/emacscollective/no-littering#native-compilation-cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" skyz-emacs/var-directory))))


;; ---------------------------------------------------------------------------
;; * Misc
;; ---------------------------------------------------------------------------

;; Explicitly set the prefered coding systems to avoid annoying prompt from emacs
;; (especially on Microsoft Windows)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8)

;; This file is loaded at run-time before `user-init-file'.
;(setopt site-run-file nil)          ; Prevent loading site-start.el 

;; Prevent flash of unstyled mode line
(setq mode-line-format nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every `*.elc' file.
(setq load-prefer-newer noninteractive)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources t)

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; Debug 
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
	    debug-on-error t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(provide 'early-init)
;;; early-init.el ends here


;; Refs:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html#Find-Init
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;; `user-init-file' locate in
;; - $HOME/.emacs.el
;; - $HOME/.emacs
;; - $HOME/.emacs.d/init.el
;; - $HOME_CONFIG_PATH/emacs/init.el (XDG Spec)

;; `user-emacs-directory' locate in 
;; - $HOME/.emacs.d/
;; - $XDG_CONFIG_PATH/emacs/ (XDG Spec)

;; The run-time load order:
;; - `site-run-file'   -> `user-init-file' -> default.el
;; - `early-init-file' -> `user-init-file' 

;; 0. `site-run-file', default.el, $XDG_CONFIG_PATH/emacs/*
;; 1. ~/.emacs.d/early-init.el,  $XDG_CONFIG_PATH/emacs/early-init.el
;; 2. .emacs, .emacs.el, ~/.emacs.d/init.el, $XDG_CONFIG_PATH/emacs/init.el



;; UI -- Startup

; fancy-splash-image

; frame-title-format
; icon-title-format 

; idle-update-delay  ; obsoleted since 30.1
; which-func-update-delay

;; UI -- scrolling

; fast-but-imprecise-scrolling
; redisplay-skip-fontification-on-input

;; UI -- frames & windows

; cursor-in-non-selected-windows
; highlight-nonselected-windows

;; UI -- fonts

; inhibit-compacting-font-caches
