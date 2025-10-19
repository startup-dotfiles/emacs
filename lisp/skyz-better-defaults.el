;;; skyz-better-defaults.el --- Better default configurations -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Allow for shorter responses: "y" for yes and "n" for no.
(setopt read-answer-short t)
(if (boundp 'use-short-answers)
    (setopt use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Clipboard (Wayland)
;; Support copying text from Emacs running in a terminal to the system clipboard.
;; Requirements: Emacs 29+, PGTK build, need to install `wl-clipboard'
;; https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

;; Fonts (Graphic)
;; The font displayed in terminal-based Emacs follows the font settings
;; of the terminal emulator you are using
(set-face-attribute 'default nil
                    ;:font "Sarasa Fixed SC"
                    :font "JetBrainsMono Nerd Font"
                    :height 120
                    :weight 'regular)

(add-to-list 'default-frame-alist 
			 ;'(font . "Sarasa Fixed SC")
             '(font . "JetBrainsMono Nerd Font"))


;; Line
(setopt global-visual-line-mode t)          ; Enable truncated lines
(setopt global-display-line-numbers-mode t) ; Display line numbers
;(global-visual-line-mode 1)                ;;!(COMPAT (<=29.1)) 
;(global-display-line-numbers-mode 1)       ;;!(COMPAT (<=29.1))
(setopt display-line-numbers-type 'relative) ; Relative line numbers
(setopt display-line-numbers-widen t)
(setopt display-line-numbers-width 3)
(setopt line-spacing 0.12) ; Set line-spacing

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setopt c-basic-offset   4
        tab-width        4
        indent-tabs-mode nil)

;; Scrolling
(setopt scroll-error-top-bottom t)
(setopt scroll-preserve-screen-position t)
(setopt scroll-conservatively 20)
(setopt scroll-error-top-bottom t)
(setopt scroll-margin 0)
(setopt hscroll-margin 2
        hscroll-step 1)
(setq auto-window-vscroll nil)

;; Mouse
(setopt mouse-wheel-progressive-speed nil) ; Disable progressive speed when scrolling
(setopt mouse-yank-at-point nil)
;(setopt mouse-wheel-tilt-scroll t)
;(setopt mouse-wheel-flip-direction t)

;; Uodo/redo
(setopt undo-limit (* 13 160000)
        undo-strong-limit (* 13 240000)
        undo-outer-limit (* 13 24000000))

;; Modeline
(display-time)                       ; Show time in modeline
(setopt display-time-format "%a %T") ; Set time format
(setopt display-time-24hr-format t)  ; 0 <= hh <= 23 / 1 <= hh <= 12 AM/PM
(setopt display-time-interval 1)
(setopt display-time-default-load-average nil)
(run-with-timer 0 1 (lambda ()
                      ;(display-time-mode 1) ;;!(COMPAT (<=29.1))
                      (setopt display-time-mode t))) ; Update time pre-second 

;; Minibuffer
(setopt enable-recursive-minibuffers t) ; Allow nested minibuffers

;; Completion
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt completions-group t)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completion-auto-select 'second-tab)

;; Files
;(auto-save-visited-mode 1) ;;!(KEEP-ONLY)
(setopt auto-save-visited-mode t) ; Auto-save files at an interval
(setopt delete-by-moving-to-trash 
        (not noninteractive)) ; Deleting files go to OS's trash folder
(setopt remote-file-name-inhibit-delete-by-moving-to-trash t)
(setopt find-file-suppress-same-file-warnings t)

;; Misc
(setopt set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

;; Backups / Lockfiles
(setopt make-backup-files nil)    ; Stop creating  ~ backup files    (~xxx)
(setopt create-lockfiles nil)     ; Stop creating .# lock files      (.#xxx)

;; Basic modes
(setopt initial-major-mode 'fundamental-mode)
(setopt major-mode 'text-mode)
(setopt column-number-mode t)     ; Show column number on mode line
(setopt blink-cursor-mode nil)    ; Don't blink cursor
(setopt delete-selection-mode t)  ; Select text and delete it by typing.
;(column-number-mode 1)     ;;!(COMPAT (<=29.1))
;(blink-cursor-mode -1)     ;;!(COMPAT (<=29.1))
;(delete-selection-mode 1)  ;;!(COMPAT (<=29.1))

;; Electric
;; Provide a set of automated editing assistance features.
(setopt electric-indent-mode nil) ; Turn off the weird indenting that Emacs does by default
(setopt electric-pair-mode t)     ; Turns on automatic parens pairing
;(electric-indent-mode -1) ;;!(COMPAT (<=29.1))
;(electric-pair-mode 1)    ;;!(COMPAT (<=29.1))

;; Auto-save
(auto-save-mode 1)
(setopt auto-save-default nil) ; Stop creating ## auto save files (#xxx#)
(setopt auto-save-no-message t)
(setq auto-save-include-big-deletions t)
(setopt kill-buffer-delete-auto-save-files t)
(setopt kill-do-not-save-duplicates t) ; Remove duplicates from the kill ring to reduce clutter

;; Auto-revert
;; Automatically reload file and show changes if the file has changed.
(setopt global-auto-revert-mode t)
;(global-auto-revert-mode 1) ;;!(COMPAT (<=29.1))
(setopt revert-without-query (list ".")  ; Do not prompt
        auto-revert-stop-on-user-input nil
        auto-revert-verbose t)
(setopt global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

;; Show-paren
;; Toggle visualization of matching parens.
(setopt show-paren-mode t)
;(show-paren-mode 1) ;;!(COMPAT (<=29.1))
(setopt show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
;(setq show-paren-style 'expression)


;; Recentf
;; Remembering recently edited files.
;(setopt recentf-mode t)
;;(recentf-mode 1) ;;!(COMPAT (<=29.1))
;(setopt recentf-max-saved-items 300)
;(setopt recentf-max-menu-items 15)
;(setopt recentf-auto-cleanup 'mode)
;(setopt recentf-exclude nil)

;; Save-place
;; Remembering the last place you visited in a file.
;(if (fboundp 'save-place-mode)
;     (setopt save-place-mode t)
;     ;(save-place-mode 1) ;;!(COMPAT (<=29.1))
;  (setopt save-place t)) ; obsolete in >= 25.1
;(setopt save-place-limit 600)

;; Savehist
;; Presevers the minibuffer history.
;(setopt savehist-mode t)
;;(savehist-mode 1) ;;!(COMPAT (<=29.1))
;(setopt history-length 300)
;(setopt savehist-save-minibuffer-history t)
;(setopt savehist-autosave-interval 300)
;(setopt savehist-additional-variables
;      '(kill-ring                                  ; clipboard
;        register-alist                             ; macros
;        mark-ring global-mark-ring                 ; marks
;        search-ring regexp-search-ring             ; searches
;        command-history extended-command-history)) ; commands  






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'skyz-better-defaults)
;;; skyz-better-defaults.el ends here


;; Modes

; *-mode
; (*-mode &optional arg) (command)
