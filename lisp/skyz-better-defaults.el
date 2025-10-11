;;; skyz-better-defaults.el --- Better default configurations -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
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

;; Fonts
(set-face-attribute 'default nil
                    :font "Sarasa Fixed SC"
                    :height 120
                    :weight 'regular)

(add-to-list 'default-frame-alist 
			 '(font . "Sarasa Fixed SC"))

;; Line
(global-visual-line-mode 1)                ; Enable truncated lines
(global-display-line-numbers-mode 1)       ; Display line numbers
(setq display-line-numbers-type 'relative) ; Relative line numbers
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-width 3)
(setq-default line-spacing 0.12) ; Set line-spacing

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Scrolling
(setq fast-but-imprecise-scrolling t)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 20)
(setq scroll-error-top-bottom t)
(setq auto-window-vscroll nil)
(setq scroll-margin 0)
(setq hscroll-margin 2
      hscroll-step 1)

;; Mouse
(setq mouse-wheel-progressive-speed nil) ; Disable progressive speed when scrolling
(setq mouse-yank-at-point nil)
;(setq mouse-wheel-tilt-scroll t)
;(setq mouse-wheel-flip-direction t)

;; Uodo/redo
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; Modeline
(display-time)                        ; Show time in modeline
;(display-time-mode 1)
(setq display-time-format "%a %T")    ; Set time format
(setq display-time-24hr-format t)     ; 0 <= hh <= 23 / 1 <= hh <= 12 AM/PM
;(setq display-time-interval 1)
;(setq display-time-default-load-average nil)
(run-with-timer 0 1 (lambda () (display-time-mode 1))) ; Update time pre-second 

;; Minibuffer
(setq enable-recursive-minibuffers t) ; Allow nested minibuffers

;; Completion
(setq completion-cycle-threshold 1)
(setq completions-detailed t)
(setq completions-group t)
(setq completions-max-height 20)
(setq completions-format 'one-column)
(setq completion-styles '(basic initials substring))
(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)

;; Files
(auto-save-visited-mode 1) ; Auto-save files at an interval
(setq delete-by-moving-to-trash (not noninteractive)) ; Deleting files go to OS's trash folder
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)
(setq find-file-suppress-same-file-warnings t)

;; Misc
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

;; Backups / Lockfiles
(setq make-backup-files nil)    ; Stop creating  ~ backup files    (~xxx)
(setq create-lockfiles nil)     ; Stop creating .# lock files      (.#xxx)

;; Basic modes
(setq-default major-mode 'text-mode)
(column-number-mode 1)     ; Show column number on mode line
(blink-cursor-mode -1)     ; Don't blink cursor
(delete-selection-mode 1)  ; Select text and delete it by typing.

;; Electric
;; Provide a set of automated editing assistance features.
(electric-indent-mode -1) ; Turn off the weird indenting that Emacs does by default
(electric-pair-mode 1)    ; Turns on automatic parens pairing

;; Auto-save
(auto-save-mode 1)
(setq auto-save-default nil) ; Stop creating ## auto save files (#xxx#)
(setq auto-save-no-message t)
(setq auto-save-include-big-deletions t)
(setq kill-buffer-delete-auto-save-files t)
(setq kill-do-not-save-duplicates t) ; Remove duplicates from the kill ring to reduce clutter

;; Auto-revert
;; Automatically reload file and show changes if the file has changed.
(auto-revert-mode 1)        ; (buffer-local) minor mode
(global-auto-revert-mode 1) ; (global) minor mode                  
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)
(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

;; Show-paren
;; Toggle visualization of matching parens.
(show-paren-mode 1)
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; Recentf
;; Remembering recently edited files.
(recentf-mode 1)
(setq recentf-max-saved-items 300)
(setq recentf-max-menu-item 15)
(setq recentf-auto-cleanip 'mode)
(setq recentf-exclude nil)

;; Save-place
;; Remembering the last place you visited in a file.
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (setq-default save-place t)) ; obsolete in >= 25.1
(setq save-place-limit 600)

;; Savehist
;; Presevers the minibuffer history.
(savehist-mode 1)
(setq history-length 300)
(setq savehist-save-minibuffer-history t)
(setq savehist-autosave-interval 300)
(setq savehist-additional-variables
      '(kill-ring                         ; clipboard
        register-alist                    ; macros
        mark-ring global-mark-ring        ; marks
        search-ring regexp-search-ring    ; searches
        extended-command-history))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'skyz-better-defaults)
;;; skyz-better-defaults.el ends here
