;;; skyz-defaults-config.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Mouse
(setq mouse-wheel-progressive-speed nil) ; Disable progressive speed when scrolling
(setq scroll-conservatively 10)          ; Smooth scrolling


;; Better defaults
(setq delete-by-moving-to-trash t)     ; Deleting files go to OS's trash folder
(setq set-mark-command-repeat-pop t)   ; Repeating C-SPC after popping mark pops it again
;; (setq-default major-mode 'text-mode)


;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)


;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)


;; Basic modes
(column-number-mode 1)          ; Show column number on mode line
;;(display-time-mode 1)           ; Display time in mode line / tab bar
(blink-cursor-mode -1)          ; Don't blink cursor

(show-paren-mode 1)             ; Enable show paren mode
(electric-indent-mode -1)       ; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)          ; Turns on automatic parens pairing
(delete-selection-mode 1)       ; Select text and delete it by typing.
(recentf-mode 1)                ; Remembering recently edited files
(auto-save-visited-mode 1)      ; Auto-save files at an interval

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



(provide 'skyz-defaults-config)

;;; skyz-defaults-config.el ends here
