;;; utils/keybinds.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;(add-to-list 'load-path
;             (expand-file-name "common/" (file-name-directory load-file-name)))
(require 'init-const  "common/init-const")
(require 'init-funcs  "common/init-funcs")
(require 'init-macros "common/init-macros")


;; -----------------------------------------------------------------------------
;; * (Local) Leader Keys variables
;; -----------------------------------------------------------------------------
;; For Evil users, the <leader>/<localleader> key is commonly used. 

(defvar keybinds/leader-key "SPC"
  "The leader prefix key.")

(defvar keybinds/leader-alt-key "M-SPC"
  "An alternative leader prefix key.")

(defvar keybinds/localleader-key "SPC m"
  "The local leader prefix key, for major-mode specific commands.")

(defvar keybinds/localleader-alt-key "M-SPC m"
  "An alternative local leader prefix key, for major-mode specific commands.")


;; -----------------------------------------------------------------------------
;; * Keymap functions
;; -----------------------------------------------------------------------------

(defun compat/keymap-set (keymap key def)
  "Set KEY to DEF in KEYMAP."
  (if (fboundp 'keymap-set)
      (keymap-set keymap (kbd key) def)
    (define-key keymap (kbd key) def)))

(defun compat/keymap-multiset (keymap key def &rest more)
  "Multi-set KEY to DEF in KEYMAP.
KEY and DEF are the first key and definition. Additional keys and definitions 
are provided in MORE as alternating elements."
  (while key
    (compat/keymap-set keymap key def)
    (setq key (pop bindings) 
          def (pop bindings))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'base-keybinds)
;;; keybinds.el ends here


