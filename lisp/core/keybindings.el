;;; core/keybindings.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'base-keybinds "utils/keybinds")

;;;; ---------------------------------------------------------------------------
;;;; * KEY(MAP) variables/constants
;;;; ---------------------------------------------------------------------------
;;;; For Evil users, the <leader>/<localleader> key is commonly used.

(defvar skyz-emacs/leader-map (make-sparse-keymap)
  "Base keymap for all skyz-emacs <leader> key commands.")

(defvar skyz-emacs/leader-key keybinds/leader-key
  "The <leader> prefix key.")

(defvar skyz-emacs/leader-alt-key keybinds/leader-alt-key
  "An alternative <leader> prefix key.")

(defvar skyz-emacs/localleader-key keybinds/localleader-key
  "The <localleader> prefix key.")

(defvar skyz-emacs/localleader-alt-key keybinds/localleader-alt-key
  "An alternative <localleader> prefix key.")


;;;; ---------------------------------------------------------------------------
;;;; * KEY(MAP) Base functions
;;;; ---------------------------------------------------------------------------

(defun skyz-emacs/keymap-set (keymap key def)
  "An alias function for `compat/keymap-set'."
  (compat/keymap-set keymap key def))

(defun skyz-emacs/keymap-multiset (keymap key def &rest more)
  "An alias function for `compat/keymap-multiset'."
  (compat/keymap-multiset keymap key def &rest more))

(defun skyz-emacs/keymap-leader-set (key def)
  "Set KEY to DEF in `skyz-leader-map'."
  (skyz-emacs/keymap-set skyz-emacs/leader-map key def))

(defun skyz-emacs/keymap-leader-multiset (key def &rest more)
  "Multi-set KEY to DEF in `skyz-leader-map'."
  (skyz-emacs/keymap-multiset skyz-emacs/leader-map key def &rest more))


;;;; ---------------------------------------------------------------------------
;;;; * Integrate with `which-key'
;;;; ---------------------------------------------------------------------------

(defun skyz-emacsc/keymap-leader-declare-prefix (prefix desc &rest more)
  "Declare a prefix PREFIX. PREFIX is a string describing a key sequence.
DESC is a string used as the prefix command."
  (declare (indent defun))
  (apply #'which-key-add-keymap-based-replacements skyz-emacs/leader-map
         prefix desc more))



;;;; ---------------------------------------------------------------------------
;;;; * Aliases
;;;; ---------------------------------------------------------------------------







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-keybindings)
;;; keybindings.el ends here

;References:
;  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html
;
;
;Global keymap:
;  - global-map
;  - (current-global-map)
;
;Local keymap:
;  - *-map / *-keymap
;  - *-mode-map (major-mode & minor-mode)
;  - (current-local-map)
;  - (current-minor-mode-maps)
;
;(make-keymap                &optional string)
;(make-sparse-keymap         &optional string)
;
;(define-keymap                            &key     full parent suppress name prefix keymap        &rest [key def]...)  
;(defvar-keymap              variable-name &key doc full parent suppress name prefix keymap repeat &rest [key def]...)
;(copy-keymap keymap)
;
;
;(suppress-keymap                          keymap &optional nodigits)
;
;
;(keymap-parent        keymap)
;(set-keymap-parent    keymap parent)
;(make-composed-keymap keymaps &optional parent)
;
;(use-global-map       keymap)
;(use-local-map        keymap)
;(set-transient-map    keymap &optional keep-pred on-exit message timeout)

;minor-mode-map-alist
;minor-mode-overriding-map-alist
;overriding-local-map
;overriding-terminal-local-map 
;overriding-local-map-menu-flag
;special-event-map
;emulation-mode-map-alists
;
;
;(minor-mode-key-binding          key &optional accept-default)
;
;(kbd keys)
;
;
;;; Legacy functions
;(substitute-key-definition  olddef newdef keymap &optional oldmap)
;
;(lookup-key                 keymap key &optional accept-default)
;(key-binding                       key &optional accept-default no-remap posistion)
;(local-key-binding                 keys &optional accept-default)
;(global-key-binding                keys &optional accept-default)
;
;;; Emacs 29.1+
;(keymap-substitute          keymap olddef newdef &optional oldmap)
;
;(keymap-lookup              keymap key &optional accept-default no-remap position)
;(keymap-local-lookup               keys &optional accept-default)
;(keymap-global-lookup              keys &optional accept-default message)
;
;
;
;;; Legacy functions
;(define-key              keymap     key def &optional remove)
;(define-key-after        keymap     key def &optional after)
;(global-set-key       (global-map)  key command)
;(global-unset-key     (global-map)  key)
;(local-set-key                      key command)
;(local-unset-key                    key)
;
;
;;; Emacs 29.1+
;(keymap-set              keymap     key def)
;(keymap-set-after        keymap     key def &optional after)
;(keymap-unset            keymap     key &optional remove)
;(keymap-global-set    (global-map)  key command)
;(keymap-global-unset  (global-map)  key &optional remove)
;(keymap-local-set                   key command)
;(keymap-local-unset                 key &optional remove)
