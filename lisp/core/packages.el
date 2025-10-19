;;; core/packages.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'package))

;;;; ---------------------------------------------------------------------------
;;;; * PACKAGE variables/constants
;;;; ---------------------------------------------------------------------------

(defvar skyz-emacs/package-installer #'package-install
  "Function to use when installing packages.")

(defvar skyz-emacs/package-installed-p #'package-installed-p
  "Function to use when checking if a package is installed.")


;;;; ---------------------------------------------------------------------------
;;;; * PACKAGE functions
;;;; ---------------------------------------------------------------------------

(defun skyz-emacs/package--install-package (package &optional installer-fn predicate-fn)
  "Install PACKAGE optionally using INSTALLER-FN.

Uses the PREDICATE-FN to check if a package is already installed before installing it.
Default values for both the PREDICATE-FN and INSTALLER-FN are helid in the
`skyz-emacs/package-installed-p' and
`skyz-emacs/pakcage-installer' variables."
  (let ((checker (or predicate-fn skyz-emacs/package-installed-p))
        (installer (or installer-fn skyz-emacs/package-installer)))
    (unless (funcall checker package)
      (funcall installer package))))


;;;###autoload
(defun skyz-emacs/package-install-selected-packages ()
  "Install all packages listed in rhe `package-selected-packages' list.

If `skyz-emacs/package-installer' has been customized, use it to install packages
one at time from the list `package-selected-packages', otherwise use the built-in
`package-install-selected-packages', which is purpose built for this."
  (if (eq #'package-install skyz-emacs/package-installer)
      (package-install-selected-packages t)
    (mapc #'skyz-emacs/package--install-package package-selected-packages)))



;;;###autoload
(defun skyz-emacs/load-package-manager ()
  "Load the package manager specified by `skyz-emacs/package-manager'.

If it's `built-in`, initialize Emacs' built-in package manager (package.el).
Otherwise load the file associated in `skyz-emacs/package-manager-alist' if it exists."
  (let ((choice (or (and (boundp 'skyz-emacs/package-manager)
                          skyz-emacs/package-manager)
                     'built-in)))
    (cond
     ((eq choice 'built-in)
      (require 'package)
      (unless (bound-and-true-p package--initialized)
        (package-initialize))
      (message "Using built-in package manager (package.el)."))
     (t
      (let ((entry (assoc choice skyz-emacs/package-manager-alist)))
        (if (not entry)
            (message "Package manager '%s' not found in 
                      skyz-emacs/package-manager-alist" choice)
          (let ((file (cdr entry)))
            (if (and file (file-exists-p file))
                (progn
                  (load-file file)
                  (message "Loaded package manager '%s' from %s" choice file))
              (message "Init file for '%s' not found or nil: %s" choice file)))))))))


(defun skyz-emacs/package--backend-match-p (keyword backend)
  "Return non-nil if KEYWORD corresponds to BACKEND symbol."
  (pcase keyword
    (:ensure (eq backend 'built-in))
    (:straight (eq backend 'straight))
    (:elpaca (eq backend 'elpaca))
    (_ nil)))


(defmacro skyz-emacs/use-package (name &rest args)
  "Use `use-package' with conditional backend args placed after NAME.

Syntax:
  (skyz-emacs/use-package <name>
    [:ensure <val>]
    [:straight <val>]
    [:elpaca <val>]
    ;; then normal use-package keywords...
    :keyword ...)

Only the backend kvs matching `skyz-emacs/package-manager' are spliced into the
resulting (use-package NAME ...). Other backend kvs are ignored.

If `skyz-emacs/package-manager' is 'elpaca, convert :elpaca KEY to :ensure KEY."
  (declare (indent 1))
  ;; Separate trailing backend kvs (consecutive leading keywords after name)
  (let (backend-pairs rest to-splice up-args)
    (setq rest args)
    (while (and rest (keywordp (car rest))
                (memq (car rest) '(:ensure :straight :elpaca)))
      (push (cons (car rest) (cadr rest)) backend-pairs)
      (setq rest (cddr rest)))
    (setq backend-pairs (nreverse backend-pairs))
    (setq up-args rest)
    ;; Select matching backend pairs
    (setq to-splice nil)
    (dolist (pair backend-pairs)
      (let* ((kw (car pair))
             (val (cdr pair)))
        (when (skyz-emacs/package--backend-match-p kw skyz-emacs/package-manager)
          ;; If backend is elpaca and keyword is :elpaca, emit :ensure instead
          (cond
           ((and (eq skyz-emacs/package-manager 'elpaca)
                 (eq kw :elpaca))
            (push :ensure to-splice)
            (push val to-splice))
           (t
            (push kw to-splice)
            (push val to-splice))))))
    (setq to-splice (nreverse to-splice))
    ;; Final expansion
    `(use-package ,name
       ,@to-splice
       ,@up-args)))


;;;; ---------------------------------------------------------------------------
;;;; * PACKAGE commands
;;;; ---------------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-packages)
;;; packages.el ends here




; (package-install     pkg &optional dont-select)
; (package-installed-p pkg &optional min-version)

; package-load-list
; (package--initialized)
; (package-initialize &optional no-activate)
; (package-activate-all)
; (package-initialize) === (package-initialize t) -> (package-activate-all)

; package-selected-packages
; (package-install-selected-packages &optional noconfirm)

; (package-quickstart-refresh) -> Generate `package-quickstart-file'.
; (package-refresh-contents &optional async)

; package-archive-contents
