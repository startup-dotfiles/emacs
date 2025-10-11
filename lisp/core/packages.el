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

(defvar skyz-emacs/package-installed-p #'package-install-p
  "Function to use when checking if a package is installed.")


;;;; ---------------------------------------------------------------------------
;;;; * PACKAGE Base functions
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
  (interactive)
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-packages)
;;; packages.el ends here
