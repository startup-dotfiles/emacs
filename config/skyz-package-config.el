;;; skyz-package-config.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; `skyz-package-installer' defaults to `package-install', but can
;; be set by the user to some other installer function.

;; `skyz-package-installed-predicate' is used to check if a package
;; is already installed; it defautls to `package-installed-p', but can
;; be set by the user to some other predicate function.

;; `skyz-package-install-package' is used to install individual
;; packages, the installer and predicate functions can be passed in if
;; needed.

;; `skyz-package-install-package-list' installs packages from the
;; `package-selected-packages' list.  All `skyz-<module>-package'
;; files add packages to this list, they are not installed
;; automatically.  The user is responsible for iterating over that
;; list to install the packages, if desired.  Users are not obligated
;; to use the `skyz-<module>-package' files and may prefer to
;; manage installing packages without using any of the facilities
;; here.  Additionally, if they choose to use the
;; `skyz-<module>-package' files, they have the opportunity modify
;; the list before calling any processing occurs to install any packages

;;; Code:

(eval-when-compile
  (require 'package))


(defvar skyz-package-installer #'package-install
  "Function to use when installing packages.")

(defvar skyz-package-installed-predicate #'package-install-p
  "Function to use when checking if a package is installed.")


(defun skyz-package-install-package (package &optional installer-fn predicate-fn)
  "Install PACKAGE optionally using INSTALLER-FN.

Uses the PREDICATE-FN to check if a package is already installed
before installing it.  Default values for both the PREDICATE-FN an
INSTALLER-FN are held in the
`skyz-package-installed-predicate' and
`skyz-package-installer' variables."
  (let ((checker (or predicate-fn skyz-package-installed-predicate))
        (installer (or installer-fn skyz-package-installer)))
    (unless (funcall checker package)
            (funcall installer package))))


(defun skyz-package-install-selected-packages ()
   "Installs all packages listed in the `package-selected-packages' list.

If `skyz-package-installer' has been customized, use it to
install packages one at at time from the list
`package-selected-packages', otherwise use the built-in
`package-install-selected-packages', which is purpose built for
this."
   (if (eq #'package-install skyz-package-installer)
       (package-install-selected-packages t)
     (mapc #'skyz-package-install-package package-selected-packages)))



(provide 'skyz-package-config)

;;; skyz-package-config.el ends here
