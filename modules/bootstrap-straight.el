;;; bootstrap-straight.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; 
;; striaght.el:  Next-generation, purely functional package manager for the Emacs hacker.
;;


;;; Code:


;;; Bootstrap straight
(setq package-enable-at-startup nil)

(defvar straight-bootstrap-version 7)
(defvar straight-base-dir skyz-emacs/var-directory)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version straight-bootstrap-version))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;; Setup skyz-emacs' package settings
;; Configure skyz-emacs to use straight as package manager. 
(load (expand-file-name "packages"
                        skyz-emacs/core-directory))

(setq skyz-emacs/package-installer #'straight-use-package)
(setq skyz-emacs/package-installed-p #'straight--installed-p)


;;; Integration with use-package and package.el
;; Refs: 
;; - https://github.com/radian-software/straight.el?tab=readme-ov-file#integration-with-use-package-1
;; - https://github.com/radian-software/straight.el?tab=readme-ov-file#integration-with-packageel
;; Ensure packages are installed with `straight.el' by default instead of `package.el'
(setq straight-enable-use-package-integration t) ;; Enable use-package integration
;;(setq straight-use-package-by-default t)         ;; Add :straight t by default in use-package macro (helpfully ignore :ensure t)
;;(setq straight-use-package-version 'straight)    ;; (For backwards compatibility)


(provide 'bootstrap-straight)

;;; bootstrap-straight.el ends here
