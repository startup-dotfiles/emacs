;;; bootstrap-elpaca.el ---  -*- lexical-binding: t -*-

;;; Commentary:
;; 
;; elpaca.el: An Elisp Package Manager
;;

;;; Code:


;;; Bootstrap elpaca
(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" skyz-emacs/var-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; If on Windows, disable symlinks
;; (Symlinks require admin permissions before Windows 11)
(when (member system-type '(windows-nt ms-dos))
  (elpaca-no-symlink-mode))



;;; Setup skyz-emacs' package settings
;; Configure skyz-emacs to use elpaca as package manager. 
(load (expand-file-name "packages"
                        skyz-emacs/core-directory))

(setq skyz-emacs/package-installer #'elpaca-try)
(setq skyz-emacs/package-installed-predicate #'straight--installed-p)



;;; use-package Integration 
;; Refs:
;; - https://github.com/progfolio/elpaca/blob/master/doc/manual.md#use-package-integration
(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode))


(provide 'bootstrap-elpaca)

;;; bootstrap-elpaca.el ends here
















