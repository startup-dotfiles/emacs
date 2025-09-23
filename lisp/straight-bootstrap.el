;;; lisp/straight-bootstrap.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(defvar straight-bootstrap-version 7)
(defvar straight-base-dir (expand-file-name "cache/" user-emacs-directory))

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




(provide 'straight-bootstrap)

;;; straight-bootstrap.el ends here
