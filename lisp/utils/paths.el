;;; utils/paths.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;(add-to-list 'load-path 
;             (expand-file-name "common/" (file-name-directory load-file-name)))
(require 'init-const  "common/init-const")
(require 'init-funcs  "common/init-funcs")
(require 'init-macros "common/init-macros")


;; -----------------------------------------------------------------------------
;; * Local App Data variables/functions
;; -----------------------------------------------------------------------------

;; Directory for persistent app cache files.
(defun paths/app-cache-directory (&optional dirname)
  "Return the absolute path used to store the app's cache files.
If DIRNAME is non-nil use it as the directory name, otherwise \"emacs\"."
  (let ((name (or dirname "emacs")))
    (if sys/win32-p
        (expand-file-name (concat name "/cache/") (getenv-internal "LOCALAPPDATA"))
      (expand-file-name (concat name "/") (or (getenv-internal "XDG_CACHE_HOME") "~/.cache")))))

;; Directory for persistent app data files.
(defun paths/app-data-directory (&optional dirname)
  "Return the absolute path used to store the app's data files.
If DIRNAME is non-nil use it as the directory name, otherwise \"emacs\"."
  (let ((name (or dirname "emacs")))
    (if sys/win32-p
        (expand-file-name (concat name "/data/") (getenv-internal "LOCALAPPDATA"))
      (expand-file-name (concat name "/") (or (getenv-internal "XDG_DATA_HOME") "~/.local/share")))))

;; Directory for app state files.
(defun paths/app-state-directory (&optional dirname)
  "Return the absolute path used to store the app's state files.
If DIRNAME is non-nil use it as the directory name, otherwise \"emacs\"."
  (let ((name (or dirname "emacs")))
    (if sys/win32-p
        (expand-file-name (concat name "/state/") (getenv-internal "LOCALAPPDATA"))
      (expand-file-name (concat name "/") (or (getenv-internal "XDG_STATE_HOME") "~/.local/state")))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'base-paths)
;;; paths.el ends here
