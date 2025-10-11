;;; skyz-load-paths.el --- -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;;; ---------------------------------------------------------------------------
;;;; * PATH variables/constants
;;;; ---------------------------------------------------------------------------

;; The root directory of skyz-emacs
(defvar skyz-emacs/home-directory user-emacs-directory
  "skyz-emacs root directory. This is the root of the skyz-emacs's installation.")

;; Directory containing skyz-emacs's assets files. 
(defconst skyz-emacs/assets-directory
  (expand-file-name "assets/" skyz-emacs/home-directory)
  "skyz-emacs assets directory. This is the root of skyz-emacs's assets files.")

;; Directory containing skyz-emacs's lisp libs.
(defconst skyz-emacs/lisp-directory
  (expand-file-name "lisp/" skyz-emacs/home-directory)
  "skyz-emacs lisp directory. This is the root of the skyz-emacs's lisp config")

;; Directory containing skyz-emacs's modules config. 
(defconst skyz-emacs/modules-directory
  (expand-file-name "modules/" skyz-emacs/home-directory)
  "skyz-emacs modules directory. This is the root of skyz-emacs's modules config.")

;; Directory containing skyz-emacs' custom settings.
(defconst skyz-emacs/custom-directory
  (expand-file-name "custom/" skyz-emacs/lisp-directory)
  "skyz-emacs custom directory. Contains custom-settings files.")

;; Directory containing skyz-emacs's utils files.
(defconst skyz-emacs/utils-directory
  (expand-file-name "utils/" skyz-emacs/lisp-directory)
  "skzy-emacs utils directory. Contains utils source files and libraries.")

;; Directory containing skyz-emacs's core files.
(defconst skyz-emacs/core-directory
  (expand-file-name "core/" skyz-emacs/lisp-directory)
  "skyz-emacs core directory. Contains core source files and libraries.")


;;;; ---------------------------------------------------------------------------
;;;; * Load Paths
;;;; ---------------------------------------------------------------------------

(add-to-list 'load-path skyz-emacs/lisp-directory)
(add-to-list 'load-path skyz-emacs/custom-directory)
(add-to-list 'load-path skyz-emacs/core-directory)
(add-to-list 'load-path skyz-emacs/utils-directory)
(add-to-list 'load-path skyz-emacs/modules-directory)

;;;; ---------------------------------------------------------------------------
;;;; * (Optional) Local App Data  
;;;; ---------------------------------------------------------------------------
;;
;; Windows:
;; - LOCALAPPDATA   : C:\Users\{username}\AppData\Local
;; GNU/Linux:
;; - XDG_CACHE_HOME : ${HOME}/.cache
;; - XDG_DATA_HOME  : ${HOME}/.local/share
;; - XDG_STATE_HOME : ${HOME}/.local/state 
;; MacOS X:
;; - Follow the XDG Base Directory Specification.

(require 'base-paths "utils/paths")

;; Directory for persistent cache files.
(defconst skyz-emacs/cache-directory (paths/app-cache-directory "skyz-emacs")
  "Where skyz-emacs stores its global cache files.")

;; Directory for persistent data files.
(defconst skyz-emacs/data-directory (paths/app-data-directory "skyz-emacs")
  "Where skyz-emacs stores its global data files.")

;; Directory for state files.
(defconst skyz-emacs/state-directory (paths/app-state-directory "skyz-emacs")
  "Where skyz-emacs stores its global state files.")

;; If you use `no-littering.el' to fix inconsistent Emacs package paths, you can use
;; the following variables to modify the corresponding `no-littering-etc-directory'
;; and `no-littering-var-directory'.
(defvar skyz-emacs/etc-directory 
  (or (expand-file-name "etc/" skyz-emacs/home-directory)
      skyz-emacs/data-directory)
  "Make sure you have `no-littering.el' installed;
This variable will be bound to `no-littering-etc-directory'.")

(defvar skyz-emacs/var-directory skyz-emacs/cache-directory
  "Make sure you have `no-littering.el' installed;
This variable will be bound to `no-littering-var-directory'.")


;;;; ---------------------------------------------------------------------------
;;;; * Setup directories
;;;; ---------------------------------------------------------------------------

;; Ensure that some directories exists before it is used.
;; The 'parents argument creates any necessary parent directories.
(dolist (dirs '(;skyz-emacs/cache-directory
                ;skyz-emacs/data-directory
                ;skyz-emacs/state-directory
                skyz-emacs/var-directory
                skyz-emacs/etc-directory))
  (let ((path (symbol-value dirs)))
    (unless (file-exists-p path)
  (make-directory path t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'skyz-load-paths)
;;; skyz-load-paths ends here
