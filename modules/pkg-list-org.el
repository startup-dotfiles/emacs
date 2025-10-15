;;; pkg-list-org --- Org Mode packages  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Pre-install Emacs' built-in org to prevent `straight.el' from attempting to install
;; `org-mode' when resolving dependencies.
;; https://github.com/radian-software/straight.el/discussions/1162
;(add-to-list 'package-selected-packages 'org)

(add-to-list 'package-selected-packages 'org-modern)


(provide 'pkg-list-org)

;;; pkg-list-org ends here
