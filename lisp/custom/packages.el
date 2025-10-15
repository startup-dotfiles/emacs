;;; custom/packages.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup custom-packages nil
  "Configuration options related to packages for skyz-emacs."
  :group 'skyz-emacs)

(defconst skyz-emacs/init-straight-file
  (expand-file-name "bootstrap-straight.el" skyz-emacs/modules-directory)
  "Init file for bootstraping straight.")

(defconst skyz-emacs/init-elpaca-file
  (expand-file-name "bootstrap-elpaca.el" skyz-emacs/modules-directory)
  "Init file for bootstraping elpace.")

;;;; ---------------------------------------------------------------------------
;;;; * Package settings
;;;; ---------------------------------------------------------------------------

(defcustom skyz-emacs/package-manager-alist
  `((built-in  . "")
    (straight  . ,skyz-emacs/init-straight-file)
    (elpaca    . ,skyz-emacs/init-elpaca-file))
  "A list of the package managers."
  :group 'custom-packages
  :type '(alist :key-type (symbol :tag "PM name")
                :value-type (file :tag "PM config")))

(defcustom skyz-emacs/package-manager 'built-in
  "Set package manager for Emacs."
  :group 'custom-packages
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (symbol-name name)
                              name)))
                    skyz-emacs/package-manager-alist)))

(defcustom skyz-emacs/package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (iscas    . (("gnu"    . ,(format "%s://mirror.iscas.ac.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirror.iscas.ac.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirror.iscas.ac.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "A list of the package archives."
  :group 'custom-packages
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom skyz-emacs/package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'custom-packages
  :set (lambda (symbol value)
         (set symbol value)
         ;; Sets default package repositories
         (setq package-archives
               (or (alist-get value skyz-emacs/package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    skyz-emacs/package-archives-alist)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'custom-packages)
;;; pacakges.el ends here
