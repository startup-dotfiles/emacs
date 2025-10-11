;;; init-eshell.el --- 	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Refs:
;; - https://www.gnu.org/software/emacs/manual/html_mono/eshell.html


;;; Code:


;; Basic Emacs command shell config
(use-package eshell
  :straight (:type built-in)
  :ensure nil

  :bind (:map eshell-mode-map
         ([remap recenter-top-bottom] . eshell/clear))
  :config
  (with-no-warnings
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))))


;; `eldoc' support
(use-package esh-help
  :after eshell  
  :commands setup-esh-help-eldoc  
  :init (setup-esh-help-eldoc))


;; `cd' to frequent directory in `eshell'
(use-package eshell-z
  :after eshell
  :hook (eshell-mode . (lambda () (require 'eshell-z))))



(provide 'init-eshell)
;;; init-eshell.el ends here
