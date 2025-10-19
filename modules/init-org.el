;;; init-org.el --- Configure for org-mode -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Refs:
;; - https://www.gnu.org/software/emacs/manual/html_node/org/index.html


;;; Code:



;; Basic org-mode config
(skyz-emacs/use-package org
 :ensure nil
 :straight (:type built-in)
 :elpaca nil ;;!compat
 
 ;; Rest of use-package args...
 :mode ("\\.org\\'" . org-mode)
 :custom-face (org-ellipsis ((t (:foreground unspecified))))

 :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
        (org-indent-mode . (lambda()
                             (diminish 'org-indent-mode)
                             (make-variable-buffer-local 'show-paren-mode)
                             (setq show-paren-mode nil))))

 :config (setopt org-todo-keywords
                 '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
                   (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
                 org-todo-keyword-faces '(("HANGUP" . warning)
                                          ("❓" . warning))
                 org-priority-faces '((?A . error)
                                      (?B . warning)
                                      (?C . success)))
         ;; Babel
         (setopt org-confirm-babel-evaluate nil
                 org-src-fontify-natively t
                 org-src-tab-acts-natively t))


;; Prettify UI
(use-package org-modern
  :after org
  :autoload global-org-modern-mode
  :init (global-org-modern-mode 1))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)
;;; init-org.el ends here
