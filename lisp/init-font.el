;;; init-font.el --- Font configuration  -*- lexical-binding: t -*-

;;; Commentary:

;; Fonts, faces, text scaling, and frame setup

;;; Code:


;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(set-face-attribute 'default nil
                    :font "Sarasa Fixed SC"
                    :height 120
                    :weight 'medium)

;; Fonts
(add-to-list 'default-frame-alist 
			 '(font . "Sarasa Fixed SC"))



(setq-default line-spacing 0.12)



(provide 'init-font)

;;; init-font.el ends here
