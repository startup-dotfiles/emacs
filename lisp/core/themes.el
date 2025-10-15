;;; core/themes.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; ---------------------------------------------------------------------------
;;;; * THEME variables/constants
;;;; ---------------------------------------------------------------------------

(defvar skyz-emacs/curren-theme nil
  "The current theme.")


;;;; ---------------------------------------------------------------------------
;;;; * THEME functions
;;;; ---------------------------------------------------------------------------

(defun skyz-emacs/theme--name (theme)
  "Return internal THEME name from `skyz-emacs/theme-alist'."
  (alist-get theme skyz-emacs/theme-alist))

(defun skyz-emacs/load--theme (theme)
  "Disable any currently enabled themes, then load this THEME
from `skyz-emacs/theme-alist'."
  (when-let* ((theme (or (skyz-emacs/theme--name theme) theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    theme))

(defun skyz-emacs/load--random-theme ()
  "Load the random theme from `skyz-emacs/theme-alist'."
  (interactive)
  (let* ((themes (mapcar #'cdr skyz-emacs/theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if (eq theme (skyz-emacs/theme--name skyz-emacs/theme))
        (skyz-emacs/load--random-theme)
      (skyz-emacs/load--theme theme))))


;;;###autoload
(defun skyz-emacs/load-theme (&optional theme)
  "Load theme specified by `skyz-emacs/theme'. If THEME is not nil, load the theme
instead of `skyz-emacs/theme'; of course THEME must come from `skyz-emacs/theme-alist'."
(interactive
 (list
  (intern
   (completing-read "Load theme: "
                    `(auto system random
                           ,@(mapcar #'car skyz-emacs/theme-alist))))))
  (let ((theme (or theme
                (and (boundp 'skyz-emacs/theme)
                     skyz-emacs/theme))))
    (pcase theme
      ('auto
       ;; TODO
       )
      ('system
       ;; TODO
       )
      ('random
       (skyz-emacs/load--random-theme))
      (_
       (skyz-emacs/load--theme theme)))))

;;;; ---------------------------------------------------------------------------
;;;; * THEME commands
;;;; ---------------------------------------------------------------------------






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'core-themes)
;;; themes.el ends here

; custom-theme-load-path
; custom-enabled-themes
; custom-safe-themes

; (load-theme    theme &optional no-confirm no-enable)
; (enable-theme  theme)
; (disable-theme theme)
