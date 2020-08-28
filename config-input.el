;;------------------------------------------------------------------------------
;; set default font
;;------------------------------------------------------------------------------
;; (if (find-font (font-spec :name "Ubuntu Mono"))
;;    (set-default-font "Ubuntu Mono-12"))
;;    ;; (set-face-attribute 'default nil :font "Ubuntu Mono-14"))
;; (if (find-font (font-spec :name "DejaVu LGC Sans Mono"))
    ;; (set-face-attribute 'default nil :font "DejaVu LGC Sans Mono-12"))
(set-face-attribute 'default nil :height 200)

(if (find-font (font-spec :name "Fira Mono"))
    (set-face-attribute 'default nil :font "Fira Mono-12"))


;;------------------------------------------------------------------------------
;; setup default input method to russian-computer
;;------------------------------------------------------------------------------
(setq default-input-method "russian-computer")

;;------------------------------------------------------------------------------
;; disable beep sound
;;------------------------------------------------------------------------------
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(provide 'config-input)
