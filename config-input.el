;;------------------------------------------------------------------------------
;; set default font
;;------------------------------------------------------------------------------
(if (find-font (font-spec :name "Ubuntu Mono"))
;;    (set-default-font "Ubuntu Mono-12"))
    (set-face-attribute 'default nil :font "Ubuntu Mono-12"))


;;------------------------------------------------------------------------------
;; setup default input method to russian-computer
;;------------------------------------------------------------------------------
(setq default-input-method "russian-computer")


(provide 'config-input)
