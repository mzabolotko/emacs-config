;;------------------------------------------------------------------------------
;; set default font
;;------------------------------------------------------------------------------
(if (find-font (font-spec :name "Monospace"))
    (set-default-font "Manospace"))


;;------------------------------------------------------------------------------
;; setup default input method to russian-computer
;;------------------------------------------------------------------------------
(setq default-input-method "russian-computer")


(provide 'config-input)
