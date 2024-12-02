;;------------------------------------------------------------------------------
;; set default font
;;------------------------------------------------------------------------------

;; (if (find-font (font-spec :name "JetBrains Mono"))
;;   (set-face-attribute 'default nil :font "JetBrains Mono" :height 180))

(if (find-font (font-spec :name "Iosevka Comfy"))
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 180))

;; Set the fixed pitch face
(if (find-font (font-spec :name "Iosevka Comfy"))
  (set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Comfy"
                    :weight 'regular
                    :height 180))

;; ;; Set the fixed pitch face
;; (if (find-font (font-spec :name "JetBrains Mono"))
;;   (set-face-attribute 'fixed-pitch nil
;;                     :font "JetBrains Mono"
;;                     :weight 'regular
;;                     :height 180))

(if (find-font (font-spec :name "Iosevka Comfy Duo"))
  (set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Iosevka Comfy Duo"
                    :height 245
                    :weight 'regular))

;; (if (find-font (font-spec :name "Iosevka Aile"))
;;   (set-face-attribute 'variable-pitch nil
;;                     ;; :font "Cantarell"
;;                     :font "Iosevka Aile"
;;                     :height 245
;;                     :weight 'regular))


;;------------------------------------------------------------------------------
;; setup default input method to russian-computer
;;------------------------------------------------------------------------------
(setq default-input-method "russian-computer")


;;------------------------------------------------------------------------------
;; setup default coding system - UTF-8
;;------------------------------------------------------------------------------
(set-default-coding-systems 'utf-8)

;;------------------------------------------------------------------------------
;; disable beep sound
;;------------------------------------------------------------------------------
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(provide 'config-input)
