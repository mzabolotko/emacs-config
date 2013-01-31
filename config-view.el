;;------------------------------------------------------------------------------
;; disable a splash screen and a start message in the echo area
;;------------------------------------------------------------------------------
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;------------------------------------------------------------------------------
;; disable toolbar and scrollbar to minimalist look
;;------------------------------------------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;------------------------------------------------------------------------------
;; Toggle "Open Recent" menu
;;------------------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)


(provide 'config-view)
