;;------------------------------------------------------------------------------
;; disable a splash screen and a start message in the echo area
;;------------------------------------------------------------------------------
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;------------------------------------------------------------------------------
;; disable toolbar, tooltips and scrollbar to minimalist look
;;------------------------------------------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;;------------------------------------------------------------------------------
;; the fringe is a thin strip down the left and/or right edge of a
;; window. give some space for it.
;;------------------------------------------------------------------------------
(set-fringe-mode 10)

;;------------------------------------------------------------------------------
;; improve scrolling
;;------------------------------------------------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;;------------------------------------------------------------------------------
;; set frame transparency and maximize windows by default.
;;------------------------------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(90 . 90)) ;; do not work for wsl
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))  ;; do not work for wsl
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;------------------------------------------------------------------------------
;; Toggle "Open Recent" menu
;;------------------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)


(provide 'config-view)
