;;------------------------------------------------------------------------------
;; enables Interactively Do Things addon, to extend capabilities minibuffer
;; match any item containing the entered characteres (disable strict match)
;; enable ido mode to work with C-x C-f (find-files)
;;------------------------------------------------------------------------------
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
;;(ido-ubiquitous-mode t)
;;(setq ido-use-filename-at-point nil)
;;(setq ido-auto-merge-work-directories-length 0)
;;(setq ido-use-virtual-buffers t)


(provide 'config-ido)
