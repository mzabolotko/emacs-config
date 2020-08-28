;;------------------------------------------------------------------------------
;; setup auto detection detection
;;------------------------------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.config-template" . xml-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.config" . xml-mode) auto-mode-alist))


(provide 'config-xml)
