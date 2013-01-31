;;------------------------------------------------------------------------------
;; set place to store backup files
;;------------------------------------------------------------------------------
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;------------------------------------------------------------------------------
;; use recucle bin
;;------------------------------------------------------------------------------
(setq delete-by-moving-to-trash t)


(provide 'config-file)
