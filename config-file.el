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

;;------------------------------------------------------------------------------
;; Non-nil means use lockfiles to avoid editing collisions.
;;------------------------------------------------------------------------------
(setq create-lockfiles nil)

(provide 'config-file)
