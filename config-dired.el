(require 'dired-x)

;;------------------------------------------------------------------------------
;; setup default open action for specific extensions
;;------------------------------------------------------------------------------
(setq dired-guess-shell-alist-user
      (list
       (list "\\.archimate" "explorer")
       (list "\\.sln" "explorer")
       (list "\\.zip" "7z x")))

;;------------------------------------------------------------------------------
;; dired search only by file names
;;------------------------------------------------------------------------------
(setq dired-isearch-filenames t)

;;------------------------------------------------------------------------------
;; Whether Dired deletes directories recursively.
;; If nil, Dired will not delete non-empty directories.
;; `always' means to delete non-empty directories recursively,
;; without asking.  This is dangerous!
;;------------------------------------------------------------------------------

(setq dired-recursive-deletes 'always)

(provide 'config-dired)
