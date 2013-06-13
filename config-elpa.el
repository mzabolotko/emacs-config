;;------------------------------------------------------------------------------
;; register ELPA packages
;;------------------------------------------------------------------------------
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
			   ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))
;; resolving issue http://melpa.milkbox.net/
(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))
(package-initialize)

(mapc
    (lambda (package)
    (or (package-installed-p package)
	(if (y-or-n-p (format "Package %s is missing. Install it? " package))
	    (package-install package))))
    '(fsharp-mode color-theme sunrise-commander dsvn magit yaml-mode csharp-mode sunrise-x-popviewer sunrise-x-modeline sunrise-x-tabs sunrise-x-w32-addons twittering-mode))

(provide 'config-elpa)
