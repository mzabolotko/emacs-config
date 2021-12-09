(add-to-list 'load-path (expand-file-name "~/.emacs-config"))

;; https://systemcrafters.cc/advanced-package-management/using-straight-el/
;; Since straight.el doesn't come with Emacs, we need a way to make sure it can be installed without using package.el.
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;------------------------------------------------------------------------------
;; set the file name to store custom settings from the M-x customize command
;; load the file with custom settings
;;------------------------------------------------------------------------------
(setq custom-file "~/.emacs-config/config-custom.el")

;;------------------------------------------------------------------------------
;; start emacs as server to share settings through emacs clients.
;;------------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;; (require 'config-elpa)
(require 'config-use-package)
(require 'config-view)
(require 'config-xml)
(require 'config-calendar)
(require 'config-file)
(require 'config-input)
(require 'config-buffer)
;; (require 'config-fsharp)
;; (require 'config-csharp)
(require 'config-packages)

(if (file-exists-p custom-file)
  (load custom-file))
