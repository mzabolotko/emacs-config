(add-to-list 'load-path (expand-file-name "~/.emacs-config"))

;;------------------------------------------------------------------------------
;; set the file name to store custom settings from the M-x customize command
;; load the file with custom settings
;;------------------------------------------------------------------------------
(setq custom-file "~/.emacs-config/config-custom.el")

;;(setq package-enable-at-startup nil)
;;(package-initialize)

;;------------------------------------------------------------------------------
;; start emacs as server to share settings through emacs clients.
;;------------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'config-elpa)
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
