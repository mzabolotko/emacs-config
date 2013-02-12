(add-to-list 'load-path (expand-file-name "~/emacs-config"))

;;------------------------------------------------------------------------------
;; set the file name to store custom settings from the M-x customize command
;; load the file with custom settings
;;------------------------------------------------------------------------------
(setq custom-file "~/emacs-config/config-custom.el")
(if (file-exists-p custom-file)
  (load custom-file))


;;------------------------------------------------------------------------------
;; start emacs as server to share settings through emacs clients.
;;------------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'config-elpa)
(require 'config-frame)
(require 'config-ido)
(require 'config-view)
(require 'config-dired)
(require 'config-xml)
(require 'config-file)
(require 'config-input)
(require 'config-buffer)
(require 'config-sunrise)
(require 'config-color)
(require 'config-fsharp)
(require 'config-dsvn)
(require 'config-yaml)
