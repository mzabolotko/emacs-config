;;------------------------------------------------------------------------------
;; register ELPA packages
;;------------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; resolving issue http://melpa.milkbox.net/
;;(defadvice package-compute-transaction
;;  (before
;;   package-compute-transaction-reverse (package-list requirements)
;;   activate compile)
;;  "reverse the requirements"
;;  (setq requirements (reverse requirements))
;;  (print requirements))
;;(package-initialize)

(provide 'config-elpa)
