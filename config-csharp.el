;;(require 'flymake)

;;(require 'helm-config)
;; (require 'omnisharp)
;; (require 'bind-key)

;; (setq omnisharp-server-executable-path "/home/nop/.emacs.d/.cache/omnisharp/server/v1.34.5/run")

;; (eval-after-load
;;   'company
;;   '(add-to-list 'company-backends #'company-omnisharp))

;; (defun my-csharp-mode-setup ()
;;   (omnisharp-mode)
;;   (company-mode)
;;   (flycheck-mode)

;;   (setq indent-tabs-mode nil)
;;   (setq c-syntactic-indentation t)
;;   (c-set-style "ellemtel")
;;   (setq c-basic-offset 4)
;;   (setq truncate-lines t)
;;   (setq tab-width 4)
;;   (setq evil-shift-width 4)

;;   ;csharp-mode README.md recommends this too
;;   ;(electric-pair-mode 1)       ;; Emacs 24
;;   ;(electric-pair-local-mode 1) ;; Emacs 25

;;   (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;   (local-set-key (kbd "C-c C-c") 'recompile))


;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;      (setq auto-mode-alist
;; 	   (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; ;;(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

;; (bind-key "C-. g d" 'omnisharp-go-to-definition)
;; (bind-key "C-. f i" 'omnisharp-find-implementations)
;; (bind-key "C-. f u" 'omnisharp-find-usages)
;; (bind-key "C-. u t p" 'omnisharp-unit-test-at-point)
;; (bind-key "C-. s e" 'omnisharp-solution-errors)


(provide 'config-csharp)
