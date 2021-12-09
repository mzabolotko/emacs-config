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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (;; ("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package ripgrep)

(use-package dash)

(use-package f)

(use-package org)

(use-package emacsql)

(use-package emacsql-sqlite)

(use-package magit-section)

(use-package org-roam
  :after (dash f org emacsql emacsql-sqlite magit-section)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org/Documents/Roam")
  (org-roam-node-display-template "${title:80} ${tags:60}")
  (org-roam-capture-templates
    '(("d" "default" plain "%?" :if-new
       (file+head "%<%Y%m%d%H%M%S>.org" "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+ROAM_ALIASES: \n#+FILETAGS: ")
       :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :after org-roam
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package magit
  :after (dash git-commit transient with-editor)
  :commands magit)


;; (require 'config-view)
;; (require 'config-xml)
;; (require 'config-calendar)
;; (require 'config-file)
(require 'config-input)
;; (require 'config-buffer)
;; ;; (require 'config-fsharp)
;; ;; (require 'config-csharp)
;; (require 'config-packages)

;; (if (file-exists-p custom-file)
;;   (load custom-file))
