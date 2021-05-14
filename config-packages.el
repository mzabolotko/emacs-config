;; inspired by https://github.com/KaratasFurkan/.emacs.d#package-management

(use-package esup
  :commands esup)

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

(use-package doom-themes
  :custom
  (doom-modeline-major-mode-color-icon t)
  :config
  (load-theme 'doom-spacegrey t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20)
  :custom-face
  (mode-line-highlight ((t (:inherit doom-modeline-bar :foreground "black"))))
  (doom-modeline-buffer-path ((t (:inherit font-lock-comment-face :slant normal))))
  :config
  (column-number-mode)
  (doom-modeline-mode))

(use-package anzu
  :after isearch
  :config
  (global-anzu-mode))

(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :bind
  (("C-M-<mouse-3>" . mc/add-cursor-on-click)))

(use-package beacon
  :preface
  (defconst cursor-color+1 (format "#%x" (+ 1 (string-to-number (string-remove-prefix "#" (face-attribute 'cursor :background)) 16))))
  :custom
  (beacon-color cursor-color+1)
  (beacon-blink-when-point-moves-vertically 10)
  (beacon-dont-blink-major-modes '(dashboard-mode))
  :config
  (beacon-mode))

(use-package all-the-icons)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package dashboard
  :custom
  (dashboard-startup-banner nil)
  (dashboard-set-navigator t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading-face ((t (:weight bold))))
  :config
  (dashboard-setup-startup-hook)
  (defun mz/home ()
    "Switch to home (dashboard) buffer."
    (interactive)
    (switch-to-buffer "*dashboard*"))
  :hook
  (dashboard-mode . (lambda () (setq cursor-type nil)))
  (dashboard-mode . (lambda () (face-remap-add-relative 'hl-line :weight 'bold))))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --si --group-directories-first --time-style=long-iso"))
  :config
;;------------------------------------------------------------------------------
;; setup default open action for specific extensions
;;------------------------------------------------------------------------------
  (setq dired-guess-shell-alist-user
    (list
      (list "\\.html" "explorer.exe")
      (list "\\.archimate" "explorer.exe")
      (list "\\.sln" "explorer.exe")
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

  (use-package dired-single))

;;------------------------------------------------------------------------------
;; Hides dotfiles by default.
;;------------------------------------------------------------------------------
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (("C-x H" . dired-hide-dotfiles-mode)))

(use-package all-the-icons-dired
  :init
  (add-to-list 'all-the-icons-icon-alist
               '("\\.mkv" all-the-icons-faicon "film"
                 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.srt" all-the-icons-octicon "file-text"
                 :v-adjust 0.0 :face all-the-icons-dcyan))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.fsx" all-the-icons-alltheicon "haskell"
                 :v-adjust 0.0 :face all-the-icons-dcyan))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.fs" all-the-icons-alltheicon "haskell"
                 :v-adjust 0.0 :face all-the-icons-dcyan))
  :config
  ;; Turn off all-the-icons-dired-mode before wdired-mode
  (defadvice wdired-change-to-wdired-mode (before turn-off-icons activate)
    (all-the-icons-dired-mode -1))
  (defadvice wdired-change-to-dired-mode (after turn-on-icons activate)
    (all-the-icons-dired-mode 1))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helm-icons
  :after helm
  :config
  (helm-icons-enable))

(use-package emojify
  :commands emojify-mode)

(use-package which-key-posframe
  :custom
  (which-key-idle-secondary-delay 0)
  :custom-face
  (which-key-posframe-border ((t (:background "gray"))))
  :config
  (which-key-mode)
  (which-key-posframe-mode))

(use-package helm
  :defer nil
  :custom
  (helm-M-x-always-save-history t)
  (helm-display-function 'pop-to-buffer)
  (savehist-additional-variables '(extended-command-history))
  (history-delete-duplicates t)
  :custom-face
  (helm-non-file-buffer ((t (:inherit font-lock-comment-face))))
  (helm-ff-file-extension ((t (:inherit default))))
  (helm-buffer-file ((t (:inherit default))))
  :config
  ;; (helm-mode)
  (savehist-mode))

(use-package helm-projectile
  ;; :bind
  ;; ("C-x f" . helm-projectile)
  :hook
  (projectile-mode . helm-projectile-on))

(use-package helm-descbinds
  :commands helm-descbinds)

(use-package helm-posframe
  :after helm
  :custom
  (helm-posframe-border-color "gray")
  (helm-posframe-parameters '((left-fringe . 5)
                              (right-fringe . 5)))
  :config
  (helm-posframe-enable))

(use-package company
  :defer nil
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  :bind
  (
   :map company-active-map
   ("RET" . nil)
   ("TAB" . company-complete-selection)
   ("<tab>" . company-complete-selection)
   ("C-s" . company-complete-selection)  ; Mostly to use during yasnippet expansion
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :config
  (global-company-mode))

(use-package json-mode
  :defer t)
(use-package json-navigator
  :after json-mode)

(use-package ripgrep)

(use-package deadgrep
  :commands deadgrep
  :bind
  (
   :map deadgrep-mode-map
	("C-c C-e" . deadgrep-edit-mode)))

(use-package treemacs
  :custom
  (treemacs-width 20)
  :bind
  ("M-0" . treemacs-select-window)
  :hook
  (treemacs-mode . (lambda ()
                     (let* ((project-path (projectile-project-root))
                            (project-name (treemacs--filename project-path)))
                       (unless (treemacs--find-project-for-path project-path)
                         (treemacs-add-project project-path project-name)))))
  (treemacs-mode . (lambda ()
                   (face-remap-add-relative 'default :height .75))))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package projectile
  :custom
  (projectile-auto-discover nil)
  (projectile-project-search-path (f-directories "~/development"))
  :bind
  (
    :map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (require 'tramp)
  (projectile-mode +1))

(use-package eglot
  :commands (eglot eglot-ensure)
  :init
  (load-library "project")  ; TEMP: https://github.com/raxod502/straight.el/issues/531
  :custom
  (eglot-autoshutdown t))
  ;; :config
  ;; (progn
  ;;   (add-to-list 'eglot-server-programs
  ;;                '(csharp-mode . ("/home/nop/Downloads/omni/omnisharp/OmniSharp.exe" "-lsp")))))

(use-package kotlin-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (kotlin-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; (use-package lsp-mode
;;   :commands (lsp lsp-deffered)
;;   :hook (kotlin-mode . lsp)
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t))

(use-package eldoc-box
  :after eglot
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-max-pixel-width 3000)
  (eldoc-box-max-pixel-height 2000)
  ;;(eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-use-multiline-p 2000.0)
  :hook
  (eglot--managed-mode . eldoc-box-hover-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package magit
  :commands magit)

(use-package vterm)

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom (markdown-header-scaling t))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (yaml-mode . display-line-numbers-mode))

(use-package fireplace
  :commands fireplace)

(use-package 2048-game
  :commands 2048-game)

(use-package zone
  :config
  (zone-when-idle 600))

(use-package ligature
  :load-path "../.emacs-config/"
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;;(ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(eval-and-compile
  (defun site-list-mu4e ()
    "/usr/share/emacs/site-lisp/mu4e/"))


(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :load-path (lambda () (list (site-list-mu4e)))
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash"))

  ;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
  ;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  ;; (setq mu4e-refile-folder "/[Gmail].All Mail")
  ;; (setq mu4e-trash-folder  "/[Gmail].Trash"))

  ;; (setq mu4e-maildir-shortcuts
  ;;     '(("/Inbox"             . ?i)
  ;;       ("/[Gmail]/Sent Mail" . ?s)
  ;;       ("/[Gmail]/Trash"     . ?t)
  ;;       ("/[Gmail]/Drafts"    . ?d)
  ;;       ("/[Gmail]/All Mail"  . ?a))))

(use-package feature-mode
  :ensure t
  :init
  (setq feature-default-i18n-file "~/.emacs-config/feature-i18n.yml"))

(use-package tj3-mode
  :ensure t)

(provide 'config-packages)
