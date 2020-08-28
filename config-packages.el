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

(use-package all-the-icons-dired
  :init
  (add-to-list 'all-the-icons-icon-alist
               '("\\.mkv" all-the-icons-faicon "film"
                 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.srt" all-the-icons-octicon "file-text"
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
	("C-c C-e" . projectile-command-map)
	("C-c p" . projectile-command-map))
  :config
  (require 'tramp)
  (projectile-mode +1))

(use-package eglot
  :commands eglot
  :init
  (load-library "project")  ; TEMP: https://github.com/raxod502/straight.el/issues/531
  :custom
  (eglot-autoshutdown t))

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
    (zone-when-idle 15))

(provide 'config-packages)
