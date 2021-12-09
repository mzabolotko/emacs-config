;; :+1_tone1:

;; inspired by
;; https://github.com/KaratasFurkan/.emacs.d#package-management
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

;; displaying World Time
(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("EDT" "Georgia")
    ("Europe/Moscow" "Moscow")))
(setq display-time-world-time-format "%Z\t%a %d %b %R")

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package feature-mode
  :init
  (setq feature-default-i18n-file "~/.emacs-config/feature-i18n.yml"))

(use-package tj3-mode
  :straight t)

(eval-and-compile
  (defun site-list-mu4e ()
    "/usr/share/emacs/site-lisp/mu4e/"))

(use-package mu4e
  :straight nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :load-path (lambda () (list (site-list-mu4e)))
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

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package fireplace
  :commands fireplace)

(use-package 2048-game
  :commands 2048-game)

(use-package zone
  :config
  (zone-when-idle 600))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom (markdown-header-scaling t))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

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

(use-package json-reformat)

(use-package json-snatcher)

(use-package json-mode
  :requires (json-reformat json-snatcher))

(use-package company
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
  :hook
  (prog-mode . company-mode))

(straight-register-package '(seq :local-repo nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun mz/openwith-open-wsl (command arglist)
  (let ((shell-file-name "/bin/sh"))
    (if (string= command "explorer.exe")
	(progn
	  (message "use explorer.exe")
	  (let* ((full-path (car arglist))
		 (directory (shell-quote-argument (file-name-directory full-path)))
		 (file-name (shell-quote-argument (file-name-nondirectory full-path)))
	         (command (concat "cd " directory " ; " command " " file-name " > /dev/null")))
		    ;; (message command)))
		    (start-process-shell-command "openwith-process" nil command)))
        (progn
	  (message "don't use explorer.exe")
	  (start-process-shell-command
	   "openwith-process" nil
	   (concat
            "exec nohup " command " "
            (mapconcat 'shell-quote-argument arglist " ")
            " >/dev/null"))))))

(defun mz/get-proc-version ()
  (let ((shell-file-name "/bin/sh")
        (command "cat /proc/version"))
    (shell-command-to-string command)))

(defun mz/check-wsl-p ()
  (if (string-match-p (regexp-quote "microsoft") (mz/get-proc-version))
    't
    nil))

(use-package openwith
  :if (mz/check-wsl-p)
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("docx"))
                "explorer.exe"
                '(file))
	  (list (openwith-make-extension-regexp
                '("archimate"))
                "explorer.exe"
                '(file))))
  (openwith-mode 1)
  :init
  (advice-add 'openwith-open-unix :override 'mz/openwith-open-wsl))

(use-package display-line-numbers)

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --si --group-directories-first")) ;; dired-rainbow doesn't work with custom time formats
  :config
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

  (add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
  (add-hook 'dired-mode-hook
          (lambda ()))

  (add-hook 'dired-load-hook
          (lambda ()
            (interactive)
            (dired-collapse))))

;;------------------------------------------------------------------------------
;; Hides dotfiles by default.
;;------------------------------------------------------------------------------
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (("C-x H" . dired-hide-dotfiles-mode)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :hook
  (prog-mode . highlight-indent-guides-mode))


(use-package page-break-lines)

;; https://github.com/hlissner/emacs-doom-themes
;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
(use-package spacegray-theme)

(use-package diminish)

(setq mz/settings-use-posframe nil)

(use-package posframe)

(use-package which-key
  :if (not mz/settings-use-posframe)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package which-key-posframe
  :requires (posframe which-key)
  :if mz/settings-use-posframe
  :custom
  (which-key-idle-secondary-delay 0.3)
  :custom-face
  (which-key-posframe-border ((t (:background "gray"))))
  :config
  (which-key-mode)
  (which-key-posframe-mode))


(defun mz/eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload 'local))                            ;; apply eww-header changes

(use-package cl-lib)

(use-package yasnippet
  :requires (cl-lib)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :requires (yasnippet cl-lib))

(use-package multiple-cursors
  :requires (cl-lib)
  :custom
  (mc/always-run-for-all t)
  :bind
  (("C-M-<mouse-3>" . mc/add-cursor-on-click)))

(use-package doom-themes
  :requires (cl-lib))
  ;; :straight t
  ;; :config
  ;; ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-opera-light t)   ;
  ;; ;; (load-theme 'doom-one t)

  ;; ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config))

(use-package theme-changer
  :requires (doom-themes)
  :config
  (setq calendar-latitude 55.75)
  (setq calendar-longitude 37.61)
  (change-theme 'doom-opera-light 'doom-one))

(use-package anzu
  :after isearch
  :config
  (global-anzu-mode +1))

(use-package s)

(use-package esup
  :requires (cl-lib s)
  :commands esup)

(use-package dash)

(use-package ht)

(use-package emojify
  :requires (seq ht)
  :commands emojify-mode)

(use-package dired-hacks-utils)

(use-package dired-rainbow
  :requires (dash dired-hacks-utils)
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#672a1e" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata" "config"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ff0761" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim" "cmd" "ps1"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js" "fsx" "exs"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" "java" "ex" ))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#9630ce" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem")) ;;
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(use-package dired-ranger
  :requires (dash dired-hacks-utils))

(use-package elisp-refs)

(use-package helpful
  :requires (dash s f elisp-refs)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

(use-package origami
  :requires (s dash cl-lib)
  :hook (yaml-mode . origami-mode))

(use-package f
  :requires (s dash))

(use-package dired-collapse
  :requires (dash f dired-hacks-utils))

(use-package shrink-path)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :requires (all-the-icons)
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

(use-package dashboard
  :requires (page-break-lines)
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

(use-package doom-modeline
  :requires (all-the-icons shrink-path dash)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-vcs-max-length 20)
  :custom-face
  (mode-line-highlight ((t (:inherit doom-modeline-bar :foreground "black"))))
  (doom-modeline-buffer-path ((t (:inherit font-lock-comment-face :slant normal))))
  :config
  (column-number-mode)
  (doom-modeline-mode))

(use-package avy
  :requires (cl-lib))

(use-package ace-window
  :requires (avy cl-lib)
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package pfuture)

(use-package lv)

(use-package lsp-mode
  :requires (dash f ht spinner markdown-mode lv)
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-enable-file-watchers nil
        read-process-output-max (* 1024 1024)  ; 1 mb
        lsp-completion-provider :capf
        lsp-idle-delay 0.500)
  (add-to-list 'exec-path "/opt/elixir-ls-0.7.0")
  :hook (
         (elixir-mode . #'lsp-deffered)
	 (java-mode . #'lsp-deferred)
         (kotlin-mode . #'lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq-default indent-tabs-mode nil)
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
  (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :requires (dash lsp-mode markdown-mode)
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-max-width 100))

(use-package hydra
  :requires (cl-lib lv))

(use-package cfrs
  :requires (dash s posframe))

(use-package treemacs
  :requires (cl-lib dash s ace-window pfuture hydra ht cfrs)
  :after (treemacs-all-the-icons lsp-mode)
  :custom
  (treemacs-width 20)
  ;; :bind
  ;; ("M-0" . treemacs-select-window)
  :hook
  (treemacs-mode . (lambda ()
                     (let* ((project-path (projectile-project-root))
                            (project-name (treemacs--filename project-path)))
                       (unless (treemacs--find-project-for-path project-path)
                         (treemacs-add-project project-path project-name)))))
  (treemacs-mode . (lambda ()
                   (face-remap-add-relative 'default :height .75))))

(use-package treemacs-all-the-icons
  :requires (all-the-icons treemacs)
  :after all-the-icons)

(use-package lsp-treemacs
  :requires (dash f ht treemacs lsp-mode)
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

(use-package bui
  :requires (dash))

(use-package dap-mode
  :requires (dash lsp-mode bui dash f s dash s lsp-treemacs posframe ht)
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package epl
  :requires (cl-lib))

(use-package pkg-info
  :requires (epl cl-lib))

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode)
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package request)

(use-package lsp-java
  :requires (lsp-mode markdown-mode dash f ht request treemacs dap-mode bui f s lsp-treemacs posframe)
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package fsharp-mode
  :requires (s)
  :config
  (setq-default fsharp-indent-offset 4))
;; ***

(use-package jsonrpc)

(use-package xref)

(use-package project
  :requires (xref))

(use-package flymake
  :requires (eldoc project))

;; Get eldoc error: (void-variable show-paren-context-when-offscreen) 
;; (use-package eglot
;;   :requires (jsonrpc flymake project xref))

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

;; Get eldoc error: (void-variable show-paren-context-when-offscreen) for eglot
;; (use-package eglot-fsharp
;;   :requires (eglot fsharp-mode jsonrpc))

(use-package projectile
  :requires (f pkg-info epl)
  :custom
  (projectile-auto-discover nil)
  (projectile-project-search-path
   (append
    (f-directories "~/development/github")
    (f-directories "~/development/learn")
    (f-directories "~/development/pets")
    (f-directories "/mnt/d/development/learn")
    (f-directories "/mnt/d/development/pets")))
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package treemacs-projectile
  :requires (projectile treemacs)
  :after (treemacs projectile))

(use-package spinner)

(use-package deadgrep
  :requires (dash s spinner)
  :commands deadgrep
  :bind
  (
   :map deadgrep-mode-map
	("C-c C-e" . deadgrep-edit-mode)))

(use-package minions
  :requires (doom-modeline)
  :hook (doom-modeline-mode . minions-mode))

(use-package org
  :hook ((org-mode . mz/org-mode-setup)
	 (before-save . mz/org-set-last-modified))
  :init
  (add-hook 'org-agenda-mode-hook #'hack-dir-local-variables-non-file-buffer)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag t
        org-enforce-todo-checkbox-dependencies t)

  (org-toggle-pretty-entities) ;; visual display of super- and subscripts
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				 (timeline . "  % s")
				 (todo . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (tags . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (search . " %i %-12:c")))

  (global-set-key "\C-cl" 'org-store-link)
  ;; (global-set-key "\C-cc" 'counsel-org-capture)
  (global-set-key "\C-ca" 'org-agenda)

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  (setq org-agenda-custom-commands
    '(("D" "Day agenda"
	((agenda "" ((org-agenda-ndays 1))) ;; limits the agenda display to a single day
	    (todo "IN-PROGRESS")
	    (todo "NEXT"
                  ((org-agenda-skip-entry-if 'deadline 'scheduled)
                   (org-agenda-dim-blocked-tasks 'invisible)))
	    (todo "WAITING"))
	    ((org-agenda-compact-blocks t)))
      ("W" "Day agenda"
	((agenda "" ((org-agenda-ndays 7))) ;; limits the agenda display to a single day
	    (todo "WAITING"))
	    ((org-agenda-compact-blocks t)))) ;; options set here apply to the entire block
	)

  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  (require 'org-indent)

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  (setq org-todo-repeat-to-state "REPEATING")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-agenda-files (list "~/Documents/org/Documents/Inbox.org"
                               "~/Documents/org/Documents/GTD.org"))

  (setq org-capture-templates
	'(("e" "Drill card Russian <-> English"
	    entry
	    (file+headline "~/Documents/org/Documents/drill/en-rus.org" "Cards")
	    (file "~/.emacs-config/drill-en-rus.orgcptmpl"))
          ("i" "Inbox")
          ("iy" "Inbox YouTube" entry
           (file "~/Documents/org/Documents/Inbox.org")
           "* Посмотреть видео [[%^{YouTube Link}][%^{YouTube Title}]] :youtube:\n %?")
          ("ia" "Inbox Article" entry
           (file "~/Documents/org/Documents/Inbox.org")
           "* Прочитать статью [[%^{Article Link}][%^{Article Title}]] :article:\n %^{Description} %?")))
          ;; ("ily" "Inbox youtube"
          ;;   entry
          ;;   (file "~/Documents/org/Documents/Inbox.org")
          ;;   "* Посмотреть видео %^{YouTube Link} %?")))

  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil))

;; (use-package org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package git-commit)

(use-package transient)

(use-package with-editor)

(use-package magit
  :requires (dash git-commit transient with-editor)
  :commands magit)

(use-package docker-tramp
  :requires (cl-lib))

(use-package tablist)

(use-package docker
  :requires (dash docker-tramp json-mode s tablist transient)
  :commands docker)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :requires (dash yaml-mode)
  :mode "docker-compose\\'")

(use-package emacsql)

(use-package magit-section
  :requires (dash))

(use-package emacsql-sqlite
  :requires (emacsql))

(defun mz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (global-linum-mode 0)
  (linum-mode 0))

;; --------------------------
;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;; from https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L2822-L2875
;;      https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321
;; --------------------------
(defun mz/org-find-time-file-property (property &optional anywhere)
"Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun mz/org-has-time-file-property-p (property &optional anywhere)
"Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (mz/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun mz/org-set-time-file-property (property &optional anywhere pos)
"Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (mz/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun mz/org-set-last-modified ()
"Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (mz/org-set-time-file-property "LAST_MODIFIED")))

(use-package org-roam
  :requires (dash f org emacsql emacsql-sqlite magit-section)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org/Documents/Roam")
  ;; (org-roam-node-display-template "${title:80} ${tags:60}")
  ;; (org-roam-capture-templates
  ;;   '(("d" "default" plain "%?" :if-new
  ;;      (file+head "%<%Y%m%d%H%M%S>.org" "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+ROAM_ALIASES: \n#+FILETAGS: ")
  ;;      :unnarrowed t)))

  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        :map org-mode-map
  ;;        ("C-M-i"   . completion-at-point))
  :config
  (org-roam-db-autosync-enable))

;; TODO: replace with use-package + straight
;; (add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
;; (load-library "org-roam-ui")
;; (setq org-roam-ui-sync-theme t
;;       org-roam-ui-follow t
;;       org-roam-ui-update-on-save t
;;       org-roam-ui-open-on-start t

(setq ispell-program-name "/usr/bin/hunspell")

(use-package kubel
  :requires (transient dash s yaml-mode))

(provide 'config-packages)
