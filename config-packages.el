;; inspired by
;; https://github.com/KaratasFurkan/.emacs.d#package-management
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

(use-package esup
  :commands esup)

(setq mz/settings-use-postframe nil)

(defun mz/eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload 'local))                            ;; apply eww-header changes

(use-package diminish)

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

;; https://github.com/hlissner/emacs-doom-themes
;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
(use-package spacegray-theme
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-opera-light t)   ;
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package theme-changer
  :config
  (setq calendar-latitude 55.75)
  (setq calendar-longitude 37.61)
  (change-theme 'doom-opera-light 'doom-one))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-encoding t)
  (doom-modeline-vcs-max-length 20)
  :custom-face
  (mode-line-highlight ((t (:inherit doom-modeline-bar :foreground "black"))))
  (doom-modeline-buffer-path ((t (:inherit font-lock-comment-face :slant normal))))
  :config
  (column-number-mode)
  (doom-modeline-mode))

(use-package anzu
  :after isearch
  :disabled				; using ivy + swiper
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

(use-package display-line-numbers)


(use-package dired
  :ensure nil
  ;; :defer 1
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

(use-package dired-rainbow
  :config
  (progn
    (message "worked")
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
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js" "fsx"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#9630ce" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem")) ;;
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired-collapse
  :defer t)

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

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helm-icons
  :after helm
  :config
  (helm-icons-enable))

(use-package emojify
  :commands emojify-mode)

(use-package which-key
  :if (not mz/settings-use-postframe)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package which-key-posframe
  :if mz/settings-use-postframe
  :custom
  (which-key-idle-secondary-delay 0.3)
  :custom-face
  (which-key-posframe-border ((t (:background "gray"))))
  :config
  (which-key-mode)
  (which-key-posframe-mode))

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
  :hook
  (prog-mode . company-mode))

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

(use-package counsel-projectile
  :after projectile
  :config
  ;; change the default action - to open a directory of the project
  (counsel-projectile-modify-action
    'counsel-projectile-switch-project-action
    '((move counsel-projectile-switch-project-action-dired 1)
      (setkey counsel-projectile-switch-project-action-dired "o")
      (setkey counsel-projectile-switch-project-action " ")))
  (counsel-projectile-mode))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

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

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom (markdown-header-scaling t))

(use-package docker
  :commands docker)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package origami
  :hook (yaml-mode . origami-mode))

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

(use-package kubel)

;; displaying World Time
(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("EDT" "Georgia")
    ("Europe/Moscow" "Moscow")))
(setq display-time-world-time-format "%Z\t%a %d %b %R")

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  ;; (setq enable-recursive-minibuffers t)

  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c k") 'counsel-rg)

  ;; Use different regex strategies per completion command
  ;; (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  ;; (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  ;; (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1)
;;   :after counsel
;;   :config
;;   (setq ivy-format-function #'ivy-format-function-line)
;;   (setq ivy-rich-display-transformers-list
;;         (plist-put ivy-rich-display-transformers-list
;;                    'ivy-switch-buffer
;;                    '(:columns
;;                      ((ivy-rich-candidate (:width 40))
;;                       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
;;                       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
;;                       (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
;;                       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))))))  ; return file path relative to project root or `default-directory' if project is nil

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; helps with easily switching between windows based on a predefined set of keys used to identify each.
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(defun mz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (global-linum-mode 0)
  (linum-mode 0))

;;--------------------------
;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;; from https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L2822-L2875
;;      https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321
;;--------------------------
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

(use-package org
  :defer t
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
        org-cycle-separator-lines 2)

  (org-toggle-pretty-entities) ;; visual display of super- and subscripts
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				 (timeline . "  % s")
				 (todo . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (tags . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (search . " %i %-12:c")))

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'counsel-org-capture)
  (global-set-key "\C-ca" 'org-agenda)

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  (setq org-agenda-custom-commands
    '(("D" "Day agenda"
	((agenda "" ((org-agenda-ndays 1))) ;; limits the agenda display to a single day
	    (todo "IN-PROGRESS")
	    (todo "NEXT")
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

  (setq org-capture-templates
	'(("e" "Drill card Russian <-> English"
	   entry
	   (file+headline "~/Documents/org/Documents/drill/en-rus.org" "Cards")
	   (file "~/.emacs-config/drill-en-rus.orgcptmpl"))))

  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Documents/org/Documents/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
    '(("d" "default" plain #'org-roam--capture-get-point
      "%?"
      :file-name "%<%Y%m%d%H%M%S>"
      :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+ROAM_ALIAS: \n#+ROAM_TAGS: "
      :unnarrowed t)))

  :bind (:map org-roam-mode-map
              (("C-c n l"   . org-roam)
               ("C-c n f"   . org-roam-find-file)
	       ("C-c n g"   . org-roam-graph)
               ("C-c n c"   . org-roam-dailies-capture-today))
         :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))


(use-package org-ref
  :init
  (setq reftex-default-bibliography "~/Documents/org/Documents/bibliography/references.bib"
	org-ref-default-bibliography '("~/Documents/org/Documents/bibliography/references.bib")
	org-ref-completion-library 'org-ref-ivy-cite
	org-ref-pdf-directory "~/Documents/org/Documents/bibliography/bibtex-pdfs/"))

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(use-package deft
  :commands (deft)
  :config (setq deft-directory "~/Documents/org/Documents/Roam"
		deft-extensions '("md" "org")
                deft-recursive t
		deft-use-filename-as-title nil
		deft-use-filter-string-for-filename t
                deft-file-naming-rules '((nospace . "-"))))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package magit-todos
  :defer t)

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(use-package daemons
  :commands daemons)

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

(use-package google-translate
  :disabled t
  :config
  (setq google-translate-default-source-language "ru"
	google-translate-default-target-language "en"
	google-translate-backend-method 'curl
	google-translate-show-phonetic t))

(use-package org-drill
  :config
  (setq org-drill-scope `("~/Documents/org/Documents/drill/en-rus.org")))

(provide 'config-packages)
