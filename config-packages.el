;; inspired by
;; https://github.com/KaratasFurkan/.emacs.d#package-management
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

(defun mz/set-org-face-attributes ()
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
  (set-face-attribute 'org-column-title nil :background nil))


(defun mz/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (mz/set-org-face-attributes)
  ;; (diminish org-indent-mode)
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
  (setq vertico-resize t)

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
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package ripgrep)

(use-package dash)

(use-package f)

(use-package org
  :hook ((org-agenda-mode . hack-dir-local-variables-non-file-buffer)
	 (org-mode . mz/org-mode-setup)
	 (before-save . mz/org-set-last-modified))
  :init
  (add-hook 'org-agenda-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'org-mode-hook #'mz/org-mode-setup)
  (add-hook 'before-save-hook #'mz/org-set-last-modified)
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

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  (setq org-agenda-custom-commands
        '(("s" "Текущий спринт"
           ((tags-todo "LEVEL>2+@work+sprint/WAITING"
                       ((org-agenda-overriding-header "\nТекущий спринт. В ожидании\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-prefix-format "")))
            (tags-todo "LEVEL>2+@work+sprint/NEXT"
                       ((org-agenda-overriding-header "\nТекущий спринт. Следующие задачи\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-prefix-format "")))
            (tags-todo "LEVEL>2+@work+sprint+TODO=\"IN-PROGRESS\""
                       ((org-agenda-overriding-header "\nТекущий спринт. В работе\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-prefix-format "")))
            (tags-todo "LEVEL=2+@work+sprint+TODO=\"PROJ\""
                       ((org-agenda-overriding-header "\nТекущий спринт. Проекты\n")
                        (org-agenda-block-separator nil)
                        (org-agenda-remove-tags 't)
                        (org-agenda-prefix-format "")))
            (agenda "" ((org-agenda-span 7)
                        (org-agenda-prefix-format "%-12:c%?-12t% s")
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                        (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "%A %-e %B %Y")
                        (org-agenda-overriding-header "\nАгенда\n")))
          ))))

  (require 'org-indent)
  (require 'org-faces)
  (require 'org-tempo)
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (require 'org-protocol)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  (setq org-todo-repeat-to-state "REPEATING")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-agenda-files (list "~/Documents/Dropbox/Inbox.org"
                               "~/Documents/Dropbox/GTD.org"
			       "~/Documents/Dropbox/Work.org"
			       "~/Documents/Dropbox/habits.org"))

  (setq org-capture-templates
	'(("w"
	    "Default template"
	    entry
	    (file+headline "~/Documents/Dropbox/references.org" "References")
	    "** %:description\n\n  %:link \n\n %:initial"
	    :empty-lines 1
	    :kill-client t)
	  ("e" "Drill card Russian <-> English"
	    entry
	    (file+headline "~/Documents/Dropbox/drill/en-rus.org" "Cards")
	    (file "~/.emacs-config/drill-en-rus.orgcptmpl"))
          ("i" "Inbox")
          ("iy" "Inbox YouTube" entry
           (file "~/Documents/Dropbox/Inbox.org")
           "* Посмотреть видео [[%^{YouTube Link}][%^{YouTube Title}]] :youtube:\n %?")
          ("ia" "Inbox Article" entry
           (file "~/Documents/Dropbox/Inbox.org")
           "* Прочитать статью [[%^{Article Link}][%^{Article Title}]] :article:\n %^{Description} %?")))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'ox-md)

(use-package emacsql)

(use-package emacsql-sqlite)

(use-package magit-section)

(use-package org-roam
  :after (dash f org emacsql emacsql-sqlite magit-section)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Dropbox/Roam")
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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-auto-discover nil)
  (projectile-register-project-type 'dotnet-sln #'projectile-dotnet-project-p
                                  :project-file '("?*.sln")
                                  :compile "dotnet build"
                                  :run "dotnet run"
                                  :test "dotnet test")
  :custom
    (projectile-project-root-functions
	'(projectile-root-marked))
  :bind (:map projectile-mode-map
              ;; ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package doom-themes
  :after (cl-lib))

(use-package theme-changer
  :after (doom-themes)
  :config
  (setq calendar-latitude 55.75)
  (setq calendar-longitude 37.61)
  (setq theme-changer-post-change-functions
	'((lambda (theme) (mz/set-org-face-attributes))))
  (change-theme 'doom-opera-light 'doom-one))

;; https://github.com/hlissner/emacs-doom-themes
;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
(use-package spacegray-theme)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :hook
  (prog-mode . highlight-indent-guides-mode))

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

(use-package shrink-path
  :after (s dash f))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :ensure t
  ;; :after (all-the-icons shrink-path dash)
  :after
    (nerd-icons shrink-path dash)
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
  :init
    (doom-modeline-mode))

(use-package page-break-lines)

(use-package dashboard
  :after (page-break-lines)
  :custom
  (dashboard-startup-banner nil)
  (dashboard-set-navigator t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-set-init-info t)
  (dashboard-match-agenda-entry "@habit")
  ;; (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-agenda-release-buffers t)
  (dashboard-week-agenda t)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)
		     (agenda . 20)))
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

(use-package dired-hacks-utils)

(use-package dired-rainbow
  :after (dash dired-hacks-utils)
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

(use-package elm-mode)

(use-package yasnippet
  :after (cl-lib)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet cl-lib))

(use-package elixir-mode)

(use-package dap-mode)

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))
;; or use :hook (elm-mode . company-mode))

(use-package lsp-mode
  :after (dash f ht spinner lv elixir-mode)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all t
	lsp-elm-trace-server t)
	;; lsp-enable-file-watchers nil
        ;; read-process-output-max (* 1024 1024)  ; 1 mb
        ;; lsp-completion-provider :capf
        ;; lsp-idle-delay 0.500)
  (add-to-list 'exec-path "~/.elixir-language-server")
  :hook ((elm-mode . #'lsp-deffered)
	 (elixir-mode . lsp))
         ;; (elixir-mode . #'lsp-deffered)
	 ;; (java-mode . #'lsp-deferred)
         ;; (kotlin-mode . #'lsp-deferred)
         ;; (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after (dash lsp-mode markdown-mode)
  :commands lsp-ui-mode)

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package ledger-mode)

(use-package fsharp-mode
  :config
  (setq inferior-fsharp-program "/usr/bin/dotnet fsi --readline-"))


(use-package consult-eglot)

(use-package consult-company
  :config
    (define-key company-mode-map [remap completion-at-point] #'consult-company))

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n d" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package embark-consult)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package yafolding)

(use-package speed-type)

(setq display-time-24hr-format 't)
(display-time)

(use-package org-ql)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; helps with easily switching between windows based on a predefined set of keys used to identify each.
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package anzu
  :after isearch
  ;; :disabled				; using ivy + swiper
  :config
  (global-anzu-mode))

(use-package deadgrep
  :requires (dash s spinner)
  :commands deadgrep
  :bind
  (
   :map deadgrep-mode-map
	("C-c C-e" . deadgrep-edit-mode)))

(use-package docker
  :requires (dash docker-tramp json-mode s tablist transient)
  :commands docker)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :requires (dash yaml-mode)
  :mode "docker-compose\\'")

(use-package emojify
  :requires (seq ht)
  :commands emojify-mode)

(use-package esup
  :requires (cl-lib s)
  :commands esup)

(use-package multiple-cursors
  :requires (cl-lib)
  :custom
  (mc/always-run-for-all t)
  :bind
  (("C-M-<mouse-3>" . mc/add-cursor-on-click)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package eglot-fsharp)

(provide 'config-packages)
