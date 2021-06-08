(require 'package)

(require 'org)
(require 'ox-taskjuggler)

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				 (timeline . "  % s")
				 (todo . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (tags . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (search . " %i %-12:c")))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((plantuml . t)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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

(defun nolinum ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (global-linum-mode 0)
  (variable-pitch-mode 1)
  (linum-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)

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

(setq org-todo-repeat-to-state "REPEATING")
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)


(provide 'config-org)
