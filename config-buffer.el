;;------------------------------------------------------------------------------
;; enable line numbers
;;------------------------------------------------------------------------------
;;(global-linum-mode t)


;;------------------------------------------------------------------------------
;; visualization of matching parens
;;------------------------------------------------------------------------------
(show-paren-mode t)


;;------------------------------------------------------------------------------
;; list of tab stop positions
;;------------------------------------------------------------------------------
(setq tab-stop-list (number-sequence 4 120 4))


;;------------------------------------------------------------------------------
;; show column in the mode line
;;------------------------------------------------------------------------------
;; (column-number-mode 1)

(column-number-mode)
(setq display-line-numbers 'relative)

;; Enable line numbers for some modes
(dolist (mode '(;;text-mode-hook
		;;conf-mode-hook
                prog-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
  ;; (add-hook mode (lambda () (highlight-indent-guides-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
  ;; (add-hook mode (lambda () (highlight-indent-guides-mode 0))))

;;------------------------------------------------------------------------------
;; toggle line highlighting
;;------------------------------------------------------------------------------
(global-hl-line-mode 1)


;;------------------------------------------------------------------------------
;; trailing whitespace is unnecessary
;;------------------------------------------------------------------------------
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;;------------------------------------------------------------------------------
;; do not truncate lines
;;------------------------------------------------------------------------------
(setq-default truncate-lines t)

;;------------------------------------------------------------------------------
;; getting rid of the “yes or no” prompt and replace it with “y or n”
;;------------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;;------------------------------------------------------------------------------
;; getting rid the annoying confirmation if a file or ddddddd dddd
;; buffer does not exist when you use C-x C-f or C-x b.
;; ------------------------------------------------------------------------------
(setq confirm-nonexistent-file-or-buffer nil)

;;------------------------------------------------------------------------------
;; kill a buffer with a live process attached to it
;;------------------------------------------------------------------------------
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;;------------------------------------------------------------------------------
;; Whether to add a newline automatically at the end of the file.
;; A value of t means do this only when the file is about to be saved.
;;------------------------------------------------------------------------------
(setq require-final-newline t)

(provide 'config-buffer)
