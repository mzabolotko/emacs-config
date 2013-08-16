(require 'org)

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				 (timeline . "  % s")
				 (todo . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (tags . " %i %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
				 (search . " %i %-12:c")))

(setq org-agenda-files (quote ("d:/work/gtd/gtd.org")))

(provide 'config-org)
