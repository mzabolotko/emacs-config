(require 'fsharp-mode)
(require 'dotnet)
(require 'company)
(require 'eglot-fsharp)

(setq inferior-fsharp-program "dotnet fsi")
(setq-default fsharp-indent-offset 4)

(add-hook 'fsharp-mode-hook 'dotnet-mode)
(add-hook 'fsharp-mode-hook 'company-mode)

(add-hook 'fsharp-mode-hook 'highlight-indent-guides-mode)
(add-to-list 'auto-mode-alist '("\.fs[iylx]?$" . fsharp-mode))

;;(setq inferior-fsharp-program "c:\\Users\\holod\\fsi.cmd")

(provide 'config-fsharp)
