
;;(setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
;;(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
;;(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

(setq inferior-fsharp-program "\"c:\\Program Files\\Microsoft F#\\v4.0\\Fsi.exe\"")
(setq fsharp-compiler "\"c:\\Program Files\\Microsoft F#\\v4.0\\Fsc.exe\"")

(provide 'config-fsharp)
