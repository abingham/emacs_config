(setq load-path (cons "~/.emacs.d/fsharp" load-path))
(setq auto-mode-alist
      (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

(setq inferior-fsharp-program "/usr/local/bin/fsi")
(setq fsharp-compiler "/usr/local/bin/fsc")
