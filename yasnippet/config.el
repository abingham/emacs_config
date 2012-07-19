(require 'yasnippet)

(setq yas/root-directory (concat (file-name-directory load-file-name) "/snippets"))
(yas/load-directory yas/root-directory)

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

(global-set-key "\C-c\C-y" 'yas/insert-snippet)
