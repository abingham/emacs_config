(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat (file-name-directory load-file-name) "/snippets"))

(setq yas/prompt-functions '(yas/completing-prompt yas/x-prompt))

(global-set-key "\C-c\C-y" 'yas/insert-snippet)
