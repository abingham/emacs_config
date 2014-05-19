(require 'yasnippet)

(yas-load-directory (concat (file-name-directory load-file-name) "/snippets"))

(global-set-key "\C-xyi" 'yas-insert-snippet)
