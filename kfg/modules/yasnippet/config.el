(require 'yasnippet)

(yas-reload-all)

(global-set-key "\C-xyi" 'yas-insert-snippet)

(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))

(yas-global-mode)
