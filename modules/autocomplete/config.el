(require 'auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 2)
(global-auto-complete-mode t)
(setq ac-quick-help-delay 0.3)
(global-set-key "\M-/" 'auto-complete)

