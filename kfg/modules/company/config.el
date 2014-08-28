(require 'company)

(global-set-key "\M-/" 'company-complete)

(add-hook 'after-init-hook 'global-company-mode)

; We're trying out ycmd. No need for original clang support.
(setq company-backends (remove 'company-clang company-backends))



