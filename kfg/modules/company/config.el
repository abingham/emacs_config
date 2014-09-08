(require 'cc-cmds)
(require 'company)

(global-set-key "\M-/" 'company-complete)

(add-hook 'after-init-hook 'global-company-mode)




