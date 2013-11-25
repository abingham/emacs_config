(require 'helm)
(require 'helm-elisp)
(require 'helm-utils)

(helm-mode t)

; Emulate old describe-* bindings
(global-set-key [(ctrl h) (f)] 'helm-apropos)
(global-set-key [(ctrl h) (v)] 'helm-apropos)

; Leave execute-extended-command alone. I like the normal behavior for now.
(add-to-list 'helm-completing-read-handlers-alist
             '(execute-extended-command . nil))



