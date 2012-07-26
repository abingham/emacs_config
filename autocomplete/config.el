(add-to-list 'load-path (concat (file-name-directory load-file-name) "/auto-complete-1.3.1"))
(require 'auto-complete)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
;(define-key ac-completing-map (kbd "M-TAB") 'ac-complete)
