(add-to-list 'load-path (concat (file-name-directory load-file-name) "/auto-complete-1.3.1"))

(require 'auto-complete)
(global-auto-complete-mode t)
; (define-key ac-completing-map (kbd "M-TAB") 'ac-complete)
