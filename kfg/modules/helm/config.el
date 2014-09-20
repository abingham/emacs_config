; helm configuration

(helm-mode t)

; Emulate old describe-* bindings
(global-set-key [(ctrl h) (f)] 'helm-apropos)
(global-set-key [(ctrl h) (v)] 'helm-apropos)

; Leave execute-extended-command alone. I like the normal behavior for now.
(add-to-list 'helm-completing-read-handlers-alist
             '(execute-extended-command . nil))

(defun helm-my-buffers ()
  (interactive)
  (helm-other-buffer '(helm-c-source-prosjekt-files
		       helm-source-buffers-list
                       helm-source-files-in-current-dir
                       helm-source-recentf
                       helm-source-buffer-not-found)
                     "*helm-my-buffers*"))
(global-set-key "\C-u" 'helm-my-buffers)
