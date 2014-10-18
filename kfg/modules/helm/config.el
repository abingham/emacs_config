; helm configuration

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)


;; ; Emulate old describe-* bindings
(global-set-key [(ctrl h) (f)] 'helm-apropos)
(global-set-key [(ctrl h) (v)] 'helm-apropos)

;; Leave execute-extended-command alone. I like the normal behavior for now.
(add-to-list 'helm-completing-read-handlers-alist
             '(execute-extended-command . nil))

(defun helm-my-buffers ()
  (interactive)
  (let* ((sources '(helm-source-projectile-projects
		    helm-source-buffers-list
		    helm-source-files-in-current-dir
		    helm-source-recentf
		    helm-source-buffer-not-found))
	 (sources (if (projectile-project-p)
		      (append '(helm-source-projectile-files-list
				helm-source-projectile-buffers-list)
			      sources)
		    sources)))
    (helm-other-buffer sources "*helm-my-buffers*")))

(global-set-key "\C-u" 'helm-my-buffers)


