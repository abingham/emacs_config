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

(use-package helm
  :bind
  (("C-h f" . helm-apropos)
   ("C-h v" . helm-apropos)
   ("C-u" . hdlm-my-buffers))
  :config
  (progn
    (use-package helm-config)
    (helm-mode 1)
    (use-package helm-projectile)
    (add-to-list 'helm-completing-read-handlers-alist
                 '(execute-extended-command . nil))))

(provide 'ab-helm)
