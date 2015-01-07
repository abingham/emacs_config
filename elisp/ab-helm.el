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
  :ensure t
  :bind
  (("C-h f" . helm-apropos)
   ("C-h v" . helm-apropos)
   ("C-u" . helm-my-buffers))
  :init
  (progn
    (require 'helm-config)
    (use-package helm-projectile :ensure t)
    (helm-mode 1))
  :config
  (add-to-list 'helm-completing-read-handlers-alist
               '(execute-extended-command . nil)))

(provide 'ab-helm)
