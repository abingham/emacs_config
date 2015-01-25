(defun helm-my-buffers ()
  (interactive)
  (let* ((sources '(helm-source-projectile-projects
                    helm-source-buffers-list
                    helm-source-files-in-current-dir
                    helm-source-recentf
                    helm-source-buffer-not-found))
         (sources (if (projectile-project-p)
                      (append '(helm-source-projectile-files-dwim-list
                                helm-source-projectile-buffers-list)
                              sources)
                    sources)))
    (helm-other-buffer sources "*helm-my-buffers*")))

(use-package helm
  :ensure t
  :bind
  (("C-h f" . helm-apropos)
   ("C-h v" . helm-apropos)
   ("C-c h" . helm-command-prefix)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-c h o" . helm-occur)
   ("M-x" . helm-M-x)
   ("C-u" . helm-my-buffers))
  :init
  (progn
    (require 'helm-config)
    (use-package helm-projectile :ensure t)
    (setq
     ;; open helm buffer inside current window, not occupy whole other window
     helm-split-window-in-side-p  t
     ;; move to end or beginning of source when reaching top or bottom of source.
     helm-move-to-line-cycle-in-source t
     ;; search for library in `require' and `declare-function' sexp.
     helm-ff-search-library-in-sexp t
     ;; scroll 8 lines other window using M-<next>/M-<prior>
     helm-scroll-amount 8
     helm-ff-file-name-history-use-recentf t)

    (helm-mode 1))
  :config
  (add-to-list 'helm-completing-read-handlers-alist
               '(execute-extended-command . nil)))

(provide 'ab-helm)
