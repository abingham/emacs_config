(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes3" (list local-file)))) ;; using bitbucket.org/prologic/pyflakes
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" . 'flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)