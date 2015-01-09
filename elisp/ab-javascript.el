(defun my-flymake-js-command-factory ()
  (message load-file-name)
  (lexical-let ((root (file-name-directory load-file-name)))
    (lambda (filename)
      (list "rhino" (concat root "jslint.js") filename))))

(defun my-flymake-js-load ()
  (dolist (ext '("\\.js$"))
    (flymake-easy-load my-flymake-js-command
		       flymake-js-err-line-patterns
		       'inplace
		       ext
		       "^W")))

(use-package flymake-easy
  :ensure t
  :config
  (progn
    (setq my-flymake-js-command (my-flymake-js-command-factory))
    (setq flymake-js-err-line-patterns 
          (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"  
                  nil 1 2 3)
                flymake-err-line-patterns))
    (add-hook 'js-mode-hook 'my-flymake-js-load)))

(provide 'ab-javascript)
