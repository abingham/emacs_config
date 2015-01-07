; python-version specific stuff
(defun activate-python2 ()
  (interactive)
  (setq python-shell-interpreter "ipython"
        ; jedi:server-command (list "python2.7" jedi:server-script)
        )
  ;; (set-variable 'traad-server-program '("/usr/local/bin/traad"))
  (set-variable 'traad-environment-root "traad")
  (set-variable 'traad-environment-virtualenv '("virtualenv"))
  (setup-flymake-executable "flake8"))

(defun activate-python3 ()
  (interactive)
  (setq python-shell-interpreter "ipython3"
        ;jedi:server-command (list "python3" jedi:server-script)
        )
  ;; (set-variable 'traad-server-program '("/usr/local/bin/traad3"))
  (set-variable 'traad-environment-root "traad3")
  (set-variable 'traad-environment-virtualenv '("pyvenv-3.4"))
  (setup-flymake-executable "flake8-3"))

(defun ab-python-ipython-setup ()
  (setq
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(defun setup-flymake-executable (executable-name)
  (if (executable-find executable-name)
      (setq flymake-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flymake will be disabled for Python!")))

(defun proper-python-electic-indent ()
  "The default electric-indent behavior for Python is
stupid. This does the right thing."
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun python-intelligent-fold ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^ *def " nil t)
      (let ((match-pos (match-beginning 0)))
        (save-excursion
          (goto-char match-pos)
          (yafolding-hide-element))))))

(defun python-hook ()
  (flymake-python-pyflakes-load)
  (show-paren-mode 1)
  (electric-indent-local-mode -1)
  (local-set-key (kbd "RET") 'proper-python-electic-indent)
  ; (jedi:setup)
  ;(hs-minor-mode)
  ; (python-intelligent-fold)
  )

(use-package f)
(use-package flymake)
(use-package flymake-python-pyflakes)
(use-package jedi)
(use-package python-pep8)
(use-package python-pylint)
(use-package request-deferred)

(use-package python
  :init
  (use-package traad
    :load-path "/Users/sixtynorth/projects/traad/elisp")

  :bind
  (([(ctrl x) (t) (r)] . traad-rename)
   ([(ctrl x) (t) (u)] . traad-undo)
   ([(ctrl x) (t) (d)] . traad-goto-definition)
   ([(ctrl x) (t) (o)] . traad-display-doc)
   ([(ctrl x) (t) (c)] . traad-display-calltip))

  :config
  (progn
    (set-variable 'traad-server-port 0)
    (set-variable 'traad-server-args '("-V" "2"))
    (setq python-indent-offset 4)
    (add-hook 'python-mode-hook 'python-hook)
    (ab-python-ipython-setup)
    (activate-python3)
    ))


(provide 'ab-python)
