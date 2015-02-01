;;; ab-python --- my python configuration
;;; Commentary:

;; We have some "global" things that are independent of venv:
;;  - traad
;;  - pyflakes
;;  - ...maybe more...
;;
;; But these *are* dependent on the major version of Python. When we
;; initially start the config should specify Python 2 or Python
;; 3. This way we know which stuff to use in the absence of a
;; virtualenv. When a virtualenv is selected, we should determine the
;; major version and activate the right tools.
;;
;;

;;; Code:

(use-package dash :ensure t)
(use-package flycheck :ensure t)
(use-package python :ensure t)
(use-package pyvenv :ensure t)
(use-package therapy :load-path "~/projects/therapy")

(defun ab-python-find-executables (options)
  "Find available executables from OPTIONS."
  (delq nil (mapcar 'executable-find options)))

(defun ab-python-setup-pyflakes-executable (executable-name)
  (if (executable-find executable-name)
      (setq flycheck-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flycheck will be disabled for Python!")))

(defun ab-setup-therapy-tests ()
  "Set the therapy-test-command based on the current environment."
  (let ((python-command (if pyvenv-virtual-env
                            (f-join pyvenv-virtual-env "bin" "python")
                          python-shell-interpreter)))
    (set-variable 'therapy-test-command
                  (format "%s -m nose" python-command))))

(defun ab-python-activate-python2 ()
  "Hook run when entering python2 environment."
  (message "Activating Python 2 toolset.")
  (set-variable 'python-shell-interpreter (first (ab-python-find-executables '("ipython" "python"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  (ab-setup-therapy-tests)
  (ab-python-setup-pyflakes-executable "flake8"))

(add-hook
 'therapy-python2-hooks
 'ab-python-activate-python2)

(defun ab-python-activate-python3 ()
  "Hook run when entering python3 environment."
  (message "Activating Python 3 toolset.")
  (set-variable 'python-shell-interpreter (first (ab-python-find-executables '("ipython3" "python3" "python"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  (ab-setup-therapy-tests)
  (ab-python-setup-pyflakes-executable "flake8-3"))

(add-hook
 'therapy-python3-hooks
 'ab-python-activate-python3)

(defun ab-python-hook ()
  "Called when a buffer enters python mode."
  (show-paren-mode 1)
  ;; (electric-indent-local-mode -1)
  ;; (local-set-key (kbd "RET") 'proper-python-electic-indent)

  ;; We're using flycheck-pyflakes
  (--map (add-to-list 'flycheck-disabled-checkers it)
         '(python-flake8 python-pylint ycmd)))

(defgroup ab-python nil
  "Python-related emacs config stuff."
  :group 'tools
  :group 'programming)

(defcustom ab-python-patterns
  '("\\.py" "wscript" "SConstruct" "SConsign")
  "File patterns that get put into Python mode."
  :group 'ab-python
  :type '(repeat string))

(use-package python
  :init
  (progn
    (dolist (pattern ab-python-patterns)
      (add-to-list 'auto-mode-alist `(,pattern . python-mode)))
    (setq python-indent-offset 4)
    (therapy-set-python-interpreter "/usr/local/bin/ipython3"))

  :config
  (progn
    (add-hook 'python-mode-hook 'ab-python-hook)))

(use-package flycheck-pyflakes :ensure t)
(use-package jedi :ensure t)

(defun ab-python-pyvenv-hook ()
  "Hook for when we change virtual environments."

  ;; Do this so that we're sure to pick up the venv's interpreter.
  (therapy-set-python-interpreter (executable-find "python"))

  ;; Activate the right toolset based on the detected major version.
  (therapy-interpreter-changed))

(use-package pyvenv
  :ensure t
  :init
  (add-hook 'pyvenv-post-activate-hooks
            'ab-python-pyvenv-hook))

(provide 'ab-python)

;;; ab-python.el ends here
