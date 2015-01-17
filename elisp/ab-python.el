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

(require 'dash)

(defun ab-python-major-version ()
  "Find major version of current Python interpreter."
  (substring
   (shell-command-to-string
    (format "%s -c \"import sys; print(sys.version_info.major)\"" python-shell-interpreter))
   0 1))

(defun ab-python-find-executables (options)
  "Find available executables from OPTIONS."
  (delq nil (mapcar 'executable-find options)))

(defun ab-python-setup-pyflakes-executable (executable-name)
  (if (executable-find executable-name)
      (setq flycheck-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flycheck will be disabled for Python!")))

(defcustom ab-python-python2-hooks nil
  "Hook for when Python 2 is activated."
  :group 'ab-python
  :type 'hook)

(defcustom ab-python-python3-hooks nil
  "Hook for when Python 3 is activated."
  :group 'ab-python
  :type 'hook)

(defun ab-python-activate-python2 ()
  "Hook run when entering python2 environment."
  (message "Activating Python 2 toolset.")
  (set-variable 'python-shell-interpreter (first (ab-python-find-executables '("ipython" "python"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  (ab-python-setup-pyflakes-executable "flake8"))

(add-hook
 'ab-python-python2-hooks
 'ab-python-activate-python2)

(defun ab-python-activate-python3 ()
  "Hook run when entering python3 environment."
  (message "Activating Python 3 toolset.")
  (set-variable 'python-shell-interpreter (first (ab-python-find-executables '("ipython3" "python3"))))
  (unless python-shell-interpreter (warn "No Python executable found!"))
  (ab-python-setup-pyflakes-executable "flake8-3"))

(add-hook
 'ab-python-python3-hooks
 'ab-python-activate-python3)

(defun ab-python-hook ()
  (show-paren-mode 1)
  ;; (electric-indent-local-mode -1)
  ;; (local-set-key (kbd "RET") 'proper-python-electic-indent)

  ;; We're using flycheck-pyflakes
  (--map (add-to-list 'flycheck-disabled-checkers it)
         '(python-flake8 python-pylint ycmd)))

(defcustom
  ab-python-patterns
  '("\\.py" "wscript" "SConstruct" "SConsign")
  "File patterns that get put into Python mode.")

;; (use-package python
;;   :init
;;   (progn
;;     (dolist (pattern ab-python-patterns)
;;       (add-to-list 'auto-mode-alist `(,pattern . python-mode)))

;;     (activate-python3))

;;   :config
;;   (progn
;;     (use-package f :ensure t)
;;     (use-package jedi :ensure t)
;;     (use-package flycheck-pyflakes :ensure t)
;;     (use-package python-pep8 :ensure t)
;;     (use-package python-pylint :ensure t)
;;     (setq python-indent-offset 4)
;;     (add-hook 'python-mode-hook 'python-hook)
;;     (ab-python-ipython-setup)))

(use-package python
  :init
  (progn
    (dolist (pattern ab-python-patterns)
      (add-to-list 'auto-mode-alist `(,pattern . python-mode)))
    (setq python-indent-offset 4)
    (run-hooks 'ab-python-python3-hooks))

  :config
  (progn
    (add-hook 'python-mode-hook 'ab-python-hook)))

(use-package flycheck-pyflakes :ensure t)
(use-package jedi :ensure t)

(defun ab-python-pyvenv-hook ()
  "Hook for when we change virtual environments."

  ;; Do this so that we're sure to pick up the venv's interpreter.
  (set-variable 'python-shell-interpreter (executable-find "python"))

  (pyvenv-restart-python)

  ;; Activate the right toolset based on the detected major version.
  (if (string-equal "3" (ab-python-major-version))
      (run-hooks 'ab-python-python3-hooks)
    (run-hooks 'ab-python-python2-hooks)))

(use-package pyvenv
  :ensure t
  :init
  (add-hook 'pyvenv-post-activate-hooks
            'ab-python-pyvenv-hook))

(provide 'ab-python)

;;; ab-python.el ends here
