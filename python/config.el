(require 'python)
(require 'python-pep8)
(require 'flymake-python-pyflakes)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))
(setq python-indent-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake

; Find a suitable executable
(let* ((candidates '("~/bin/flymake_python.sh" "pyflakes3" "epylint" "pep8"))
       (available (remove-if-not 'executable-find candidates)))
  (if available
      (setq flymake-python-pyflakes-executable "~/bin/flymake_python.sh")
    (warn "No python flymake executables available.")))

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook (lambda () (show-paren-mode t)))
