(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(set-variable 'flycheck-disabled-checkers
              '(c/c++-clang
                c/c++-gcc
                c/c++-cppcheck
                python-flake8
                python-pylint))



