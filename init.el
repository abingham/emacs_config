(require 'package)
(package-initialize)

(require 'use-package)

; Add the necessary package repositories
(add-to-list 'package-archives '("local-dir" . "/Users/abingham/projects/melpa/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c C-u SPC" . ace-jump-char-mode)
	 ("C-c C-u C-u SPC" . ace-jump-line-mode)))

(use-package auto-complete
  :bind (("M-/" . auto-complete))
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
    (use-package auto-complete-config)
    (setq ac-auto-start 2)
    (global-auto-complete-mode t)
    (setq ac-use-quick-help t)
    (setq ac-quick-help-delay 0.3)))

(use-package cleanup-buffer
  :load-path "elisp"
  :config
  (cleanup-buffer-setup))

(use-package clojure-mode
  :config
  (progn
    (use-package clojure-test-mode)
    (use-package cider)
    (use-package paredit)
    (set-variable 'cider-auto-select-error-buffer nil)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)))

(use-package company
  :config
  (setq company-backends
	'(company-ycmd
	  company-bbdb
	  company-nxml
	  company-css
	  company-eclim
	  company-semantic
	  company-xcode
	  ;; company-ropemacs
	  company-cmake
	  company-capf
	  (company-dabbrev-code company-gtags company-etags company-keywords)
	  company-oddmuse
	  company-files
	  company-dabbrev)))

(use-package ycmd
  :load-path "/Users/sixtynorth/projects/emacs-ycmd"
  :bind (("C-c y g" . ycmd-goto))
  :config
  (progn
    (use-package anaphora)
    (use-package company-ycmd
      ;; We're trying out ycmd. No need for original clang support.
      :config (setq company-backends (remove 'company-clang company-backends)))
    
    (use-package flycheck-ycmd
      :config (flycheck-ycmd-setup))
        
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (add-hook 'python-mode-hook 'ycmd-mode)
    (setq ycmd--log-enabled t)
    (set-variable 'ycmd-server-command '("/usr/bin/python" "/Users/sixtynorth/projects/ycmd/ycmd"))
    (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*" "~/sandbox/*"))
    (set-variable 'ycmd-global-config
		  (concat (file-name-directory load-file-name)
			  "ycm_global_conf.py"))))

(use-package simple-bookmark
  :load-path "elisp"
  :bind (([(shift return)] . simple-bookmark-set)
	 ([(control return)] . simple-bookmark-jump)))

(load-file "misc.el")
;;; init.el ends here
