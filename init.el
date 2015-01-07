(require 'package)
;; (package-refresh-contents)
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
    (use-package cider)
    (use-package paredit)
    (set-variable 'cider-auto-select-error-buffer nil)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (("M-/" . company-complete))
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

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (set-variable 'flycheck-disabled-checkers
                '(c/c++-clang
                  c/c++-gcc
                  c/c++-cppcheck
                  python-flake8
                  python-pylint)))

(use-package fsharp-mode
  :config
  (progn
    (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
    (setq fsharp-compiler "/usr/local/bin/fsharpc")))

(use-package graphviz-dot-mode)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

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

(use-package ahg)

(use-package ido
  :config
  (progn
    (use-package ido-vertical-mode)
    (setq ido-enable-flex-matching t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)))

(use-package magit
  :bind (("<f5>" . magit-status)))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package multi-term
  :disabled t
  :config
  (progn
    (setq multi-term-program "/bin/zsh")
    (custom-set-variables
     '(term-default-bg-color "#ffffff")
     '(term-default-fg-color "#000000"))))

(use-package neotree
  :ensure t)

(use-package htmlize)

(use-package org
  :config
  (progn

    ;; (use-package ox-reveal
    ;;   :ensure t)
    
    (setq org-src-fontify-natively t)
    
    (defface org-block-begin-line
      '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#222222")))
      "Face used for the line delimiting the begin of source blocks.")
    
    (defface org-block-background
      '((t (:background "#000000")))
      "Face used for the source block background.")
    
    (defface org-block-end-line
      '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#222222")))
      "Face used for the line delimiting the end of source blocks.")))

(use-package ab-codesearch
  :load-path "elisp")

(use-package ab-color-theme
  :load-path "elisp")

(use-package ab-cpp
  :load-path "elisp")

(use-package ab-helm
  :load-path "elisp")

(use-package hideshow
  :disabled t
  :config
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c e") 'hs-toggle-hiding)
              (local-set-key (kbd "C-c <right>") 'hide-level)
              (local-set-key (kbd "C-c <up>")    'hs-hide-all)
              (local-set-key (kbd "C-c <down>")  'hs-show-all))))

(use-package sgml-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.mak" "\\.jinja2" "\\.mustache" . html-mode))
    (add-to-list 'auto-mode-alist '("\\.html" . web-mode))))

(use-package ab-javascript
  :load-path "elisp")

(use-package ab-misc
  :load-path "elisp")

(use-package outline-presentation
  :disabled t
  :bind
  (("C-c ." . outline-presentation-next)
   ("C-c ," . outline-presentation-previous)))

(use-package p4
  :disabled t)

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode))

(use-package ab-projectile
  :load-path "elisp")

(use-package ab-python
  :load-path "elisp")

(use-package ab-rst
  :load-path "elisp")

(use-package slime
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup)))

(use-package smart-mode-line
  :config
  (progn
    (sml/setup)
    
    ;; TODO: Think about letting the individual modules do this
    ;; configuration. It's convenient to have it here for now, though.
    (dolist (m '("Helm" "AC" "Undo-Tree" "Paredit"))
      (add-to-list 'sml/hidden-modes (concat " " m)))))

(use-package switch-window
  :disabled t
  :bind
  (("C-x o" . switch-window))
  :config
  (custom-set-variables '(switch-window-shortcut-style (quote qwerty))))

;; ;;; init.el ends here
