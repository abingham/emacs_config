(require 'package)
(setq package-enable-at-startup nil)

;; Add the necessary package repositories
;; (add-to-list 'package-archives '("local-dir" . "/Users/abingham/projects/melpa/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;(setq use-package-verbose t)

(use-package ahg :ensure t)
(use-package anaphora :ensure t)
(use-package clojure-mode :ensure t)
(use-package deferred :ensure t)
(use-package f :ensure t)
(use-package htmlize :ensure t)
(use-package markdown-mode :ensure t)
(use-package neotree :ensure t)
(use-package python-environment :ensure t)
(use-package request :ensure t)
(use-package request-deferred :ensure t)
(use-package smart-mode-line :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c C-u SPC" . ace-jump-char-mode)
	 ("C-c C-u C-u SPC" . ace-jump-line-mode)))

(use-package auto-complete
  :ensure t
  :bind (("M-/" . auto-complete))
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
    (use-package auto-complete-config)
    (setq ac-auto-start 2)
    (global-auto-complete-mode t)
    (setq ac-use-quick-help t)
    (setq ac-quick-help-delay 0.3)))

(use-package ab-cleanup-buffer
  :load-path "elisp"
  :config
  (cleanup-buffer-setup))

(use-package cider
  :ensure t
  :config
  (progn
    (set-variable 'cider-auto-select-error-buffer nil)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)))

(use-package company
  :ensure t
  :defer t
  :bind
  (("M-/" . company-complete))
  :idle
  (global-company-mode)
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

(use-package exec-path-from-shell
  :ensure t
  :idle
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package fsharp-mode
  :ensure t
  :config
  (progn
    (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
    (setq fsharp-compiler "/usr/local/bin/fsharpc")))

(use-package graphviz-dot-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package ycmd
  :load-path "/Users/sixtynorth/projects/emacs-ycmd"
  :bind (("C-c y g" . ycmd-goto))
  :init
  (progn
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (add-hook 'python-mode-hook 'ycmd-mode)
    (setq ycmd--log-enabled t)
    (set-variable 'ycmd-server-command '("/usr/bin/python" "/Users/sixtynorth/projects/ycmd/ycmd"))
    (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*" "~/sandbox/*"))
    (set-variable 'ycmd-global-config
		  (concat (file-name-directory load-file-name)
			  "ycm_global_conf.py"))))

(use-package company-ycmd
  :load-path "/Users/sixtynorth/projects/emacs-ycmd"
  ;; We're trying out ycmd. No need for original clang support.
  :init (setq company-backends (remove 'company-clang company-backends)))

(use-package flycheck-ycmd
  :load-path "/Users/sixtynorth/projects/emacs-ycmd"
  :init (flycheck-ycmd-setup))

(use-package simple-bookmark
  :load-path "elisp"
  :bind (([(shift return)] . simple-bookmark-set)
	 ([(control return)] . simple-bookmark-jump)))

(use-package ido
  :ensure t
  :idle
  (progn
    (ido-mode 1)
    (ido-everywhere 1))
  :config
  (progn
    (use-package ido-vertical-mode
      :ensure t
      :init
      (ido-vertical-mode 1))
    
    (setq ido-enable-flex-matching t)))

(use-package magit
  :ensure t
  :bind (("<f5>" . magit-status)))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode t))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package multi-term
  :ensure t
  :disabled t
  :config
  (progn
    (setq multi-term-program "/bin/zsh")
    (custom-set-variables
     '(term-default-bg-color "#ffffff")
     '(term-default-fg-color "#000000"))))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode t))

(use-package open-next-line
  :load-path "elisp")

(use-package org
  :ensure t
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

(use-package traad
  :bind
  (([(ctrl x) (t) (r)] . traad-rename)
   ([(ctrl x) (t) (u)] . traad-undo)
   ([(ctrl x) (t) (d)] . traad-goto-definition)
   ([(ctrl x) (t) (o)] . traad-display-doc)
   ([(ctrl x) (t) (c)] . traad-display-calltip))
  :config
  (progn
    (set-variable 'traad-server-port 0)
    (set-variable 'traad-server-args '("-V" "2")))
  :load-path "/Users/sixtynorth/projects/traad/elisp")

(use-package ab-codesearch
  :load-path "elisp")

(use-package ab-color-theme
  :load-path "elisp")

(use-package ab-cpp
  :load-path "elisp")

(use-package ab-helm
  :load-path "elisp")

(use-package hideshow
  :ensure t
  :disabled t
  :config
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c e") 'hs-toggle-hiding)
              (local-set-key (kbd "C-c <right>") 'hide-level)
              (local-set-key (kbd "C-c <up>")    'hs-hide-all)
              (local-set-key (kbd "C-c <down>")  'hs-show-all))))

(use-package sgml-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.mak" "\\.jinja2" "\\.mustache" . html-mode))
    (add-to-list 'auto-mode-alist '("\\.html" . web-mode))))

(use-package ab-javascript
  :load-path "elisp")

(use-package ab-misc
  :load-path "elisp")

(use-package outline-presentation
  :ensure t
  :disabled t
  :bind
  (("C-c ." . outline-presentation-next)
   ("C-c ," . outline-presentation-previous)))

(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode))

(use-package p4
  :ensure t
  :disabled t)

(use-package ab-projectile
  :load-path "elisp")

(use-package ab-python
  :load-path "elisp")

(use-package ab-rst
  :load-path "elisp")

(use-package slime
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup)))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (sml/setup)
    
    ;; TODO: Think about letting the individual modules do this
    ;; configuration. It's convenient to have it here for now, though.
    (dolist (m '("Helm" "AC" "Undo-Tree" "Paredit"))
      (add-to-list 'sml/hidden-modes (concat " " m)))))

(use-package switch-window
  :ensure t
  :disabled t
  :bind
  (("C-x o" . switch-window))
  :config
  (custom-set-variables '(switch-window-shortcut-style (quote qwerty))))

(use-package undo-tree
  :ensure t
  :idle (global-undo-tree-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package yafolding-mode
  :ensure t
  :disabled true
  :config
  (progn
    ;; (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
    ;; (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
    ;; (define-key yafolding-mode-map (kbd "<C-return>") nil)
    (define-key yafolding-mode-map (kbd "C-c <right>") 'yafolding-show-element)
    (define-key yafolding-mode-map (kbd "C-c <left>") 'yafolding-hide-element)
    
    (add-hook 'prog-mode-hook
              (lambda () (yafolding-mode)))))

(use-package yasnippet
  :ensure t
  :bind
  (("C-x y i" . yas-insert-snippet))
  :config
  (progn
    (yas-reload-all)
    (setq yas-prompt-functions '(yas-ido-prompt
                                 yas-completing-prompt))
    (yas-global-mode)))

;;; init.el ends here
