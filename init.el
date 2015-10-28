(require 'package)
(setq package-enable-at-startup nil)

;; Add the necessary package repositories
;; (add-to-list 'package-archives '("local-dir" . "/Users/abingham/projects/melpa/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

(desktop-save-mode 1)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;(setq use-package-verbose t)

(use-package ahg :ensure t)
(use-package anaphora :ensure t)
(use-package clojure-mode :ensure t)
(use-package color-theme
  :ensure t
  :init
  (progn
    (use-package leuven-theme)
    (load-theme 'leuven t)))
(use-package deferred :ensure t)
(use-package dockerfile-mode :ensure t)

;; Elixir stuff
(use-package elixir-mode :ensure t)
(use-package elixir-mix :ensure t)
(use-package alchemist :ensure t)

(use-package f :ensure t)
(use-package fsharp-mode
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
    (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
    (setq fsharp-compiler "/usr/local/bin/fsharpc")))

(use-package grunt
  :ensure t
  :config
  (setq grunt-base-command "/usr/local/bin/node /usr/local/bin/grunt"))

(use-package htmlize :ensure t)
(use-package markdown-mode
  :ensure t
  :init
  (progn
    (add-hook 'markdown-mode-hook 'auto-fill-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))
(use-package neotree
  :ensure t
  :disabled t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action))
(use-package python-environment :ensure t)
(use-package request :ensure t)
(use-package request-deferred :ensure t)
(use-package smart-mode-line :ensure t)
(use-package speedbar
  :config
  (progn
    (set-variable 'speedbar-use-images nil)
    (speedbar-add-supported-extension ".rst")))
  
(use-package sr-speedbar
  :ensure t
  :init
  (set-variable 'sr-speedbar-right-side nil))

(use-package projectile-speedbar
  :ensure t
  :disabled t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c C-u SPC" . ace-jump-char-mode)
	 ("C-c C-u C-u SPC" . ace-jump-line-mode)))

;; (use-package auto-complete
;;   :ensure t
;;   :bind (("M-/" . auto-complete))
;;   :config
;;   (progn
;;     (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;;     (use-package auto-complete-config)
;;     (setq ac-auto-start 2)
;;     (global-auto-complete-mode t)
;;     (setq ac-use-quick-help t)
;;     (setq ac-quick-help-delay 0.3)))

(setq local-elisp-dir (concat (file-name-as-directory (file-name-directory load-file-name)) "elisp"))

(use-package ab-cleanup-buffer
  :load-path local-elisp-dir
  :config
  (cleanup-buffer-setup))

(use-package alchemist
  :ensure t)

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
  :init
  (global-company-mode)
  :config
  (progn
    (setq company-backends
          '(;;company-ycmd
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
            company-dabbrev))
    (set-variable 'company-idle-delay 0.1)))

(use-package csharp-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package elixir-mix
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(use-package feature-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))))

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

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-flycheck
  :load-path local-elisp-dir)

(use-package haskell-mode
  :ensure t
  :bind
  (("C-c C-l" . haskell-process-load-or-reload)
   ("C-`" . haskell-interactive-bring)
   ("C-c C-t" . haskell-process-do-type)
   ("C-c C-i" . haskell-process-do-info)
   ("C-c C-c" . haskell-process-cabal-build)
   ("C-c C-k" . haskell-interactive-mode-clear)
   ("C-c c" . haskell-process-cabal))
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))
    (speedbar-add-supported-extension ".hs")
    ))

;; (use-package ycmd
;;   :load-path emacs-ycmd-root
;;   :bind (("C-c y g" . ycmd-goto))
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook
;; 	      (lambda ()
;; 		(if (not (eq major-mode 'emacs-lisp-mode))
;; 		    (ycmd-mode))))
;;     (set-variable 'ycmd-parse-conditions '(save new-line buffer-focus))
;;     (set-variable 'ycmd-idle-change-delay 0.1)
;;     (set-variable 'url-show-status nil)
;;     (set-variable 'ycmd-request-message-level -1)
;;     ;;(setq ycmd--log-enabled t)
;; ))

;; (use-package company-ycmd
;;   :load-path emacs-ycmd-root
;;   ;; We're trying out ycmd. No need for original clang support.
;;   :init (setq company-backends (remove 'company-clang company-backends)))

;; (use-package flycheck-ycmd
;;   :load-path emacs-ycmd-root
;;   :config (flycheck-ycmd-setup))

(use-package simple-bookmark
  :load-path local-elisp-dir
  :bind (([(shift return)] . simple-bookmark-set)
	 ([(control return)] . simple-bookmark-jump)))

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
  :load-path local-elisp-dir)

(use-package org
  :ensure t
  :disabled t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :init
  (setq org-log-done t)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  
  :config
  (progn
    
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

(use-package ox-reveal
  :ensure t)

;; (use-package ab-python :load-path local-elisp-dir)

;; (use-package traad
;;   :bind
;;   (([(ctrl x) (t) (r)] . traad-rename)
;;    ([(ctrl x) (t) (u)] . traad-undo)
;;    ([(ctrl x) (t) (d)] . traad-goto-definition)
;;    ([(ctrl x) (t) (o)] . traad-display-doc)
;;    ([(ctrl x) (t) (c)] . traad-display-calltip))
;;   :init
;;   (progn
;;     (require 'traad)
;;     (set-variable 'traad-server-port 0)
;;     (set-variable 'traad-server-args '("-V" "2"))
;;     (add-hook
;;      'therapy-python3-hooks
;;      (lambda ()
;;        (set-variable 'traad-environment-root "traad3")
;;        (set-variable 'traad-environment-virtualenv '("pyvenv-3.4"))))
;;     (add-hook
;;      'therapy-python2-hooks
;;      (lambda ()
;;        (set-variable 'traad-environment-root "traad")
;;        (set-variable 'traad-environment-virtualenv '("virtualenv")))))
;;   :load-path "~/projects/traad/elisp")

;; (use-package codesearch
;;   :ensure t
;;   :bind
;;   (("M-'" . codesearch-search)
;;    ("M-." . projectile-codesearch-search))
;;   :config
;;   (set-variable 'codesearch-cindex-flags '("-exclude" "~/.csearch_excludes")))

(use-package ab-cpp
  :load-path local-elisp-dir)

(use-package ab-helm
  :load-path local-elisp-dir)

(use-package helm-swoop
  :ensure t
  :config
  (progn
    ;; Change the keybinds to whatever you like :)
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
    
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
    
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)
    
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)
    
    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)
    
    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)))

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

(use-package ab-javascript
  :load-path local-elisp-dir
  :disabled t)

(use-package js2-mode
  :ensure t
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (add-hook 'js-mode-hook
	      (lambda () (flycheck-mode t)))
    (setq js2-highlight-level 3)))

(use-package ab-misc
  :load-path local-elisp-dir)

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

(use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))

(use-package p4
  :ensure t
  :disabled t)

(use-package ab-projectile
  :load-path local-elisp-dir)

(use-package ab-rst
  :load-path local-elisp-dir)

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
  :bind
  (("C-x o" . switch-window))
  :config
  (custom-set-variables '(switch-window-shortcut-style (quote qwerty))))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
			     (setq web-mode-markup-indent-offset 4)
			     (setq web-mode-code-indent-offset 4)))
  )

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
