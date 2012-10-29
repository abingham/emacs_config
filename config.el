; -*- lisp -*-

;; (setq debug-on-error t)
					; allow loading code from
					; ./elisp
(add-to-list 'load-path 
	     (concat 
	      (file-name-directory load-file-name) 
	      "/elisp"))

;; Key bindings
(global-set-key [(ctrl x) (i)]    'windmove-up)
(global-set-key [(ctrl x) (m)]  'windmove-down)
(global-set-key [(ctrl x) (j)]  'windmove-left)
(global-set-key [(ctrl x) (l)] 'windmove-right)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word) 
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-j" 'copy-region-as-kill)
(global-set-key "\C-x\C-n" 'compile)
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key (kbd "<mouse-5>") 'up-slightly)
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key "\C-l" 'line-to-top-of-window)
(global-set-key [f9] 'match-paren)
(global-set-key [f10] 'start-kbd-macro)
(global-set-key [f11] 'end-kbd-macro)
(global-set-key [f12] 'call-last-kbd-macro)


;; display and behavior
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode 1)
(setq inhibit-splash-screen 't)
(global-auto-revert-mode 't)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(setq visible-bell t)
(global-linum-mode 1)

;; Compilation stuff
(setq-default display-buffer-reuse-frames t)
(setq compile-command "scons -u")
(setq compilation-scroll-output 't)
(defun my-compilation-mode-hook ()
  (define-key compilation-mode-map "\C-x\C-n" 'next-error)
  (define-key compilation-mode-map "\C-x\C-p" 'previous-error)
  )
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;; Misc. other stuff
(setq c-basic-offset 2)
(require 'cl)
(require 'rtf-mode)
(require 'open-next-line)
(require 'tramp)
(setq tramp-default-method "ssh")

(fset 'yes-or-no-p 'y-or-n-p)           ; easy answer to stupid question

;; --- mode configurations and such ---
(setq config_packages
      '("anything"
	;"autocomplete"
	; "autopair"
	"bookmarks"
	"cleanup"
        "codesearch"
	"cpp"
	;"ctags"
	"dart"
	"elemental"
	; "eproject"
	; "flymake"
	"git"
	; "go"
	;"gtags"
	"hg"
        "highlight_indentation"
	"ido"
	"multiterm"
	"p4"
	"prosjekt"
	"python"
	"rst"
	; "smart-tab"
	"traad"
	"uniquify"
	; "w3m"
	"yasnippet"
	)
      )

(mapc (lambda (dir)
	(add-to-list 'load-path (concat (file-name-directory load-file-name) "/" dir))
	(load (concat dir "/config")))
      config_packages)


;; various utility functions (mostly bound to keys above

(defun line-to-top-of-window ()
  "Scroll current line to top of window. \
Replaces three keystroke sequence C-u 0 C-l."
  (interactive)
  (recenter 0))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

					; Revert all open buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (message (number-to-string (count-matches "\\sw+"))))))

; Load any local settings.
(let* ((this_dir (file-name-directory load-file-name))
      (local_config (concat this_dir "local_config.el")))
  ; (add-to-list 'load-path this_dir)
  (if (file-exists-p local_config)
      (load local_config)
      (message (format "no local config %s" local_config))))
  ;; (condition-case nil
  ;;     (load local_config)
  ;;   (error (message ")))
