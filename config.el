
; -*- lisp -*-

;; (setq debug-on-error t)

					; We can optionally load a minimal config
(setq minimal-config (member "-min" command-line-args))
(setq command-line-args (delete "-min" command-line-args))


					; allow loading code from
					; ./elisp
(add-to-list 'load-path 
	     (concat 
	      (file-name-directory load-file-name) 
	      "/elisp"))

					; bring in common lisp so
					; that, e.g., cdb-gud works
(require 'cl)

(setq visible-bell t)

					; Experiment with
					; autocompletion
; (global-set-key [(meta a)] 'dabbrev-expand)

					; ctrl-x arrow-key moves you
					; between frames
(global-set-key [(ctrl x) (i)]    'windmove-up)
(global-set-key [(ctrl x) (m)]  'windmove-down)
(global-set-key [(ctrl x) (j)]  'windmove-left)
(global-set-key [(ctrl x) (l)] 'windmove-right)

					; we don't need to stinking
					; toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode 1)
(setq inhibit-splash-screen 't)
(global-auto-revert-mode 't)

					; yegge's suggestions...
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-j" 'copy-region-as-kill)

					; always show parens
(show-paren-mode 1)

					; hippie-expand
; (global-set-key [(ctrl tab)] 'hippie-expand)

					; make compilation simpler
(global-set-key "\C-x\C-n" 'compile)

					; Reuse existing compilation
					; buffer frame if it exists
(setq-default display-buffer-reuse-frames t)

					; wheel scrolling moves by 5
					; rather than 1.
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key (kbd "<mouse-5>") 'up-slightly)
(global-set-key (kbd "<mouse-4>") 'down-slightly)

(column-number-mode 1)
(line-number-mode 1)
(global-set-key "\C-l" 'line-to-top-of-window)

					; I really would rather have
					; this be triggered from a
					; mode hook, but that seems to
					; be not working for some
					; reason
(setq c-basic-offset 2)

					; I'm almost always using
					; scons, so...
(setq compile-command "scons -u")

					; follow compilation output by
					; default
(setq compilation-scroll-output 't)

;; --- mode configurations and such ---
(setq config_packages
      (if minimal-config
					; minimal config
	  '("anything"
	    "cpp"
	    "python"
	    "rst"
	    "uniquify")
					; full config
	
	'("anything"
	  ;"autopair"
	  ;"bookmarks"
	  "cleanup"
	  "cpp"
	  "ctags"
 	  "elemental"
	  ; "eproject"
	  ; "flymake"
	  "git"
	  ;"go"
	  "hg"
	  "ido"
	  "multiterm"
	  "p4"
	  "prosjekt"
	  "python"
	  "rst"
	  "smart-tab"
	  "traad"
	  "uniquify"
	  "w3m"
	  "yasnippet"
	  )
))

(mapc (lambda (dir)
	(add-to-list 'load-path (concat (file-name-directory load-file-name) "/" dir))
	(load (concat dir "/config")))
      config_packages)

(require 'rtf-mode)
(require 'open-next-line)

(require 'tramp)
(setq tramp-default-method "ssh")

; javascript mode
;(autoload 'js2-mode "js2" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; setup some error traversal keys the way I like 'em
(defun my-compilation-mode-hook ()
  (define-key compilation-mode-map "\C-x\C-n" 'next-error)
  (define-key compilation-mode-map "\C-x\C-p" 'previous-error)
  )
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

					; This remaps C-l to move the
					; current line to the top of
					; the screen rather than the
					; middle (I mean, who needs
					; *that*?)
(defun line-to-top-of-window ()
  "Scroll current line to top of window.
          Replaces three keystroke sequence C-u 0 C-l."
  (interactive)
  (recenter 0))


					; bouncing between matching
					; parens
(global-set-key [f9] 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


					; easy keyboard macros
(global-set-key [f10] 'start-kbd-macro)
(global-set-key [f11] 'end-kbd-macro)
(global-set-key [f12] 'call-last-kbd-macro)


(defun fix-compilation-buffer ()
  "Replace scon's build-root with our normal source root in the
compilation buffer, allowing us to jump to errors naturally."
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    ; replace "build" with "workspace"
    (goto-char (point-min))
    (perform-replace
     "/build/"
     "/git/"
     nil   ; query-flag
     1     ; regex-flag
     nil)  ; delimited flag

    ; Remove build variant
    (goto-char (point-min))
    (perform-replace
     "/windows-[^/]*/"
     "/"
     nil 1 nil)

    (goto-char (point-min))
    (perform-replace
     "/linux-[^/]*/"
     "/"
     nil 1 nil)))
(global-set-key [f8] 'fix-compilation-buffer)


					; f3 -> switch to the
					; compilation buffer
(defun switch-to-compilation ()
  "switch to compilation buffer"
  (interactive)
  (switch-to-buffer "*compilation*"))
(global-set-key [f3] 'switch-to-compilation)


					; f4 -> switch to the gud
					; buffer (assumes that there
					; is only one!)
(fset 'switch-to-gud
   [?\C-x ?b ?* ?g ?u ?d tab return])
(global-set-key [f4] 'switch-to-gud)


					; Revert all open buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

;; moinmoin mode
; (require 'moinmoin-mode)


(defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (message (number-to-string (count-matches "\\sw+"))))))
