(require 'f)
; (require 'jedi)
(add-to-list 'load-path "/Users/sixtynorth/projects/traad/elisp")

(set-variable 'traad-server-port 0)
(set-variable 'traad-server-args '("-V" "2"))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))
(setq python-indent-offset 4)

;; flymake

(defun setup-flymake-executable (executable-name)
  (if (executable-find executable-name)
      (setq flymake-python-pyflakes-executable executable-name)
    (warn "No python flake8 executable found. Flymake will be disabled for Python!")))

;;; old, but interesting function
;; (let* ((candidates '("~/bin/flymake_python.sh" "pyflakes3" "epylint" "pep8"))
;;        (available (remove-if-not 'executable-find candidates)))
;;   (if available
;;       (setq flymake-python-pyflakes-executable "~/bin/flymake_python.sh")
;;     (warn "No python flymake executables available.")))

(defun proper-python-electic-indent ()
  "The default electric-indent behavior for Python is
stupid. This does the right thing."
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun python-intelligent-fold ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^ *def " nil t)
      (let ((match-pos (match-beginning 0)))
        (save-excursion
          (goto-char match-pos)
          (yafolding-hide-element))))))

(defun python-hook ()
  (flymake-python-pyflakes-load)
  (show-paren-mode 1)
  (electric-indent-local-mode -1)
  (local-set-key (kbd "RET") 'proper-python-electic-indent)
  ; (jedi:setup)
  ;(hs-minor-mode)
  ; (python-intelligent-fold)
  )

(add-hook 'python-mode-hook 'python-hook)
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; (add-hook 'python-mode-hook (lambda () (show-paren-mode t)))
;; (add-hook 'python-mode-hook (lambda () (electric-indent-mode t)))

;; Pylookup stuff
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir (f-join (file-name-directory load-file-name)
                           "pylookup"))
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

; IPython stuff (python-version independent)
(setq
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

; python-version specific stuff
(defun activate-python2 ()
  (interactive)
  (setq python-shell-interpreter "ipython"
        ; jedi:server-command (list "python2.7" jedi:server-script)
        )
  ;; (set-variable 'traad-server-program '("/usr/local/bin/traad"))
  (set-variable 'traad-environment-root "traad")
  (set-variable 'traad-environment-virtualenv '("virtualenv"))
  (setup-flymake-executable "flake8"))

(defun activate-python3 ()
  (interactive)
  (setq python-shell-interpreter "ipython3"
        ;jedi:server-command (list "python3" jedi:server-script)
        )
  ;; (set-variable 'traad-server-program '("/usr/local/bin/traad3"))
  (set-variable 'traad-environment-root "traad3")
  (set-variable 'traad-environment-virtualenv '("pyvenv-3.4"))
  (setup-flymake-executable "flake8-3"))

;; traad keybindings
(global-set-key [(ctrl x) (t) (r)] 'traad-rename)
(global-set-key [(ctrl x) (t) (u)] 'traad-undo)
(global-set-key [(ctrl x) (t) (d)] 'traad-goto-definition)
(global-set-key [(ctrl x) (t) (o)] 'traad-display-doc)
(global-set-key [(ctrl x) (t) (c)] 'traad-display-calltip)

;; Jedi setup
; (add-hook 'python-mode-hook 'jedi:setup)
; (setq jedi:setup-keys t)
; (setq jedi:complete-on-dot t)

;; default to python3
(activate-python3)
