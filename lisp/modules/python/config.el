(require 'jedi)
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

; Look for the executable
(let ((fm-bin "~/bin/flymake_python.sh"))
  (if (executable-find fm-bin)
      (setq flymake-python-pyflakes-executable fm-bin)
    (warn "No python flymake executable found!")))

;;; old, but interesting function
;; (let* ((candidates '("~/bin/flymake_python.sh" "pyflakes3" "epylint" "pep8"))
;;        (available (remove-if-not 'executable-find candidates)))
;;   (if available
;;       (setq flymake-python-pyflakes-executable "~/bin/flymake_python.sh")
;;     (warn "No python flymake executables available.")))

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook (lambda () (show-paren-mode t)))

;; Pylookup stuff
;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir (kfg:join (file-name-directory load-file-name)
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

; jedi stuff
(defun activate-jedi2 ()
  (interactive)
  (setq jedi:server-command
	(list "python2.7" jedi:server-script))
)

(defun activate-jedi3 ()
  (interactive)
  (setq jedi:server-command
	(list "/usr/local/bin/python3" jedi:server-script)))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(activate-jedi3)

