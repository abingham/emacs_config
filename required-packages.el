(require 'cl)
(require 'package)

(package-initialize)

(defvar required-packages
  '(ahg
    anything 
    auto-complete 
    fuzzy-match
    magit
    multi-term
    python
    python-pep8
    python-pylint
    p4
    tree-mode
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(defun required-packages-installed-p ()
  (every (lambda (x) x) (mapcar 'package-installed-p required-packages)))

(unless (required-packages-installed-p)
  (setq package-archives 
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("marmalade" . "http://marmalade-repo.org/packages/")
	  ("melpa" . "http://melpa.milkbox.net/packages/")))
  ;;check for new packages (package versions)
  (message "%s" "Missing packages detected. Refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'required-packages)