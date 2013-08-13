(require 'python-environment)

(load "traad/elisp/traad")

(defun activate-traad3 ()
  (interactive)
  (set-variable 'traad-server-port 9752)
  (set-variable 'traad-server-args '("-V" "2"))
  (set-variable 'python-environment-root "~/.emacs.d/traad/traad/venv3_json")
  (set-variable 'traad-server-program (python-environment-bin "traad3")))

(defun activate-traad2 ()
  (interactive)
  (set-variable 'traad-server-port 9752)
  (set-variable 'traad-server-args '("-V" "2"))
  (set-variable 'python-environment-root "~/.emacs.d/traad/traad/venv2")
  (set-variable 'traad-server-program (python-environment-bin "traad")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete stuff
(require 'auto-complete)

(defvar ac-traad-cache nil
  "Hold the results of the last traad-code-assist.")

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (if (traad-running?)
      (progn
	(setq ac-traad-cache (assoc-default 'completions (traad-code-assist (point))))
	(mapcar (lambda (v) (elt v 0)) ac-traad-cache))
    (setq ac-traad-cache nil)))

(defun ac-traad-documentation (sym)
  "Look up symbol documentation in the cache."
    (let ((entry (assoc sym ac-traad-cache)))
      (if entry (cadr entry))))

;; The autocomplete source for traad
(ac-define-source traad
  '((depends traad)
    (candidates . ac-traad-candidates)
    (cache)
    (document . ac-traad-documentation)
    (symbol . "s")
    (requires . 0)))

(defun ac-traad-setup ()
  "Add ac-source-traad to autocomplete list."
  (setq ac-sources (append '(ac-source-traad) ac-sources)))

;; Insert the traad source in python mode.
(add-hook 'python-mode-hook 'ac-traad-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful keybindings
(global-set-key [(ctrl x) (t) (r)] 'traad-rename)
(global-set-key [(ctrl x) (t) (u)] 'traad-undo)
(global-set-key [(ctrl x) (t) (d)] 'traad-get-definition)
(global-set-key [(ctrl x) (t) (o)] 'traad-display-doc)
(global-set-key [(ctrl x) (t) (c)] 'traad-display-calltip)
(global-set-key [(ctrl x) (t) (f)] 'traad-findit)
