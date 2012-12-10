(load "traad/elisp/traad")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete stuff
(require 'auto-complete)

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (mapcar 'car (traad-code-assist (point))))

(defun ac-traad-documentation (sym)
  "Look up symbol documentation in the cache."
  (traad-get-doc (point)))

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
