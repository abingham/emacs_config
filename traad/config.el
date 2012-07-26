(add-to-list 'load-path "~/projects/traad/elisp")

(load "traad.el")

(require 'auto-complete)

(defun ac-traad-find ()
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defvar ac-traad-cache nil)

(defun ac-traad-candidates ()
  (let* ((real-point (ac-traad-find))
	 (pt (if real-point real-point (point))))
    (setq ac-traad-cache (traad-code-assist pt))
    (mapcar 'car ac-traad-cache)))

(defun ac-traad-documentation (sym)
  (let ((entry (assoc sym ac-traad-cache)))
    (if entry (cadr entry))))

(ac-define-source traad
  '((depends traad)
    (candidates . ac-traad-candidates)
    (document . ac-traad-documentation)
    (symbol . "s")
    (requires . 0)))

(defun ac-traad-setup ()
  ;(setq ac-sources (append '(ac-source-traad) ac-sources)))
  (setq ac-sources '(ac-source-traad)))

(add-hook 'python-mode-hook 'ac-traad-setup)

