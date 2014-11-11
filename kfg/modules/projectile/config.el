(require 'dash)

(projectile-global-mode)

(defun projectile-set-local-keymap (&rest bindings)
  (dolist (binding (-partition-in-steps 2 2 bindings))
    (lexical-let* ((key (car binding))
                   (cmd (cadr binding))
                   (is-interactive (interactive-form cmd))
                   (local-map (or (current-local-map) (make-keymap))))
      (define-key local-map key
        (lambda ()
          (interactive)
          (if is-interactive
              (call-interactively cmd)
            (eval cmd)))))))

(global-set-key [f6] 'projectile-run-shell-command-in-root)
