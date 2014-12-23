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

(defun run-command-in-projectile-root (command)
  "Switch to the `projectile-project-root' and run COMMAND
interactively."
  (interactive "C")
  (let ((default-directory (projectile-project-root)))
    (call-interactively command)))

(global-set-key
 [f6]
 (lambda ()
   (interactive)
   (call-interactively
    (if (projectile-project-p)
        'projectile-run-async-shell-command-in-root
      'shell-command))))

(global-set-key [f7]
                ;; 'projectile-run-command-in-root
                'run-command-in-projectile-root)

(global-set-key
 "\C-x\C-n"
 (lambda ()
   (interactive)
   (run-command-in-projectile-root 'compile)))

