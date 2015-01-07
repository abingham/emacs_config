(require 'dash)

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

(defun run-compile-in-projectile-root ()
  (interactive)
  (run-command-in-projectile-root 'compile))

(defun shell-command-at-root ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'projectile-run-async-shell-command-in-root
     'shell-command)))

(use-package projectile
  :init
  (projectile-global-mode)
  
  :bind
  (("<f7>" . run-command-in-projectile-root)
   ("C-x C-n" . run-compile-in-projectile-root)
   ("<f6>" . shell-command-at-root)))

(provide 'ab-projectile)
