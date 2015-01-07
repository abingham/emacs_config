(use-package codesearch
  :bind
  (("\M-'" . codesearch-search)
   ("\M-." . projectile-codesearch-search))
  :config
  (set-variable 'codesearch-cindex-flags '("-exclude" "~/.csearch_excludes")))

(defun projectile-codesearch-search (pattern file-pattern)
  (interactive
   (list
    (read-string "Pattern: " (thing-at-point 'symbol))
    (read-string "File pattern: " ".*")))
  (unless (projectile-project-root) (error "Not in a projectile project."))
  (let ((fpatt (concat (projectile-project-root) file-pattern)))
    (codesearch-search pattern fpatt)))

(provide 'ab-codesearch)
;;; ab-codesearch.el ends here
