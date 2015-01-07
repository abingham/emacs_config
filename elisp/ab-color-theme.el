(use-package color-theme
  :config
  (progn
    ;; (use-package color-theme-solarized)
    ;; (use-package zenburn-theme)
    (add-to-list 'custom-theme-load-path (file-name-directory load-file-name))
    (load-theme 'zorkon t)))

(provide 'ab-color-theme)
;; ab-color-theme.el ends here
