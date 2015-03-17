(use-package color-theme
  :ensure t
  :init
  (progn
    (add-to-list 'custom-theme-load-path (file-name-directory load-file-name))
    (use-package leuven-theme)
    (load-theme 'leuven t)))

(provide 'ab-color-theme)
;; ab-color-theme.el ends here
