(defun my-rst-hook ()
  (auto-fill-mode t)
  ;(set-face-background rst-level-1-face "grey20")
  ;(set-face-background rst-level-2-face "grey20")
  ;(set-face-background rst-level-3-face "grey20")
  ;(set-face-background rst-level-4-face "grey20")
  (flyspell-mode t))

(use-package rst
  :config
  (progn
    ;; (autoload 'rst-mode
    ;;   "rst" "Edit Restructured Text documents" t)
    (setq auto-mode-alist
          (append '(("\\.txt$" . rst-mode)
                    ("\\.rst$" . rst-mode)
                    ("\\.rest$" . rst-mode)) auto-mode-alist))
    (setq rst-mode-lazy nil)
    (add-hook 'rst-mode-hook 'my-rst-hook)))

(provide 'ab-rst)
