(set-variable 'cider-auto-select-error-buffer nil)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
; (add-hook 'clojure-mode-hook 'ac-nrepl-setup)
