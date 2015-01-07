(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e") 'hs-toggle-hiding)
            (local-set-key (kbd "C-c <right>") 'hide-level)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)))
