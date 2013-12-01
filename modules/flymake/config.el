(require 'flymake)

(global-set-key [(ctrl c) (f)] 'flymake-display-err-menu-for-current-line)
(global-set-key [(ctrl c) (n)] 'flymake-goto-next-error)
