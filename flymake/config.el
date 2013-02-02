(require 'flymake)

(global-set-key [(ctrl c) (f)] 'flymake-display-err-menu-for-current-line)
(global-set-key [(ctrl c) (n)] 'flymake-goto-next-error)


;(custom-set-variables
;     '(help-at-pt-timer-delay 0.9)
;     '(help-at-pt-display-when-idle '(flymake-overlay)))
