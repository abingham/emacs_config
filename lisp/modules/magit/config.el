; This lets us edit commit messages in the same emacs session
(set-variable 'magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

(require 'magit)

(global-set-key [f5] 'magit-status)
