(add-to-list 
 'load-path 
 (concat (file-name-directory load-file-name)
         "/slime-2013-04-05"))
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)
