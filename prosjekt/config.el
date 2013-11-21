(load "prosjekt/prosjekt/prosjekt.el")
(require 'dirtree-prosjekt)
(require 'helm-prosjekt)

(global-set-key [(ctrl x) (p) (s)] 'prosjekt-setup)
(global-set-key [(ctrl x) (p) (c)] 'prosjekt-close)
(global-set-key [(ctrl x) (p) (r)] 'prosjekt-repopulate)

(global-set-key 
 [f2] 
 (lambda () 
   (interactive) 
   (if prosjekt-proj-dir
       (dirtree-in-buffer prosjekt-proj-dir t))))
