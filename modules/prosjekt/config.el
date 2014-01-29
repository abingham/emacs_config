(add-to-list
 'load-path
 (kfg:join (file-name-directory load-file-name) "prosjekt" "prosjekt"))

(add-to-list
 'load-path
 (kfg:join (file-name-directory load-file-name) "prosjekt" "prosjekt" "ext" "helm"))

(require 'prosjekt)
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
