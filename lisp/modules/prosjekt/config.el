; populate load-path appropriately for prosjekt and its extensions.
(let ((root-path (list (file-name-directory load-file-name) "prosjekt" "prosjekt")))
  (dolist (path '(("") 
		  ("ext" "helm")
		  ( "ext" "dirtree")))
    (add-to-list
     'load-path
     (apply 'kfg:join (append root-path path)))))

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
