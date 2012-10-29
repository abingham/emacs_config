(load "prosjekt/prosjekt/prosjekt.el")
(require 'anything-prosjekt)

(add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
(add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)

(global-set-key [(ctrl x) (p) (s)] 'prosjekt-setup)
(global-set-key [(ctrl x) (p) (c)] 'prosjekt-close)
(global-set-key [(ctrl x) (p) (r)] 'prosjekt-repopulate)
