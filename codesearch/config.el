(add-to-list 'load-path "~/projects/codesearch.el")

(require 'codesearch)

(global-set-key "\M-'" 'codesearch-search)
(global-set-key "\M-;" 'codesearch-search-at-point)
