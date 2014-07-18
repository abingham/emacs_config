(add-to-list
 'load-path
 (kfg-join (file-name-directory load-file-name) "codesearch"))
(require 'codesearch)

(global-set-key "\M-'" 'codesearch-search)
(global-set-key "\M-;" 'codesearch-search-at-point)
