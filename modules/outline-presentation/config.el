(add-to-list
 'load-path
 (file-name-directory load-file-name))
(require 'outline-presentation)
(global-set-key [(ctrl c) (.)] 'outline-presentation-next)
(global-set-key [(ctrl c) (\,)] 'outline-presentation-previous)
