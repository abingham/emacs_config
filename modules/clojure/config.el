(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'ac-nrepl-setup)
