(require 'smart-mode-line)

(sml/setup)

; TODO: Think about letting the individual modules do this
; configuration. It's convenient to have it here for now, though.
(dolist (m '("Helm" "AC" "Undo-Tree" "Paredit"))
  (add-to-list 'sml/hidden-modes (concat " " m)))
