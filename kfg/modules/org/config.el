(setq org-src-fontify-natively t)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#222222")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#000000")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#222222")))
  "Face used for the line delimiting the end of source blocks.")

(require 'ox-reveal)
