;;;###autoload
(defun simple-bookmark-set ()
  "set the simple bookmark"
  (interactive)
  (bookmark-set "__simple_bookmark__"))

;;;###autoload
(defun simple-bookmark-jump ()
  "Jump to the simple bookmark"
  (interactive)
  (bookmark-jump "__simple_bookmark__")
  ; (bookmark-delete "temporary_poppable")
  )

(global-set-key [(shift return)] 'simple-bookmark-set)
(global-set-key [(shift down)] 'simple-bookmark-jump)