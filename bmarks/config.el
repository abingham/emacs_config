;;;###autoload
(defun simple-bookmark-set ()
  "set the simple bookmark"
  (interactive)
  (bookmark-set "abingham_bookmark"))

;;;###autoload
(defun simple-bookmark-jump ()
  "Jump to the simple bookmark"
  (interactive)
  (bookmark-jump "abingham_bookmark")
  ; (bookmark-delete "temporary_poppable")
  )

(global-set-key [(shift return)] 'simple-bookmark-set)
(global-set-key [(shift down)] 'simple-bookmark-jump)
