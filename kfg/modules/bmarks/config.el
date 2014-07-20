;;;###autoload
(defun simple-bookmark-set ()
  "set the simple bookmark"
  (interactive)
  (bookmark-set "abingham_bookmark"))

;;;###autoload
(defun simple-bookmark-jump ()
  "Jump to the simple bookmark"
  (interactive)
  (bookmark-jump "abingham_bookmark"))

(global-set-key [(shift return)] 'simple-bookmark-set)
(global-set-key [(control shift return)] 'simple-bookmark-jump)
