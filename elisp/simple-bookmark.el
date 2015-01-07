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

(provide 'simple-bookmark)
;;; bmarks.el ends here


