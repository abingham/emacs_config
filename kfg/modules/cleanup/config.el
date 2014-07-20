; run untabify and delete-trailing-whitespace when saving files,
; but only for files that match a filter list (to avoid borking Makefiles, etc.)

(setq cleanup-buffer-filter
      (list ".*\.cpp"
            ".*\.hpp"
            ".*\.h"
            ".*\.py"
            "SConstruct"
            "SConscript"
            ".*\.rst"
	    ".*\.js"
	    ".*\.jinja2"))

;;;###autoload
(defun cleanup-buffer ()
  (if (delq nil (mapcar (lambda (re) (string-match re (buffer-name))) cleanup-buffer-filter))
      (let ()
        (untabify (point-min) (point-max))
        (delete-trailing-whitespace)
        ; or, (whitespace-cleanup)
    )))

(add-hook 'before-save-hook 'cleanup-buffer)
