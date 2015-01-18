;;; therapy --- Useful Python stuff

;;; Commentary:
;;
;; "This is supposed to be torture, not therapy!"  -- Minerva Mayflower
;;

;;; Code:

(require 'python)

(defgroup therapy nil
  "Useful Python stuff."
  :group 'tools
  :group 'programming)

(defcustom therapy-python2-hooks nil
  "Hook for when Python 2 is activated."
  :group 'therapy
  :type 'hook)

(defcustom therapy-python3-hooks nil
  "Hook for when Python 3 is activated."
  :group 'therapy
  :type 'hook)

(defun therapy-interpreter-changed ()
  "Call this when the Python interpreter is changed.

This will run the correct hooks for the new version."
  (if (string-equal "3" (therapy--python-major-version))
      (run-hooks 'therapy-python3-hooks)
    (run-hooks 'therapy-python2-hooks)))

(defun therapy--python-major-version ()
  "Find major version of current Python interpreter.

This runs `python-shell-interpreter' to determine the current
major version."
  (let* ((program "\"import sys; print(sys.version_info.major)\"")
         (command (format "%s -c %s" python-shell-interpreter program))
         (results (shell-command-to-string command)))
    ;; The output has the major version in the firt character.
    (substring results 0 1)))

(provide 'therapy)

;;; therapy.el ends here
