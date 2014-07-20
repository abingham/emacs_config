(dolist 
    (cmd 
     (list 
      'p4-add
      'p4-branch
      'p4-branches
      'p4-change
      'p4-changes
      'p4-client
      'p4-clients
      'p4-delete
      'p4-describe
      'p4-diff
      'p4-diff2
      'p4-diff-head
      'p4-ediff2
      'p4-edit
      'p4-filelog
      'p4-files
      'p4-fix
      'p4-get
      'p4-group
      'p4-groups
      'p4-have
      'p4-help
      'p4-info
      'p4-integ
      'p4-job
      'p4-jobs
      'p4-jobspec
      'p4-label
      'p4-labels
      'p4-labelsync
      'p4-lock
      'p4-opened
      'p4-print
      'p4-refresh
      'p4-rename
      'p4-reopen
      'p4-resolve
      'p4-revert
      'p4-submit
      'p4-sync
      'p4-user
      'p4-users
      'p4-unlock
      'p4-where))
  (autoload cmd "p4" "" t))

;(autoload 'p4-add "p4" "" t)
;(autoload 'p4-edit "p4" "" t)
;(autoload 'p4-revert "p4" "" t)
;(autoload 'p4-diff "p4" "" t)
;(autoload 'p4-reopen "p4" "" t)
;(autolo


;(load-library "p4")

;(defun p4-blame ()
;  "Run p4_introduced on the line containing point in the current buffer"
;  (interactive)
;  (let ((file (buffer-file-name))
;	(line (count-lines 1 (point)))
;	(script "\\\\nirvana\\labview\\users\\amonat\\p4_introduced.py")
;	(python "c:\\Python25\\python.exe")
;	(resize-mini-windows nil)
;	(output "*p4_introduced output*"))
;    (shell-command (format "%s \"%s\" \"%s\" %d" python script file line) output)
;    (display-buffer output)))