(require 'find-file-in-tags)

; How to initialize a TAGS file:
;    ctags -f TAGS -e -R .
; Maybe add a command for this...but really, how often is this needed?

(global-set-key "\M-;" 'tags-apropos)
(global-set-key "\M-'" 'tags-search)
(global-set-key "\M-\\" 'find-file-in-tags)
(setq tags-revert-without-query 't) ; don't ask if tags should be reloaded...that always should!

; Reconstruct the active TAGS file on file save
;;;###autoload
(defun create-tags ()
  "Reconstruct the active tags file, if it's defined"
  (interactive)
  (if tags-file-name
    (shell-command
     (format "ctags --exclude=\"*.html\" --langmap=\"python:(SConstruct)(SConscript).py\" -f %s -e -R %s"
             tags-file-name 
             (file-name-directory tags-file-name)))))

; This can take a really long time on larger code bases. Just do it manually 
; as needed.
; (add-hook 'after-save-hook 'create-tags)
  
;;;###autoload
(defun initialize-ctags (dir tagfile)
  "Create a new ctags index."
  (interactive
   (list
    (read-directory-name "Directory: " ".")
    (read-string "Tag file: " "TAGS")))
  (shell-command
   (format "cd %s && ctags --exclude=\"*.html\" --langmap=\"python:(SConstruct)(SConscript).py\" -f %s -e -R ." dir tagfile)))
