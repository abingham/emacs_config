;; set up gtags

(autoload 'gtags-mode "gtags" "" t)

(defun my-gtags-mode-hook ()
  (define-key gtags-mode-map "\M-/" 'gtags-find-file)
  (define-key gtags-mode-map "\M-," 'gtags-find-rtag)
  (define-key gtags-mode-map "\M-'" 'gtags-find-tag)
)

(add-hook 'gtags-mode-hook 'my-gtags-mode-hook)

; Reconstruct the active TAGS file on file save
(defun create-gtags ()
  "Reconstruct the active tags file, if it's defined"
  (if (and (boundp 'gtags-rootdir)
	   gtags-rootdir)
      (shell-command
       (format "cd %s && gtags %s" 
               gtags-rootdir 
               gtags-rootdir))))

(add-hook 'after-save-hook 'create-gtags)

;;;###autoload
(defun initialize-gtags (dir)
  "Create a new ctags index."
  (interactive
   (list
    (read-directory-name "Directory: " ".")))
  (message "Constructing tag tables. This can take a while...")
  (shell-command
   (format "cd %s && gtags --gtagsconf=/usr/local/share/gtags/gtags.conf --gtagslabel=ctags" dir)))

;;;###autoload
(defun update-gtags (dir)
  "Create a new ctags index."
  (interactive
   (list
    (read-directory-name "Directory: " ".")))
  (shell-command
   (format "cd %s && gtags -i" dir)))

; (gtags-mode 1) ; I kinda like gtags on at all times
(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'python-mode-hook (lambda () (gtags-mode 1)))
