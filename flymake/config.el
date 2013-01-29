(require 'flymake)

(defvar flymake-additional-compilation-flags nil)
(put 'flymake-additional-compilation-flags 'safe-local-variable 'listp)
 
;; no need to arrange Makefile
(defun flymake-cc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (common-args (append (list "-Wall" "-W" "-fsyntax-only" local-file)
                              flymake-additional-compilation-flags)))
    (if (eq major-mode 'c++-mode)
        (list "g++" common-args)
      (list "g++" common-args))))
 
(loop for ext in '("\\.c$" "\\.h$" "\\.cc$" "\\.cpp$" "\\.hh$" "\\.hpp$")
      do
      (push `(,ext flymake-cc-init) flymake-allowed-file-name-masks))
 
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes3" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))

(add-hook 'c-mode-hook (lambda () (flymake-mode t)))
(add-hook 'c++-mode-hook (lambda () (flymake-mode t)))
(add-hook 'python-mode-hook (lambda () (flymake-mode t)))

(global-set-key [(ctrl c) (f)] 'flymake-display-err-menu-for-current-line)
