; mode-specific stuff
(add-to-list 'load-path (concat (file-name-directory load-file-name) "/gallina"))
;(setq interpreter-mode-alist (cons '("python" . python-mode)
;                                   interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)
(require 'python)

; mode-independent stuff
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))
(setq python-indent-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda mode
(autoload 'lambda-mode "lambda-mode")
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))


;(require 'anything)
; (require 'anything-ipython)
;(when (require 'anything-show-completion nil t)
;  (use-anything-show-completion 'anything-ipython-complete
;                                '(length initial-pattern)))

;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (define-key py-mode-map (kbd "M-<tab>") 'ipython-complete)))
;; (add-hook 'ipython-shell-hook
;;           #'(lambda ()
;;               (define-key py-mode-map (kbd "M-<tab>") 'ipython-complete)))


;(require 'comint)
;(define-key comint-mode-map (kbd "M-") 'comint-next-input)
;(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
;(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
;(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pylookup stuff
; Create new database: ./pylookup.py -u file:///usr/share/doc/python2.6-doc/html
;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq pylookup-dir "~/.emacs.d/python/pylookup")
(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-cp" 'pylookup-lookup)


(require 'python-pep8)
(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")


                                     ; this get called after python mode is enabled
(defun my-python-hook ()
                                     ; outline uses this regexp to find headers. I match lines with no indent and indented "class"
                                     ; and "def" lines.
  ;(setq outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\) ")
  ;(setq outline-regexp " *\\(def\\|class\\) ")
                                     ; enable our level computation
  ;(setq outline-level 'py-outline-level)
                                     ; do not use their \C-c@ prefix, too hard to type. Note this overides some python mode bindings
  ;(setq outline-minor-mode-prefix "\C-c")
                                     ; turn on outline mode
  ;(outline-minor-mode t)
                                     ; initially hide all but the headers
  ;(hide-body)
                                     ; I use CUA mode on the PC so I rebind these to make the more accessible
  ;(local-set-key [?\C-\t] 'py-shift-region-right)
  ;(local-set-key [?\C-\S-\t] 'py-shift-region-left)
  ;(local-set-key "\M-f" 'forward-word)
  ;(local-set-key "\M-b" 'backward-word)
                                     ; make paren matches visible
  (show-paren-mode 1)
  )

                                        ; add my customization
(add-hook 'python-mode-hook 'my-python-hook)

; pymacs
;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)
;(autoload 'pymacs-autoload "pymacs")
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; ropemacs
;(setq ropemacs-enable-shortcuts nil)
;(setq ropemacs-local-prefix "C-c C-p")
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

;; -- one way to lazy-load ropemacs
;(defun load-ropemacs ()
;  "Load pymacs and ropemacs"
;  (interactive)
;  (require 'pymacs)
;  (pymacs-load "ropemacs" "rope-")
;  ;; Automatically save project python buffers before refactorings
;  (setq ropemacs-confirm-saving 'nil)
;  )
; (global-set-key "\C-xpl" 'load-ropemacs)