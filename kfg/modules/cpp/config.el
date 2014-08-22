(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ipp\\'" . c++-mode) auto-mode-alist))

;; setup some basic C++ formatting stuff, aiming to sorta match msvc
;;;###autoload
(defun my-cpp-mode-hook ()
  ; (setq c-basic-offset 2)
  (setq tab-width 1)
  (c-set-style "stroustrup")
  (c-set-offset 'innamespace '-) ; do not add extra indent for namespaces
  (c-set-offset 'inclass '+)     ; indent one tabs inside classes...
  (c-set-offset 'access-label '-) ; ...except for access labels.
  (c-set-offset 'case-label '+)
  (c-set-offset 'inline-open 0) ; don't indent opening an inline function
  (setq-default indent-tabs-mode nil) ; 't = use tabs; nil = use spaces
  (delete-selection-mode 1) ; Maybe on by default on windows and off by default on solaris?
  (setq c-basic-offset 4)
  (show-paren-mode 1)
  ;; (c-set-offset 'topmost-intro '-)
  ;; (c-set-offset 'inline-open '-)
)
(add-hook 'c++-mode-hook 'my-cpp-mode-hook) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
;; (defvar my-flymake-cc-compiler "g++"
;;   "C++ compiler to use for syntax checking.")

;; (defun my-flymake-cc-command (filename)
;;   "Construct a command that flymake can use to syntax-check FILENAME."
;;   (list my-flymake-cc-compiler "-Wall" "-W" "-fsyntax-only" filename))

;; (defun my-flymake-cc-load ()
;;   (dolist (ext '("\\.c$" "\\.h$" "\\.cc$" "\\.cpp$" "\\.hh$" "\\.hpp$"))
;;     (flymake-easy-load 'my-flymake-cc-command
;; 		       flymake-err-line-patterns
;; 		       'inplace
;; 		       ext
;; 		       "^W")))
 
;; (add-hook 'c-mode-hook 'my-flymake-cc-load)
;; (add-hook 'c++-mode-hook 'my-flymake-cc-load)

