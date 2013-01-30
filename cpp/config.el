(require 'flymake)

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
  (setq-default indent-tabs-mode nil) ; 't = use tabs; nil = use spaces
  (delete-selection-mode 1) ; Maybe on by default on windows and off by default on solaris?
  (setq c-basic-offset 2)
  (show-paren-mode 1)
  ;; (c-set-offset 'topmost-intro '-)
  ;; (c-set-offset 'inline-open '-)
)
(add-hook 'c++-mode-hook 'my-cpp-mode-hook) 

;; flymake
;; no need to arrange Makefile
;; (defun flymake-cc-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name)))
;;          (common-args (append (list "-Wall" "-W" "-fsyntax-only" local-file)
;;                               flymake-additional-compilation-flags)))
;;     (if (eq major-mode 'c++-mode)
;;         (list "g++" common-args)
;;       (list "g++" common-args))))
 
;; (loop for ext in '("\\.c$" "\\.h$" "\\.cc$" "\\.cpp$" "\\.hh$" "\\.hpp$")
;;       do
;;       (push `(,ext flymake-cc-init) flymake-allowed-file-name-masks))
 
;(add-hook 'c-mode-hook (lambda () (flymake-mode t)))
;(add-hook 'c++-mode-hook (lambda () (flymake-mode t)))
