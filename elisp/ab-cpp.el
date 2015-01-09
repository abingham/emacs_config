(require 'dash)

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
  (--map (add-to-list 'flycheck-disabled-checkers it)
         '(c/c++-clang c/c++-gcc c/c++-cppcheck))
  ;; (c-set-offset 'topmost-intro '-)
  ;; (c-set-offset 'inline-open '-)
)

(defcustom ab-cpp-patterns
  '("\\.h" "\\.ipp")
  "File patterns that get put into C++ mode.")

(use-package cc-mode
  :init
  (progn
    (dolist (pattern ab-cpp-patterns)
      (add-to-list 'auto-mode-alist `(,pattern . c++-mode)))
    (add-hook 'c++-mode-hook 'my-cpp-mode-hook)))

(provide 'ab-cpp)
;;; ab-cpp.el ends here
