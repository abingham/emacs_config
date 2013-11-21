(require 'helm)
(require 'helm-elisp)
(require 'helm-utils)

(helm-mode t)

; Emulate old describe-* bindings
(global-set-key [(ctrl h) (f)] 'helm-apropos)
(global-set-key [(ctrl h) (v)] 'helm-apropos)

;; (helm :sources '(helm-source-findutils
;;                  helm-source-recentf
;;                  helm-source-bookmarks
;;                  helm-source-buffers-list
;;                  helm-source-locate
;;                  helm-source-ls-git)
;;       :buffer "*helm all the things*")
; (global-set-key "\C-u" (helm))


