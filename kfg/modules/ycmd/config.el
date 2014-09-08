(add-to-list 'load-path "/Users/sixtynorth/projects/emacs-ycmd")

(require 'ycmd)
(require 'company-ycmd)

(setq ycmd--log-enabled t)
;; (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*"))
(set-variable 'ycmd-global-config
              (concat (file-name-directory load-file-name)
                      "ycm_global_conf.py"))
;; (ycmd-open)

; We're trying out ycmd. No need for original clang support.
(setq company-backends (remove 'company-clang company-backends))

;; Enable automatic completion after -> and ::
(company-ycmd-enable-comprehensive-automatic-completion)

(setq company-backends
      '(company-bbdb
        company-nxml
        company-css
        company-eclim
        company-semantic
        ;; company-xcode
        ;; company-ropemacs
        company-cmake
        company-capf
        (company-ycmd-backend company-dabbrev-code company-gtags company-etags company-keywords)
        company-oddmuse
        company-files
        company-dabbrev))

;; (setq company-backends (append company-backends '(company-ycmd-backend)))

;; (company-ycmd-setup)

