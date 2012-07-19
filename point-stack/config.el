; (require 'breadcrumb)

;(global-set-key [(shift space)]         'bc-set)            ;; Shift-SPACE for set bookmark
;(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
;(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
;(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
;(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
;(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
;(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;;  Another set of bindings similar to MS Visual Studio bookmark setting.
;(global-set-key [(control f2)]          'bc-set)
;(global-set-key [(f2)]                  'bc-previous)
;(global-set-key [(shift f2)]            'bc-next)
;(global-set-key [(meta f2)]             'bc-list)

(require 'point-stack)
;(global-set-key '[(f5)] 'point-stack-push)
;(global-set-key '[(f6)] 'point-stack-pop)
;(global-set-key '[(f7)] 'point-stack-forward-stack-pop)

; my fingers like these bindings
(global-set-key [(shift return)] 'point-stack-push)
(global-set-key [(shift down)] 'point-stack-pop)
