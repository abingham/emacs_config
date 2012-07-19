; "original" eproject
(load "eproject/eproject.el")

(setq prj-autotracking nil)

; anything + eproject integration
(require 'anything-eproject)

; let project files be sources in anything
(add-to-list 'anything-sources 'anything-c-source-eproject-files t)

; let projects be sources in anything
(add-to-list 'anything-sources 'anything-c-source-eproject-projects t)
