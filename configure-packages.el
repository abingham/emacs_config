; Allow loading code from ./elisp
(add-to-list 'load-path 
	     (concat 
	      (file-name-directory load-file-name) 
	      "/elisp"))

;; --- mode configurations and such ---
(setq config_packages
      '("anything"
	"autocomplete"
	"bmarks"
	"cleanup"
        "codesearch"
	"cpp"
	"dirtree"
        "flymake"
	"git"
	"hg"
 	"html"
	"ido"
	"multiterm"
	"p4"
	"prosjekt"
	"python"
	"rst"
	"traad"
	"uniquify"
	"yasnippet"
	)
      )

(mapc (lambda (dir)
	(add-to-list 'load-path (concat (file-name-directory load-file-name) "/" dir))
	(load (concat dir "/config")))
      config_packages)

(provide 'configure-packages)
