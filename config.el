;; Initialize the package system, get any missing packages, etc.
(require 'required-packages)

;; Do specific package configuration
(require 'configure-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rest...

;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'ropeburn)

(defun helm-my-buffers ()
  (interactive)
  (helm-other-buffer '(
                       helm-source-files-in-current-dir
                       helm-source-recentf
                       helm-c-source-prosjekt-projects
                       helm-c-source-prosjekt-files
                       helm-source-buffer-not-found)
                     "*helm-my-buffers*"))
(global-set-key "\C-u" 'helm-my-buffers)

(require 'smooth-scroll)

(defun line-to-top-of-window ()
  "Scroll current line to top of window. \
Replaces three keystroke sequence C-u 0 C-l."
  (interactive)
  (recenter 0))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

					; Revert all open buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (message (number-to-string (count-matches "\\sw+"))))))


(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))

;; display and behavior
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode 1)
(setq inhibit-splash-screen 't)
(global-auto-revert-mode 't)
(show-paren-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(setq visible-bell t)
(global-hl-line-mode t)
; (global-linum-mode t)

;; Compilation stuff
(setq-default display-buffer-reuse-frames t)
(setq compile-command "scons -u")
(setq compilation-scroll-output 't)
(defun my-compilation-mode-hook ()
  (define-key compilation-mode-map "\C-x\C-n" 'next-error)
  (define-key compilation-mode-map "\C-x\C-p" 'previous-error)
  )
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;; Misc. other stuff
(require 'cl)
; (require 'rtf-mode)
(require 'open-next-line)
;(require 'rainbow-delimiters)
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
; (require 'tramp)
; (setq tramp-default-method "ssh")

(fset 'yes-or-no-p 'y-or-n-p)           ; easy answer to stupid question

; Load any local settings.
(let* ((this_dir (file-name-directory load-file-name))
      (local_config (concat this_dir "local_config.el")))
  ; (add-to-list 'load-pah this_dir)
  (if (file-exists-p local_config)
      (load local_config)
      (message (format "no local config %s" local_config))))
  ;; (condition-case nil
  ;;     (load local_config)
  ;;   (error (message ")))

(load "bindings")
