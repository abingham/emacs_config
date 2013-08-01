;;; ropeburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/ropeburn-emacs
;; Version: 20130716.1457
;; X-Original-Version: 2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Ropeburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on such this port
;; is based.

;;; Code:

(deftheme ropeburn "The Ropeburn color theme")

;;; Color Palette

(defvar ropeburn-colors-alist
  '(("ropeburn-fg"       . "white")
    ;("ropeburn-fg"       . "#DCDCCC")
    ("ropeburn-fg-1"     . "#656555")
    ("ropeburn-bg-1"     . "#2B2B2B")
    ("ropeburn-bg-05"    . "black")
    ("ropeburn-bg"       . "#3F3F3F")
    ("ropeburn-bg+1"     . "#4F4F4F")
    ("ropeburn-bg+2"     . "#5F5F5F")
    ("ropeburn-bg+3"     . "#6F6F6F")
    ("ropeburn-red+1"    . "#DCA3A3")
    ("ropeburn-red"      . "#CC9393")
    ("ropeburn-red-1"    . "#BC8383")
    ("ropeburn-red-2"    . "#AC7373")
    ("ropeburn-red-3"    . "#9C6363")
    ("ropeburn-red-4"    . "#8C5353")
    ("ropeburn-orange"   . "#DFAF8F")
    ("ropeburn-yellow"   . "#F0DFAF")
    ("ropeburn-yellow-1" . "#E0CF9F")
    ("ropeburn-yellow-2" . "#D0BF8F")
    ("ropeburn-green-1"  . "#5F7F5F")
    ("ropeburn-green"    . "#7F9F7F")
    ("ropeburn-green+1"  . "#8FB28F")
    ("ropeburn-green+2"  . "#9FC59F")
    ("ropeburn-green+3"  . "#AFD8AF")
    ("ropeburn-green+4"  . "#BFEBBF")
    ("ropeburn-cyan"     . "#93E0E3")
    ("ropeburn-blue+1"   . "#94BFF3")
    ("ropeburn-blue"     . "#8CD0D3")
    ("ropeburn-blue-1"   . "#7CB8BB")
    ("ropeburn-blue-2"   . "#6CA0A3")
    ("ropeburn-blue-3"   . "#5C888B")
    ("ropeburn-blue-4"   . "#4C7073")
    ("ropeburn-blue-5"   . "#366060")
    ("ropeburn-magenta"  . "#DC8CC3"))
  "List of Ropeburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro ropeburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `ropeburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   ropeburn-colors-alist))
     ,@body))

;;; Theme Faces
(ropeburn-with-color-variables
  (custom-theme-set-faces
   'ropeburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,ropeburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,ropeburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,ropeburn-fg :background ,ropeburn-bg))))
   `(cursor ((t (:foreground ,ropeburn-fg :background "white"))))
   `(escape-glyph ((t (:foreground ,ropeburn-yellow :bold t))))
   `(fringe ((t (:foreground ,ropeburn-fg :background ,ropeburn-bg+1))))
   `(header-line ((t (:foreground ,ropeburn-yellow
                                  :background ,ropeburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,ropeburn-bg-05))))
   `(success ((t (:foreground ,ropeburn-green :weight bold))))
   `(warning ((t (:foreground ,ropeburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,ropeburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,ropeburn-green))))
   `(compilation-error-face ((t (:foreground ,ropeburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,ropeburn-fg))))
   `(compilation-info-face ((t (:foreground ,ropeburn-blue))))
   `(compilation-info ((t (:foreground ,ropeburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,ropeburn-green))))
   `(compilation-line-face ((t (:foreground ,ropeburn-yellow))))
   `(compilation-line-number ((t (:foreground ,ropeburn-yellow))))
   `(compilation-message-face ((t (:foreground ,ropeburn-blue))))
   `(compilation-warning-face ((t (:foreground ,ropeburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,ropeburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,ropeburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,ropeburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,ropeburn-fg))))
   `(grep-error-face ((t (:foreground ,ropeburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,ropeburn-blue))))
   `(grep-match-face ((t (:foreground ,ropeburn-orange :weight bold))))
   `(match ((t (:background ,ropeburn-bg-1 :foreground ,ropeburn-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,ropeburn-yellow-2 :weight bold :background ,ropeburn-bg-1))))
   `(isearch-fail ((t (:foreground ,ropeburn-fg :background ,ropeburn-red-4))))
   `(lazy-highlight ((t (:foreground ,ropeburn-yellow-2 :weight bold :background ,ropeburn-bg-05))))

   `(menu ((t (:foreground ,ropeburn-fg :background ,ropeburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,ropeburn-yellow))))
   `(mode-line
     ((,class (:foreground ,ropeburn-green+1
                           :background ,ropeburn-bg+2
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,ropeburn-green-1
                      :background ,ropeburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,ropeburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,ropeburn-bg+2))))
   `(trailing-whitespace ((t (:background ,ropeburn-red))))
   `(vertical-border ((t (:foreground ,ropeburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,ropeburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,ropeburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,ropeburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,ropeburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,ropeburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,ropeburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,ropeburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,ropeburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,ropeburn-red))))
   `(font-lock-type-face ((t (:foreground ,ropeburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,ropeburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,ropeburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-default-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,ropeburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,ropeburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,ropeburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,ropeburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,ropeburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,ropeburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,ropeburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,ropeburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,ropeburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,ropeburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,ropeburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,ropeburn-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,ropeburn-fg-1 :background ,ropeburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,ropeburn-green+2 :background ,ropeburn-bg :inverse-video nil))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,ropeburn-fg))))
   `(ack-file ((t (:foreground ,ropeburn-blue))))
   `(ack-line ((t (:foreground ,ropeburn-yellow))))
   `(ack-match ((t (:foreground ,ropeburn-orange :background ,ropeburn-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,ropeburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,ropeburn-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,ropeburn-blue-4 :foreground ,ropeburn-fg))))
   `(popup-tip-face ((t (:background ,ropeburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,ropeburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,ropeburn-bg-1))))
   `(popup-isearch-match ((t (:background ,ropeburn-bg :foreground ,ropeburn-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,ropeburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,ropeburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,ropeburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,ropeburn-green))))
   `(android-mode-warning-face ((t (:foreground ,ropeburn-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,ropeburn-yellow-1 :foreground ,ropeburn-bg))))
   `(bm-fringe-face ((t (:background ,ropeburn-yellow-1 :foreground ,ropeburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,ropeburn-green-1 :foreground ,ropeburn-bg))))
   `(bm-persistent-face ((t (:background ,ropeburn-green-1 :foreground ,ropeburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,ropeburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,ropeburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,ropeburn-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,ropeburn-blue :foreground ,ropeburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,ropeburn-bg-05 :foreground ,ropeburn-bg))))
   `(ctbl:face-row-select ((t (:background ,ropeburn-cyan :foreground ,ropeburn-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,ropeburn-green+4 :background nil))
                 (t (:foreground ,ropeburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,ropeburn-yellow))))
   `(diff-removed ((,class (:foreground ,ropeburn-red :background nil))
                   (t (:foreground ,ropeburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,ropeburn-bg+2))
                  (t (:background ,ropeburn-fg :foreground ,ropeburn-bg))))
   `(diff-file-header
     ((,class (:background ,ropeburn-bg+2 :foreground ,ropeburn-fg :bold t))
      (t (:background ,ropeburn-fg :foreground ,ropeburn-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,ropeburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,ropeburn-orange))))
   `(diredp-date-time ((t (:foreground ,ropeburn-magenta))))
   `(diredp-deletion ((t (:foreground ,ropeburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,ropeburn-red))))
   `(diredp-dir-heading ((t (:foreground ,ropeburn-blue :background ,ropeburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,ropeburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,ropeburn-red))))
   `(diredp-executable-tag ((t (:foreground ,ropeburn-green+1))))
   `(diredp-file-name ((t (:foreground ,ropeburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,ropeburn-green))))
   `(diredp-flag-mark ((t (:foreground ,ropeburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,ropeburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,ropeburn-red))))
   `(diredp-link-priv ((t (:foreground ,ropeburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,ropeburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,ropeburn-orange))))
   `(diredp-no-priv ((t (:foreground ,ropeburn-fg))))
   `(diredp-number ((t (:foreground ,ropeburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,ropeburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,ropeburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,ropeburn-green-1))))
   `(diredp-symlink ((t (:foreground ,ropeburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,ropeburn-magenta))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,ropeburn-green+4 :background ,ropeburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,ropeburn-red :background ,ropeburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,ropeburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,ropeburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,ropeburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,ropeburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,ropeburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,ropeburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ropeburn-red) :inherit unspecified))
      (t (:foreground ,ropeburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ropeburn-orange) :inherit unspecified))
      (t (:foreground ,ropeburn-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,ropeburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,ropeburn-orange :weight bold))))
;;;;; flymake
   ;; `(flymake-errline
   ;;   ((((supports :underline (:style wave)))
   ;;     (:underline (:style wave :color ,ropeburn-red)
   ;;                 :inherit unspecified :foreground unspecified :background unspecified))
   ;;    (t ;(:foreground ,ropeburn-red-1 :weight bold :underline t)
   ;;     (:foreground "#FF0000")
   ;;     )))
   ;; `(flymake-warnline
   ;;   ((((supports :underline (:style wave)))
   ;;     (:underline (:style wave :color ,ropeburn-orange)
   ;;                 :inherit unspecified :foreground unspecified :background unspecified))
   ;;    (t (:foreground ,ropeburn-orange :weight bold :underline t))))
   ;; `(flymake-infoline
   ;;   ((((supports :underline (:style wave)))
   ;;     (:underline (:style wave :color ,ropeburn-green)
   ;;                 :inherit unspecified :foreground unspecified :background unspecified))
   ;;    (t (:foreground ,ropeburn-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ropeburn-orange) :inherit unspecified))
      (t (:foreground ,ropeburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ropeburn-red) :inherit unspecified))
      (t (:foreground ,ropeburn-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,ropeburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,ropeburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,ropeburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,ropeburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,ropeburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,ropeburn-green))))
   `(erc-pal-face ((t (:foreground ,ropeburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,ropeburn-orange :background ,ropeburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,ropeburn-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,ropeburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,ropeburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,ropeburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,ropeburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,ropeburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,ropeburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,ropeburn-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,ropeburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,ropeburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,ropeburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,ropeburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,ropeburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,ropeburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,ropeburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,ropeburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,ropeburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,ropeburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,ropeburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,ropeburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,ropeburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,ropeburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,ropeburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,ropeburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,ropeburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,ropeburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,ropeburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,ropeburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,ropeburn-green))))
   `(gnus-cite-7 ((t (:foreground ,ropeburn-red))))
   `(gnus-cite-8 ((t (:foreground ,ropeburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,ropeburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,ropeburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,ropeburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,ropeburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,ropeburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,ropeburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,ropeburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,ropeburn-bg+2))))
   `(gnus-signature ((t (:foreground ,ropeburn-yellow))))
   `(gnus-x ((t (:background ,ropeburn-fg :foreground ,ropeburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,ropeburn-blue))))
   `(guide-key/key-face ((t (:foreground ,ropeburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,ropeburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,ropeburn-green
                      :background ,ropeburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,ropeburn-yellow
                      :background ,ropeburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,ropeburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,ropeburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,ropeburn-bg :background ,ropeburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,ropeburn-green+4 :background ,ropeburn-bg-1))))
   `(helm-ff-directory ((t (:foreground ,ropeburn-magenta))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,ropeburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,ropeburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,ropeburn-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,ropeburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,ropeburn-yellow))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,ropeburn-orange))))
   `(js2-error ((t (:foreground ,ropeburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,ropeburn-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,ropeburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,ropeburn-green+3))))
   `(js2-function-param ((t (:foreground, ropeburn-green+3))))
   `(js2-external-variable ((t (:foreground ,ropeburn-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,ropeburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,ropeburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,ropeburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,ropeburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,ropeburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,ropeburn-red+1))))
   `(jabber-activity-face((t (:foreground ,ropeburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,ropeburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,ropeburn-green+2 :background ,ropeburn-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,ropeburn-green+2 :background ,ropeburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,ropeburn-red+1 :background ,ropeburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,ropeburn-blue+1 :background ,ropeburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,ropeburn-magenta :background ,ropeburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,ropeburn-yellow :background ,ropeburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,ropeburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,ropeburn-bg+1 :bold nil))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,ropeburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,ropeburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,ropeburn-green+3))))
   `(egg-branch ((t (:foreground ,ropeburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,ropeburn-yellow))))
   `(egg-term ((t (:foreground ,ropeburn-yellow))))
   `(egg-diff-add ((t (:foreground ,ropeburn-green+4))))
   `(egg-diff-del ((t (:foreground ,ropeburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,ropeburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,ropeburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,ropeburn-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,ropeburn-green+1))))
   `(message-header-other ((t (:foreground ,ropeburn-green))))
   `(message-header-to ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,ropeburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,ropeburn-green))))
   `(message-mml ((t (:foreground ,ropeburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,ropeburn-orange))))
   `(mew-face-header-from ((t (:foreground ,ropeburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,ropeburn-green))))
   `(mew-face-header-to ((t (:foreground ,ropeburn-red))))
   `(mew-face-header-key ((t (:foreground ,ropeburn-green))))
   `(mew-face-header-private ((t (:foreground ,ropeburn-green))))
   `(mew-face-header-important ((t (:foreground ,ropeburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,ropeburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,ropeburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,ropeburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,ropeburn-red))))
   `(mew-face-body-url ((t (:foreground ,ropeburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,ropeburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,ropeburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,ropeburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,ropeburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,ropeburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,ropeburn-red))))
   `(mew-face-mark-review ((t (:foreground ,ropeburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,ropeburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,ropeburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,ropeburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,ropeburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,ropeburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,ropeburn-green))))
   `(mew-face-eof-part ((t (:foreground ,ropeburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,ropeburn-cyan :background ,ropeburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,ropeburn-bg :background ,ropeburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,ropeburn-bg :background ,ropeburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,ropeburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,ropeburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,ropeburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,ropeburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,ropeburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,ropeburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,ropeburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,ropeburn-cyan))))
   `(nav-face-dir ((t (:foreground ,ropeburn-green))))
   `(nav-face-hdir ((t (:foreground ,ropeburn-red))))
   `(nav-face-file ((t (:foreground ,ropeburn-fg))))
   `(nav-face-hfile ((t (:foreground ,ropeburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,ropeburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,ropeburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,ropeburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,ropeburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,ropeburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,ropeburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,ropeburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,ropeburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,ropeburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,ropeburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,ropeburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,ropeburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,ropeburn-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,ropeburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,ropeburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,ropeburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,ropeburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,ropeburn-green+3))))
   `(org-formula ((t (:foreground ,ropeburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,ropeburn-green+3))))
   `(org-hide ((t (:foreground ,ropeburn-bg-1))))
   `(org-level-1 ((t (:foreground ,ropeburn-orange))))
   `(org-level-2 ((t (:foreground ,ropeburn-green+4))))
   `(org-level-3 ((t (:foreground ,ropeburn-blue-1))))
   `(org-level-4 ((t (:foreground ,ropeburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,ropeburn-cyan))))
   `(org-level-6 ((t (:foreground ,ropeburn-green+2))))
   `(org-level-7 ((t (:foreground ,ropeburn-red-4))))
   `(org-level-8 ((t (:foreground ,ropeburn-blue-4))))
   `(org-link ((t (:foreground ,ropeburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,ropeburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,ropeburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,ropeburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,ropeburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:foreground ,ropeburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,ropeburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,ropeburn-orange))))
   `(org-todo ((t (:bold t :foreground ,ropeburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,ropeburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,ropeburn-bg-1))))
   `(org-column-title ((t (:background ,ropeburn-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,ropeburn-orange))))
   `(outline-2 ((t (:foreground ,ropeburn-green+4))))
   `(outline-3 ((t (:foreground ,ropeburn-blue-1))))
   `(outline-4 ((t (:foreground ,ropeburn-yellow-2))))
   `(outline-5 ((t (:foreground ,ropeburn-cyan))))
   `(outline-6 ((t (:foreground ,ropeburn-green+2))))
   `(outline-7 ((t (:foreground ,ropeburn-red-4))))
   `(outline-8 ((t (:foreground ,ropeburn-blue-4))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,ropeburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,ropeburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,ropeburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,ropeburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,ropeburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,ropeburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,ropeburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,ropeburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,ropeburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,ropeburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,ropeburn-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,ropeburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,ropeburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,ropeburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,ropeburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,ropeburn-blue-2))))
   `(rcirc-server ((t (:foreground ,ropeburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,ropeburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,ropeburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,ropeburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,ropeburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,ropeburn-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,ropeburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,ropeburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,ropeburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,ropeburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,ropeburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,ropeburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,ropeburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,ropeburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,ropeburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,ropeburn-orange))))
   `(rst-level-2-face ((t (:foreground ,ropeburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,ropeburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,ropeburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,ropeburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,ropeburn-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,ropeburn-red-3 :background ,ropeburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,ropeburn-blue-1 :background ,ropeburn-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,ropeburn-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,ropeburn-fg
                                    :background ,ropeburn-bg))))
   `(tabbar-selected ((t (:foreground ,ropeburn-fg
                                      :background ,ropeburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,ropeburn-fg
                                        :background ,ropeburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,ropeburn-bg
                                       :background ,ropeburn-bg-1))))
   `(term-color-red ((t (:foreground ,ropeburn-red-2
                                       :background ,ropeburn-red-4))))
   `(term-color-green ((t (:foreground ,ropeburn-green
                                       :background ,ropeburn-green+2))))
   `(term-color-yellow ((t (:foreground ,ropeburn-orange
                                       :background ,ropeburn-yellow))))
   `(term-color-blue ((t (:foreground ,ropeburn-blue-1
                                      :background ,ropeburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,ropeburn-magenta
                                         :background ,ropeburn-red))))
   `(term-color-cyan ((t (:foreground ,ropeburn-cyan
                                       :background ,ropeburn-blue))))
   `(term-color-white ((t (:foreground ,ropeburn-fg
                                       :background ,ropeburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,ropeburn-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,ropeburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,ropeburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,ropeburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,ropeburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,ropeburn-green+2 :background ,ropeburn-bg))))
   `(w3m-lnum-match ((t (:background ,ropeburn-bg-1
                                     :foreground ,ropeburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,ropeburn-yellow))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,ropeburn-bg+1 :foreground ,ropeburn-bg+1))))
   `(whitespace-hspace ((t (:background ,ropeburn-bg+1 :foreground ,ropeburn-bg+1))))
   `(whitespace-tab ((t (:background ,ropeburn-red-1))))
   `(whitespace-newline ((t (:foreground ,ropeburn-bg+1))))
   `(whitespace-trailing ((t (:background ,ropeburn-red))))
   `(whitespace-line ((t (:background ,ropeburn-bg :foreground ,ropeburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,ropeburn-orange :foreground ,ropeburn-orange))))
   `(whitespace-indentation ((t (:background ,ropeburn-yellow :foreground ,ropeburn-red))))
   `(whitespace-empty ((t (:background ,ropeburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,ropeburn-yellow :foreground ,ropeburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,ropeburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,ropeburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,ropeburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,ropeburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,ropeburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,ropeburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,ropeburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,ropeburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,ropeburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,ropeburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,ropeburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,ropeburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,ropeburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,ropeburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,ropeburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,ropeburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,ropeburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,ropeburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,ropeburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,ropeburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,ropeburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,ropeburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,ropeburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,ropeburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,ropeburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,ropeburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,ropeburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,ropeburn-bg-1 :foreground ,ropeburn-bg-1))))
   ))

;;; Theme Variables
(ropeburn-with-color-variables
  (custom-theme-set-variables
   'ropeburn
;;;;; ansi-color
   `(ansi-color-names-vector [,ropeburn-bg ,ropeburn-red ,ropeburn-green ,ropeburn-yellow
                                          ,ropeburn-blue ,ropeburn-magenta ,ropeburn-cyan ,ropeburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,ropeburn-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,ropeburn-red-1)
       ( 40. . ,ropeburn-red)
       ( 60. . ,ropeburn-orange)
       ( 80. . ,ropeburn-yellow-2)
       (100. . ,ropeburn-yellow-1)
       (120. . ,ropeburn-yellow)
       (140. . ,ropeburn-green-1)
       (160. . ,ropeburn-green)
       (180. . ,ropeburn-green+1)
       (200. . ,ropeburn-green+2)
       (220. . ,ropeburn-green+3)
       (240. . ,ropeburn-green+4)
       (260. . ,ropeburn-cyan)
       (280. . ,ropeburn-blue-2)
       (300. . ,ropeburn-blue-1)
       (320. . ,ropeburn-blue)
       (340. . ,ropeburn-blue+1)
       (360. . ,ropeburn-magenta)))
   `(vc-annotate-very-old-color ,ropeburn-magenta)
   `(vc-annotate-background ,ropeburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar ropeburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for ropeburn color names.
In buffers visiting library `ropeburn-theme.el' the ropeburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar ropeburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after ropeburn activate)
;;   "Maybe also add font-lock keywords for ropeburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or ropeburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "ropeburn-theme.el")))
;;     (unless ropeburn-colors-font-lock-keywords
;;       (setq ropeburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car ropeburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc ropeburn-colors-alist))))))
;;     (font-lock-add-keywords nil ropeburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after ropeburn activate)
;;   "Also remove font-lock keywords for ropeburn colors."
;;   (font-lock-remove-keywords nil ropeburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'ropeburn)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; ropeburn-theme.el ends here
