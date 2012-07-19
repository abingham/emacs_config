;; elemental.el -- functions for intelligently jumping between and
;; transposing list/tuple/dictionary/function-parameter
;; elements. These functions are primarily useful when editing
;; software source code.
;;
;; Copyright (C) 2012 Menno Smits
;;
;; Author: Menno Smits <menno@freshfoo.com>
;; Version: 0.1
;; URL: https://bitbucket.org/mjs0/elemental/
;;
;; This file is not part of GNU Emacs.

;; Description:
;;
;; It is difficult to efficiently navigate and manipulate list
;; elements and function parameter lists, especially when string
;; literals and nested function calls are involved. The functions in
;; this module make this process easier.
;;
;; The main functions of interest are elem/forward and elem/transpose.
;;
;; Given a source code line, here's how the point moves with successive
;; calls to elem/forward (the | shows the point):
;;
;;     myfunc(|123, "some, string", fn(a, b))
;;     myfunc(123|, "some, string", fn(a, b))
;;     myfunc(123, "some, string"|, fn(a, b))
;;     myfunc(123, "some, string", fn(a, b)|)
;;
;; Similarly, here's how successive calls to elem/transpose work:
;;
;;     myfunc(|123, "some, string", fn(a, b))
;;     myfunc("some, string", 123|, fn(a, b))
;;     myfunc("some, string", fn(a, b), 123|)
;;
;; These functions work correctly over multiple lines and respect
;; whitespace around elemnents. For example, here's how successive
;; elem/transpose calls work on this multi-line list/array:
;;
;;     [  1|23,
;;        "some, string",
;;        fn(a, b)  ]
;;
;;     [  "some, string",
;;        123|,
;;        fn(a, b)  ]
;;
;;     [  "some, string",
;;        fn(a, b),
;;        123|  ]
;;
;; elem/forward and elem/transpose take an optional numerical argument
;; (which can be specified as a prefix argument). Positive numbers
;; cause the action to be applied multiple times in the forward
;; direction. Negative numbers cause the action to be applied
;; backwards.

;; Installation:
;;
;; Copy elemental.el to somewhere in your load path and add "(require
;; 'elemental)" to your init.el or .emacs file. You'll want to add
;; some bindings.
;;
;; Example config:
;;
;;   (require 'elemental)
;;   (global-set-key (kbd "C-(") 'elem/backward-one)
;;   (global-set-key (kbd "C-)") 'elem/forward)
;;   (global-set-key (kbd "C-*") 'elem/transpose)

;; Limitations:
;;
;; elemental.el assumes that a comma is the separator between
;; elements. This means it currently won't work when editing code for
;; languages where this assumption isn't true. This will be addressed
;; in a future version. For Lisp-like languages the transpose-sexps
;; function might be want you want.

;; Tests:
;;
;; Unit tests can be found in elemental-tests.el. They rely on the ERT
;; framework which is included with Emacs 24. It is also available
;; here: https://github.com/ohler/ert
;;
;; See the top of elemental-tests.el for details on running the tests.

;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


(defun elem/forward (arg)
  "Move over 'listish elements'

  The definition of a listish element varies according to the
  current mode but would typically include lists, arrays, tuples,
  dictionaries and function arguments in program source code.

  This function does nothing if the point is not inside a 'listish
  structure'.

  When prefix arg is positive, move that many elements forward.
  When prefix arg is negative, move that many elements backwards.
  An arg of 0, means the point won't move.
  "
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (elem/forward-one))
    (dotimes (_ (abs arg)) (elem/backward-one))))

(defun elem/transpose (arg)
  "Interchange 'listish elements' at the point.

  The semantics are the same as other Emacs transpose
  functions (e.g. transpose-words).

  See elem/forward for a description of 'listish elements'.

  When prefix arg is positive, transpose that many elements forward.
  When prefix arg is negative, tranpose that many elements backwards.
  An arg of 0, means nothing happens.
  "
  (interactive "*p")
  (unless (eq arg 0)
    (transpose-subr 'elem/forward arg)))

(defun elem/forward-one ()
  "Move forward one listish element"
  (interactive)
  (unless (elem/outside-parens?)
    (elem/move-out-of-string-if-required)
    (condition-case nil
        (progn
          (elem/forward-sexp-skipping-comments)
          (while (not (elem/looking-at-over-ws-and-comments ","))
            (elem/forward-sexp-skipping-comments)))
      (scan-error nil))))

(defun elem/backward-one ()
  "Move backward one listish element"
  (interactive)
  (unless (elem/outside-parens?)
    (condition-case nil
        (progn
          (unless (elem/move-out-of-string-if-required)
            (elem/backward-sexp-skipping-comments))
          (while (not (elem/looking-back-over-ws-and-comments ","))
            (elem/backward-sexp-skipping-comments)))
      (scan-error nil))))

(defun elem/forward-sexp-skipping-comments ()
  "Like forward-sexp, but keeps going if the point end up on a comment"
  (forward-sexp)
  (while (elem/in-comment-by-font-lock? (point))
    (forward-sexp)))

(defun elem/backward-sexp-skipping-comments ()
  "Like backward-sexp, but keeps going if the point end up on a comment"
  (backward-sexp)
  (while (elem/in-comment-by-font-lock? (point))
    (backward-sexp)))

(defun elem/looking-at-over-ws-and-comments (regex)
  "Like looking-at, but pretends whitespace and comments aren't there"
  (save-excursion
    (elem/skip-ws-and-comments 0 'forward-char)
    (looking-at regex)))

(defun elem/looking-back-over-ws-and-comments (regex)
  "Like looking-back, but pretends whitespace and comments aren't there"
  (save-excursion
    (elem/skip-ws-and-comments -1 'backward-char)
    (looking-back regex)))

(defun elem/skip-ws-and-comments (look-offset move-func)
  (while (elem/in-ws-or-comment? (+ (point) look-offset))
      (funcall move-func)))

(defun elem/in-ws-or-comment? (where)
  "Return non-nil if character at WHERE is whitespace or part of a comment

  Use both Emacs' internal parser state tables and font-lock to work
  this out as neither approach is quite good enough on it's own."
  (or (elem/in-comment-or-ws-by-syntax? where)
      (elem/in-comment-by-font-lock? where)))

(defun elem/in-comment-or-ws-by-syntax? (where)
  "Return non-nil if char at WHERE is whitespace or part of a comment,
  using Emacs' parser state and the mode's syntax tables."
  (memq (char-syntax (char-after where)) elem/comment-and-ws-classes))

(defun elem/in-comment-by-font-lock? (where)
  "Return non-nil if char at WHERE is part of a comment,using the
  font-lock face property"
  (memq (get-text-property where 'face) elem/comment-faces))

(defun elem/outside-parens? ()
  "Return non-nil if the point is outside of any parentheses
  (as specified by the buffer's mode)."
  (<= (car (syntax-ppss)) 0))

(defun elem/move-out-of-string-if-required ()
  "If the point is inside a source code string literal, move the point
  to the start of the string.

  Returns non-nil if this happens, nil otherwise."
  (let ((pstate (syntax-ppss)))
    (when (nth 3 pstate)
        (goto-char (nth 8 pstate)))))    ; in a string, move to start

(defconst elem/comment-faces
  '(font-lock-comment-face
    font-lock-comment-delimiter-face)
  "Font lock faces used for comments")

(defconst elem/comment-and-ws-classes
  '(32 33 60 62)
  "Emacs syntax classes for whitespace and comments")

(provide 'elemental)
