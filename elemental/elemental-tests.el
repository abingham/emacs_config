;; Unit tests for elemental.el
;;
;; These tests use the ERT framework which ships with Emacs 24.
;;
;; To run the tests, eval this buffer and run the ert function. For
;; more information see the ERT info page.

(require 'ert)
(require 'elemental)

(require 'cc-mode)

(defun elem/test (before func-to-test after &optional mode-func)
  "Run an elemental unit test

  Sets up a temporary buffer with the 'before' text, runs func-to-test
  and checks that the buffer looks like the 'after' text. 'mode-func'
  is an optional function to call to set the mode of the buffer."
  (with-temp-buffer
    (insert before)
    (beginning-of-buffer)
    (search-forward "|")
    (backward-char)
    (delete-char 1)
    (when mode-func
      (funcall mode-func)
      (font-lock-fontify-buffer))
    (if (functionp func-to-test)
        (funcall func-to-test)
      (eval func-to-test))
    (insert "|")
    (should (string=
             (buffer-substring-no-properties (point-min) (point-max))
             after))))

(defmacro elem/deftest (name before test-func after &optional mode-func)
  "Macro for defining elemental tests - reduces boilerplate"
  (declare (indent 5))
  `(ert-deftest ,(intern (format "elem/test-%s" name)) ()
     (elem/test ,before (quote ,test-func) ,after ,mode-func)))


;; elem/forward-one

(elem/deftest move-to-end-of-arg
    "foo(|abc, def)"
    elem/forward-one
    "foo(abc|, def)")

(elem/deftest move-to-next-arg
    "foo(abc|, def)"
    elem/forward-one
    "foo(abc, def|)")

(elem/deftest attempt-to-move-past-end
    "foo(abc, def|)"
    elem/forward-one
    "foo(abc, def|)")

(elem/deftest move-to-next-arg-when-starting-in-string
    "foo(\"th|at, thing\", 123)"
    elem/forward-one
    "foo(\"that, thing\"|, 123)")

(elem/deftest move-to-next-arg-with-trailing-space
    "foo(abc|, def )"
    elem/forward-one
    "foo(abc, def| )")

(elem/deftest move-to-next-arg-inside-brackets
    "[abc|, 123, haha ]"
    elem/forward-one
    "[abc, 123|, haha ]")

(elem/deftest move-to-next-arg-inside-braces
    "{abc|, 123 , haha}"
    elem/forward-one
    "{abc, 123| , haha}")

(elem/deftest move-to-end-of-arg-complex
    "foo(123, |yyy =\"a,b\")"
    elem/forward-one
    "foo(123, yyy =\"a,b\"|)")

(elem/deftest move-to-next-arg-complex
    "foo(123|, yyy=\"a,b\")"
    elem/forward-one
    "foo(123, yyy=\"a,b\"|)")

(elem/deftest move-to-next-arg-complex
    "foo(123|, yyy =\"a,b\")"
    elem/forward-one
    "foo(123, yyy =\"a,b\"|)")

(elem/deftest when-inside-buffer
    "foo(|abc, def)"
    elem/forward-one
    "foo(abc|, def)")

; Nothing should happen when outside parens/brackets/braces
(elem/deftest move-forward-when-not-in-parens
    "f|oo bah"
    elem/forward-one
    "f|oo bah")

(elem/deftest move-forward-multi-line-0
    "[|foo,\n   \"second, actually\", \n123  ]"
    elem/forward-one
    "[foo|,\n   \"second, actually\", \n123  ]")

(elem/deftest move-forward-multi-line-1
    "[foo|,\n   \"second, actually\", \n123  ]"
    elem/forward-one
    "[foo,\n   \"second, actually\"|, \n123  ]")

(elem/deftest move-forward-multi-line-2
    "[foo,\n   \"second, actually\"|, \n123  ]"
    elem/forward-one
    "[foo,\n   \"second, actually\", \n123|  ]")

(elem/deftest c++-move-forward-with-comments

    "
   x = [foo|,                    /* another, comment */
        \"second, actually\",   // comment
        123 ];
   "
    elem/forward-one
    "
   x = [foo,                    /* another, comment */
        \"second, actually\"|,   // comment
        123 ];
   "
  'c++-mode)

(elem/deftest c++-move-forward-with-comments-2

    "
   x = [foo,                    /* another, comment */
        \"second, actually\"|,   // comment
        123 ];
   "
    elem/forward-one
    "
   x = [foo,                    /* another, comment */
        \"second, actually\",   // comment
        123| ];
   "
  'c++-mode)

;; elem/backward-one

(elem/deftest move-back-to-start-of-arg
    "foo(abc, def|)"
    elem/backward-one
    "foo(abc, |def)")

(elem/deftest move-to-previous-arg
    "foo(abc, |def)"
    elem/backward-one
    "foo(|abc, def)")

(elem/deftest move-to-previous-arg-with-leading-space
    "foo( abc, |def)"
    elem/backward-one
    "foo( |abc, def)")

(elem/deftest move-to-start-of-arg-when-starting-in-string
    "foo(321, \"th|at, thing\", 123)"
    elem/backward-one
    "foo(321, |\"that, thing\", 123)")

(elem/deftest move-to-start-of-arg-when-starting-in-string-with-kwarg
    "foo(meh=\"th|at, thing\", 123)"
    elem/backward-one
    "foo(|meh=\"that, thing\", 123)")

(elem/deftest move-to-previous-arg-inside-brackets
    "[abc, |123, haha ]"
    elem/backward-one
    "[|abc, 123, haha ]")

(elem/deftest move-to-previous-arg-inside-braces
    "{abc, |123 , haha}"
    elem/backward-one
    "{|abc, 123 , haha}")

(elem/deftest move-to-beginning-of-arg-complex
    "foo(123, yyy =\"a,b\"|)"
    elem/backward-one
    "foo(123, |yyy =\"a,b\")")

(elem/deftest move-to-previous-arg-complex
    "foo(123, |yyy =\"a,b\")"
    elem/backward-one
    "foo(|123, yyy =\"a,b\")")


; Nothing should happen when outside parens/brackets/braces
(elem/deftest move-backward-when-not-in-parens
    "foo b|ah"
    elem/backward-one
    "foo b|ah")

(elem/deftest move-backward-multi-line-0
    "[foo,\n   \"second, actually\", \n123|  ]"
    elem/backward-one
    "[foo,\n   \"second, actually\", \n|123  ]")

(elem/deftest move-backward-multi-line-1
    "[foo,\n   \"second, actually\", \n|123  ]"
    elem/backward-one
    "[foo,\n   |\"second, actually\", \n123  ]")

(elem/deftest move-backward-multi-line-2
    "[foo,\n   |\"second, actually\", \n123  ]"
    elem/backward-one
    "[|foo,\n   \"second, actually\", \n123  ]")

(elem/deftest c++-move-backwards-with-comments

    "
   x = [foo,                    /* another, comment */
        \"second, actually\",   // comment
        |123 ];
   "
    elem/backward-one
    "
   x = [foo,                    /* another, comment */
        |\"second, actually\",   // comment
        123 ];
   "
  'c++-mode)

(elem/deftest c++-move-backwards-with-comments-2

    "
   x = [foo,                    /* another, comment */
        |\"second, actually\",   // comment
        123 ];
   "
    elem/backward-one
    "
   x = [|foo,                    /* another, comment */
        \"second, actually\",   // comment
        123 ];
   "
  'c++-mode)

;; elem/forward

(elem/deftest elem/forward-one
    "foo(|abc, def)"
    (elem/forward 1)
    "foo(abc|, def)")

(elem/deftest elem/forward-two
    "foo(|abc, def)"
    (elem/forward 2)
    "foo(abc, def|)")

(elem/deftest elem/forward-too-many
    "foo(|abc, def)"
    (elem/forward 4)
    "foo(abc, def|)")

(elem/deftest elem/forward-negative
    "foo(abc, def|)"
    (elem/forward -2)
    "foo(|abc, def)")

(elem/deftest elem/forward-negative-too-many
    "foo(abc, def|)"
    (elem/forward -3)
    "foo(|abc, def)")

;; elem/transpose

(elem/deftest transpose-simple-forward
    "foo(|abc, def)"
    (elem/transpose 1)
    "foo(def, abc|)")

(elem/deftest transpose-simple-forward-2
    "foo(a|bc, def)"
    (elem/transpose 1)
    "foo(def, abc|)")

(elem/deftest transpose-forward-with-ws
    "foo(  |aaa, bbb  ,  ccc,ddd  )"
    (elem/transpose 1)
    "foo(  bbb, aaa|  ,  ccc,ddd  )")

(elem/deftest transpose-forward-with-ws-2
    "foo(  bbb, aaa|  ,  ccc,ddd  )"
    (elem/transpose 1)
    "foo(  bbb, ccc  ,  aaa|,ddd  )")

(elem/deftest transpose-simple-backward
    "foo(abc, def|)"
    (elem/transpose -1)
    "foo(def|, abc)")

;; FIXME: doesn't work as expected but transpose-words doesn't work
;; well in this situation either
;; (elem/deftest transpose-simple-backward-2
;;   "foo(abc, |def)"
;;   (elem/transpose -1)
;;   "foo(|def, abc)")

(elem/deftest transpose-nested-inner
    "xxx(yyy(|123, zzz), 66)"
    (elem/transpose 1)
    "xxx(yyy(zzz, 123|), 66)")

(elem/deftest transpose-nested-outer
    "xxx(y|yy(123, zzz), 66)"
    (elem/transpose 1)
    "xxx(66, yyy(123, zzz)|)")

(elem/deftest transpose-inside-string-with-comma
    "foo(abc, \"de|f, smell\", thing=12, xxx)"
    (elem/transpose 1)
    "foo(abc, thing=12, \"def, smell\"|, xxx)")

(elem/deftest c++-mode-transpose-0
    "
    x = [foo|,    /* another, comment */
         \"second, actually\",   // comment
         123 ];
    "
    (elem/transpose 1)
    "
    x = [\"second, actually\",    /* another, comment */
         foo|,   // comment
         123 ];
    "
    'c++-mode)

(elem/deftest c++-mode-transpose-1
    "
    x = [foo,    /* another, comment */
         \"seco|nd, actually\",   // comment
         123 ];
    "
    (elem/transpose 1)
    "
    x = [foo,    /* another, comment */
         123,   // comment
         \"second, actually\"| ];
    "
    'c++-mode)
