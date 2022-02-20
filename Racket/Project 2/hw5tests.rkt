#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   
   ;; The followings are my tests.
   ;; Problem 1(a) test cases.
   (check-equal? (racketlist->mupllist '(1 2 3 4)) (apair 1 (apair 2 (apair 3 (apair 4 (munit))))))
   (check-equal? (racketlist->mupllist '((int 3) (int 6) (int 9)))
                 (apair '(int 3) (apair '(int 6) (apair '(int 9) (munit)))))
   (check-equal? (racketlist->mupllist null) (munit))
   (check-equal? (racketlist->mupllist '("1" 2 3 "haha"))
                 (apair "1" (apair 2 (apair 3 (apair "haha" (munit))))))

   ;; Problem 1(b) test cases.
   (check-equal? (mupllist->racketlist (apair "1" (apair 2 (apair 3 (apair "haha" (munit))))))
                 '("1" 2 3 "haha"))
   (check-equal? (mupllist->racketlist (munit)) null)
   (check-equal? (mupllist->racketlist (apair '(int 3) (apair '(int 6) (apair '(int 9) (munit)))))
                 '((int 3) (int 6) (int 9)))
   (check-equal? (mupllist->racketlist (apair 1 (apair 2 (apair 3 (apair 4 (munit))))))
                 '(1 2 3 4))

   ;; Problem 2 test cases.
   (check-equal? (eval-exp (int 1)) (int 1))
   (check-equal? (eval-exp (isgreater (int 3) (int 4))) (int 0))
   (check-equal? (eval-exp (isgreater (int 4) (int 3))) (int 1))
   (check-equal? (eval-exp (isgreater (add (int 1) (int 2)) (int 3))) (int 0))
   (check-equal? (eval-exp (ifnz (int 1) (int 2) (int 3))) (int 2))
   (check-equal? (eval-exp (ifnz (int 0) (int 2) (int 3))) (int 3))
   (check-equal? (eval-exp (ifnz (add (int 1) (int -1)) (int 2) (int 3))) (int 3))
   (check-equal? (eval-exp (apair (int 1) (int 3))) (apair (int 1) (int 3)))
   (check-equal? (eval-exp (apair (int 1) (apair (int 2) (apair (int 3) (munit)))))
                 (apair (int 1) (apair (int 2) (apair (int 3) (munit)))))
   (check-equal? (eval-exp (first (apair (int 1) (int 3)))) (int 1))
   (check-equal? (eval-exp (second (apair (int 1) (int 3)))) (int 3))
   (check-equal? (eval-exp (munit)) (munit))
   (check-equal? (eval-exp (ismunit (munit))) (int 1))
   (check-equal? (eval-exp (ismunit (int 2))) (int 0))
   (check-equal? (eval-exp (mlet "x" (int 1) (var "x"))) (int 1))
   (check-equal? (eval-exp (mlet "x" (add (int 1) (int 2)) (var "x"))) (int 3))
   ;; fun, closure and call will be tested together. 
   (check-equal? (eval-exp (call (closure (list (cons "m" (int 1))) (fun null "m" (add (var "m") (int 3)))) (int 6)))
                 (int 9))
   (check-equal? (eval-exp (call (closure (list (cons "h" (int 1))) (fun null "y" (add (var "h") (int 3)))) (int 6)))
                 (int 4))
   (check-equal? (eval-exp (call (closure (list (cons "x" (int 1))) (fun "x" "h" (add (var "h") (int 3)))) (int 6)))
                 (int 9))

   ;; Problem 3(a) test cases
   (check-equal? (eval-exp (ifmunit (munit) (int 1) (int 2))) (int 1))
   (check-equal? (eval-exp (ifmunit (add (int 1) (int 2)) (int 1) (int 2))) (int 2))
   (check-equal? (eval-exp (ifmunit (munit) (add (int 3) (int 6)) (int 2))) (int 9))

   ;; Problem 3(b) test cases
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2))) (add (var "x") (var "y")))) (int 3))
   (check-equal? (eval-exp (mlet* (list (cons "x" (add (int 1) (int 2))) (cons "y" (var "x"))) (var "y"))) (int 3))
   (check-equal? (eval-exp (mlet* (list (cons "x" (munit))) (var "x"))) (munit))

   ;; Problem 3(c) test cases
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 2) (int 3))) (int 2))
   (check-equal? (eval-exp (ifeq (int 0) (int 1) (int 2) (int 3))) (int 3))
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (munit) (apair (int 1) (int 2)))) (apair (int 1) (int 2)))

   ;; Problem 4(a) and 4(b) will be tested together in here.
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 3))
                                 (racketlist->mupllist
                                  (list (int 1) (int 2) (int 3))))))
                 null)             
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
