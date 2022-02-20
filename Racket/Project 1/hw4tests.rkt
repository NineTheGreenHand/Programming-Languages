#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; #1 test cases
(equal? (sequence 2 0 9) '(0 2 4 6 8))
(equal? (sequence 1 8 1) '())

; #2 test cases
(define firstList (list "hell" "d" "t" "fo" "potat"))
(equal? (string-append-map firstList "o") '("hello" "do" "to" "foo" "potato"))
(equal? (string-append-map null "o") '())

; #3 test cases
(equal? (list-nth-mod firstList 3) "fo")
; (equal? (list-nth-mod firstList -1) This one will raise "list-nth-mod: negative number" error.
; (equal? (list-nth-mod null 3) This one will raise "list-nth-mod: empty list" error.

; #4 test cases
(equal? (stream-for-k-steps funny-number-stream 0) '())
(equal? (stream-for-k-steps funny-number-stream 6) '(1 2 3 4 5 -6))
(equal? (stream-for-k-steps dan-then-dog 5) '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))

; #5 and #6 can be tests from #4 test cases
; #7 is tested from the below (visual-only-one) case
; #8 returns a stream.
(equal? (stream-for-k-steps (cycle-lists '(1 2 3) '("a" "b")) 5) '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a")))

; #9 test cases
(equal? (vector-assoc 3 '#((1 . 2) (3 . 4) (5 . 6))) '(3 . 4))
(equal? (vector-assoc 0 '#((1 . 2) (3 . 4) (5 . 6))) #f)

; #10 test cases
(equal? ((caching-assoc (list (cons 1 2) (cons 3 4) (cons 5 6)) 3) 3) '(3 . 4))
(equal? ((caching-assoc (list (cons 1 2) (cons 3 4) (cons 5 6)) 3) 2) #f)

; #11 is a macro
(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
; should evaluates to: "x""x""x""x""x"#t
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
; should evaluates to: "x"#t



; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-k-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))
