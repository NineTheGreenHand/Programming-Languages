; Homework #4
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Question #1
; Pre : Takes three arguments, spacing, low and high, assume spacing is
;       a positive number.
; Post: Produces a list of numbers from low to high separated by spacing
;       and in sorted order.

(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

; Question #2
; Pre : Takes a string list and a string value.
; Post: Produces a list of string in which each elements in the
;       string list appended with the given string value with no
;       extra space between the element and string value.

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; Question #3
; Pre : Takes a string list and an int value.
; Post: Produces the ith element of the list where we count from zero and
;       i is the remainder produced when dividing the given int value by
;       the list's length.

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Question #4
; Pre : Takes a stream and a number k and the number is non-negative.
; Post: Returns a list holding the first k values produced by the
;       stream in order.

(define (stream-for-k-steps s k)
  (if (= k 0)
      null
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

; Question #5
; Post: This is a stream of natural numbers except numbers divisible
;       by 6 are negated.

(define funny-number-stream
  (letrec ([f (lambda (i) (cons (if (= (remainder i 6) 0) (- i) i)
                                (lambda () (f (+ i 1)))))])
    (lambda () (f 1))))

; Question #6
; Post: This is a stream of strings where elements alternate between
;       "dan.jpg" and "dog.jpg" starting with "dan.jpg".

(define dan-then-dog
  (letrec ([odd (lambda () (cons "dan.jpg" even))]
           [even (lambda () (cons "dog.jpg" odd))])
    odd))

; Question #7
; Pre : Takes a stream.
; Post: If steam would produce v for its ith element, then
;       this function would produce the pair (1 . v) for its
;       ith element. 

(define (stream-add-one s)
  (letrec ([f (lambda (i) (cons (cons 1 (car (i))) (lambda () (f (cdr (i))))))])
    (lambda () (f s))))

; Question #8
; Pre : Takes two lists. Both are non-empty.
; Post: Returns a stream that produces pairs where the first part is from
;       xs and the second part from ys, the stream cycles forever through
;       the lists. 

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (lambda ()
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (f (+ n 1)))))])
          (f 0)))

; Question #9
; Pre : Takes a value v and a vector vec. 
; Post: Behave like Racket's assoc livrary function except
;       (1)it processes a vector (Racket’s name for an array) instead of a list
;       (2)it allows vector elements not to be pairs in which case it skips them
;       (3)it always takes exactly two arguments.
;       Returns #f if no vector elements is a pair with a car field equal to v,
;       else return the first pair with an equal car field.

(define (vector-assoc v vec)
  (letrec ([length (vector-length vec)]
           [f (lambda (len)
                (cond [(= len length) #f]
                      [(and (pair? (vector-ref vec len)) (equal? (car (vector-ref vec len)) v))
                       (vector-ref vec len)]
                      [#t (f (+ len 1))]))])
    (f 0)))

; Question #10
; Pre : Takes a list and a positive number.
; Post: Returns a function that takes one argument v
;       which returns the same thing that (assoc v xs) would return.
;       But we using an n-element cache of recent results to possibly
;       make this function faster than just calling assoc.

(define (caching-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [current 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (cond [ans ans]
                        [#t (let ([new-ans (assoc v xs)])
                              (begin (vector-set! memo current new-ans)
                                     (set! current (remainder (+ 1 current) n))
                                     new-ans))])))])
    f))

; Question #11
; Post : This is a macro that is used like (while-greater e1 do e2)
;        (1)It evaluates e1 exactly once.
;        (2)It evaluates e2 at least once.
;        (3)It keeps evaluating e2 until and only until the result is not
;           a number greater than the result of the evaluation of e1.
;        (4)Assuming evaluation terminates, the result is #t
;        (5)Assume e1 and e2 produce numbers, this macro can do anything or
;           fail otherwise.

(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (begin (letrec ([value e1]
                     [f (lambda (value) (if (> e2 value) (f value) #t))])
              (f value)))]))
                     
         
                      
           
  
  























  