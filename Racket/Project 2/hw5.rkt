;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1 
;; Problem 1(a)
;; Pre : Takes a racket list.
;; Post: Make a MUPL list based on the given racket list.
(define (racketlist->mupllist rkt-list)
  (if (null? rkt-list)
      (munit)
      (apair (car rkt-list) (racketlist->mupllist (cdr rkt-list)))))

;; Problem 1(b)
;; Pre : Takes a MUPL list.
;; Post: Make a racket list based on the given MUPL list.
(define (mupllist->racketlist mlist)
  (if (munit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; int case
        [(int? e) e]
        ;; isgreater case
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2)) (int 1) (int 0))
               (error "MUPL isgreater applied to non-number")))]
        ;; ifnz case
        [(ifnz? e)
         (let ([v (eval-under-env (ifnz-e1 e) env)])   
           (if (int? v)
               (if (= (int-num v) 0)
                   (eval-under-env (ifnz-e3 e) env)
                   (eval-under-env (ifnz-e2 e) env))
               (error "MUPL ifnz applied to non-number")))]
        ;; fun case
        [(fun? e) (closure env e)]
        ;; mlet case
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        ;; apair case
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        ;; first case
        [(first? e)
         (let ([v (eval-under-env (first-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL first applied to non-pair")))]
        ;; second case
        [(second? e)
         (let ([v (eval-under-env (second-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL second applied to non-pair")))]
        ;; munit case
        [(munit? e) e]
        ;; ismunit case
        [(ismunit? e)
         (if (munit? (eval-under-env (ismunit-e e) env)) (int 1) (int 0))]
        ;; closure case
        [(closure? e) e]
        ;; call case
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([c-fun (closure-fun v1)]
                      [c-env (closure-env v1)]
                      [f-name (fun-nameopt c-fun)]
                      [nul-env (cons (cons (fun-formal c-fun) v2) c-env)]
                      [notnul-env (cons (cons f-name v1) nul-env)]
                      [new-env (if (null? f-name) nul-env notnul-env)])
                 (eval-under-env (fun-body c-fun) new-env))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
;; Problem 3(a)
;; Pre : Takes three MUPL expressions. (e1 e2 e3)
;; Post: If first expression is munit, then it
;;       evaluates e2, else evaluates e3.
(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

;; Problem 3(b)
;; Pre : Takes a racket list of racket pairs, and a final MUPL expression.
;;       Pairs are like '((s1 . e1) ... (sn . en)), MUPL of e(n+1).
;; Post: Returns a MUPL expression whose value is the final MUPL expression
;;       evaluated in an environment where each si is a variable bound to the
;;       result of evaluating the corresponding ei for 1<= i <= n.
(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (caar bs) (cdar bs) (mlet* (cdr bs) e2))))

;; Problem 3(c)
;; Pre : Takes four MUPL expressions. (e1, e2, e3, e4)
;; Post: Similar as ifnz, but only when e1 = e2 (if integer), e3 is evaluated.
;;       else, evaluate e4. If e1 and e2 are not integer, error will show up.
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifnz (isgreater (var "_x") (var "_y"))
               e4
               (ifnz (isgreater (var "_y") (var "_x")) e4 e3))))

;; Problem 4
;; Problem 4(a)
;; Pre : Takes a MUPL function.
;; Post: Returns a MUPL function that is similar to the filter function, that
;;       takes a MUPL list and applies the function to every element of the
;;       list returning a new MUPL list with all the elements for which the
;;       function returns a number other than zero. An error would occur if the
;;       function returns a non-number. 
(define mupl-filter
  (fun null "filter"
       (fun "recursion" "list"
            (ifmunit (var "list")
                     (munit)
                     (ifnz (call (var "filter") (first (var "list")))
                           (apair (first (var "list")) (call (var "recursion")
                                                             (second (var "list"))))
                           (call (var "recursion") (second (var "list"))))))))

;; Problem 4(b)
;; Pre : Takes an MUPL integer i.
;; Post: Returns a MUPL function that takes a MUPL list of MUPL integers
;;       and returns a new MUPL list of MUPL integers containing the elements
;;       of the input list (in original order) that are greater than i.
(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "i"
             (call (var "filter") (fun null "x" (isgreater (var "x") (var "i")))))))





