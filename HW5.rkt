#lang racket
;; Programming Languages, Homework 5


(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist Mlst)
  (if (aunit? Mlst)
      null
      (cons (apair-e1 Mlst) (mupllist->racketlist (apair-e2 Mlst)))))


;(check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
;(check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

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
;; "in real life" it would be a helper function of eval-exp.(define mupl-map (lambda (f) (lambda (lst)
                             
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
        [(int? e) e]
        [(ifgreater? e)
         (let  ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
            (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                  (eval-under-env (ifgreater-e3 e) env)
                  (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL addition applied to non-number")))]
        [(mlet? e)
         (let ([val (eval-under-env (mlet-e e) env)])
            (eval-under-env (mlet-body e) (cons (cons (mlet-var e) val) env )))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "not a pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "not a pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
             (if (aunit? v)
                 (int 1)
                 (int 0)))]
        [(closure? e)e]
        [(fun? e) (closure env e) ]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e)env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (if (fun-nameopt (closure-fun v1))
                (eval-under-env (fun-body (closure-fun v1)) (cons (cons (fun-nameopt (closure-fun v1)) v1) (cons (cons (fun-formal (closure-fun v1))v2) (closure-env v1)))) 
                 (eval-under-env (fun-body (closure-fun v1)) (cons (cons (fun-formal (closure-fun v1))v2) (closure-env v1))))
               (error "not a closure")))]
         [(aunit? e)(aunit)]        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


; (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
; (check-equal? (eval-exp (add (int 3) (int 4))) (int 7) "ifgreater test")
; (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
; (check-equal? (eval-exp (apair (add (int 3) (int 4)) (int 4))) (apair (int 7) (int 4)) "ifgreater test")
; (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
;(check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
; (check-equal? (eval-exp (isaunit (int 2))) (int 0) "isaunit test")
;(check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1)) ) (int 8) "call test")
      
; Problem 3

(define (ifaunit e1 e2 e3)(ifgreater (isaunit e1) (int 0) e2 e3) )
;(check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
;(check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")

(define (mlet* lstlst e2)
  (cond [(null? lstlst ) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst ) e2))]))
 
;(check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
;(check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 5)))(add (var "x") (var "y")))) (int 15) "mlet* test")

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (if (equal? (var "_x") (var "_y"))
                  e3
                  e4))))

 ; (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "map" "lst"
            (ifaunit (var "lst")
                     (var "lst")
                     (apair (call (var "f") (fst (var "lst"))) (call (var "map") (snd (var "lst"))))))))
                                 
;(check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
;              (apair (int 8) (aunit)) "mupl-map test")

(define mupl-mapAddN 
  (mlet "map" mupl-map
      (fun #f "i"
           (fun #f "lst" (call (call (var "map") (fun #f "x"(add (var "i")(var "x") ))) (var "lst") )))))

; (check-equal? (mupllist->racketlist
;   (eval-exp (call (call mupl-mapAddN (int 7))
;                   (racketlist->mupllist 
;                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
;
(define f (lambda (x) (lambda (y) 4)))
(f 3 )
