#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; 1st problem :

(define (sequence low high stride )
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

;(check-equal? (sequence 0 5 1 ) (list 0 1 2 3 4 5) "Sequence test")

;; 2nd problem :

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix )) xs))

;(check-equal? (string-append-map 
                 ; (list "dan" "dog" "curry" "dog2") 
                 ; ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

;; 3rd problem :

(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

 ;(check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
 ;(check-equal? (list-nth-mod (list  1 2 3 4 5) 5) 1 "list-nth-mod test")
 ;(check-equal? (list-nth-mod (list  1 2 3 4) 10) 3 "list-nth-mod test")
 ;(check-equal? (list-nth-mod (list 1 2 3 5) 0) 1 "list-nth-mod test")



;; 4th problem :
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s))(stream-for-n-steps (cdr (s)) (- n 1)))))

;(define ones (lambda () (cons 1 ones)))
;(check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")


;; 5th problem :

(define funny-number-stream
  (letrec [(neg (lambda (x) (cond [(= 0 (remainder x 5)) (- 0 x)]
                                  [(< x 0)  (+ 2(- 0 x ))]
                                  [#t x])))
                              
           (f (lambda (x) (cons x (lambda () (f (neg(+ x 1)))))))]
     (lambda ()(f 1))))
  
  ; (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
  ; (check-equal? (stream-for-n-steps funny-number-stream 10) (list 1 2 3 4 -5 6 7 8 9 -10) "funny-number-stream test")
  ; (check-equal? (stream-for-n-steps funny-number-stream 5) (list 1 2 3 4 -5) "funny-number-stream test")
  ; (check-equal? (stream-for-n-steps funny-number-stream 4) (list 1 2 3 4) "funny-number-stream test")

;; 6th problem :

(define dan-then-dog (letrec
                         [(f (lambda (x) (cons "dan.jpg" (lambda () (f2 (- x 1))))))
                          (f2 (lambda (x) (cons "dog.jpg" (lambda () (f (- x 1))))))]
                       (lambda ()(f 1))))
;(check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
;(check-equal? (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg") "dan-then-dog test")
;(check-equal? (stream-for-n-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg") "dan-then-dog test")

;; 7th problem :

(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

   ;(check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
   ;(check-equal? (stream-for-n-steps (stream-add-zero nats) 2) (list (cons 0 1)(cons 0 2)) "stream-add-zero test")
   ;(check-equal? (stream-for-n-steps (stream-add-zero nats) 3) (list (cons 0 1) (cons 0 2) (cons 0 3)) "stream-add-zero test")

;; 8th problem :
 (define (cycle-lists xs ys )
   (letrec ([cycle (lambda (x org) (if (null? x)
                                       org
                                       x))]
            [f (lambda (x y) (cons (cons (car x) (car y)) (lambda () (f (cycle (cdr x) xs) (cycle (cdr y) ys)))))])
     (lambda () (f xs ys))))


 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")))
 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 4) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b")))
 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b" "c")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "c")))
 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 ) (list "a" "b" "c")) 3) (list (cons 1 "a") (cons 2 "b") (cons 1 "c")))
 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 ) (list "a" "b")) 3) (list (cons 1 "a") (cons 1 "b") (cons 1 "a")))
 ; (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 6) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")
 ;                                                                                    (cons 1 "b")(cons 2 "a")(cons 3 "b")))

;; 9th problem :
(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec n)
            (cond [(equal? (vector-length vec) n)#f]
                  [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v)
                                                  (vector-ref vec n)
                                                  (f v vec (+ n 1)))]
                  [#t (f v vec (+ n 1))]))])
    (f v vec 0)))
           
 ;(check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
; (check-equal? (vector-assoc 6 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test")
; (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 4 2))) (cons 4 1) "vector-assoc test")
; (check-equal? (vector-assoc 4 (vector (cons 2 1) 4 (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
; (check-equal? (vector-assoc 4 (vector 4 4 (cons 4 1) (cons 4 4))) (cons 4 1) "vector-assoc test")


;; 10th problem :
(define (cached-assoc xs n)
    (letrec ([memo (make-vector n #f)]
             [slot 0]
             [f (lambda (x)
                (let ([ans (vector-assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (assoc x xs)])
                        (begin
                          (vector-set! memo slot new-ans)
                          (if (< slot (- n 1) )
                          (set! slot (+ slot 1))
                          (set! slot 0))
                          new-ans)))))])
                      f))

                        
  ;(check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")                                       