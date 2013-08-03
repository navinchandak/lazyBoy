;Valid Moves
(define from-this (build-vector 8 (lambda(x) (make-vector 8 '()))))
(define (valid-moves board f c)
  (define vec (make-vector 8 '()))
  (define (add-pos c)
    (cond [(isquen c) 0]
          [(ispawn c) 1]
          [(isbish c) 2]
          [(isnite c) 3]
          [(isruke c) 4]
          [(isking c) 5]))
  (define (valid-moves-h)
    (vec-foldr 
     (lambda(fst i acc) 
       (vec-foldr (lambda(fst j acc) 
                    (2dvec-set! from-this i j '())
                    (if (equal? c (colour (board-ref i j)))
                        (let ([val (valid i j)])
                        (vector-set! vec 
                                     (add-pos (board-ref i j)) 
                                     (append (map (lambda(pair) (cons (cons i j) pair))
                                                val)
                                             (vector-ref vec (add-pos (board-ref i j)))))
                        (2dvec-set! from-this i j val))
                        void)) void fst))
     void b))
  (valid-moves-h)
  (vec-foldr (lambda(fst i acc) (append fst acc)) '() vec)
  )
;;;;;;;;;;;;;;;;Valid moves from a single location;;;;;;;;;;;;;;;
(define (valid x y)
  (define (h c)
    (cond 
      [(iszero c) '()]
      [(isking c) 
       (append 
        (filter 
         (not-incheck x y)
         (filter 
          (diff-colour/null c)
          (filter 
           in-bound 
           (list (cons (+ x 1) (+ y 1))
                 (cons (+ x 1) (- y 1))
                 (cons (- x 1) (- y 1))
                 (cons (- x 1) (+ y 1))
                 (cons (+ x 1) y)
                 (cons (- x 1) y)
                 (cons x (+ y 1))
                 (cons x (- y 1))))))
        (filter (lambda(pair) 
                  (not (cant-castle (begin 
                                      (string (c-pos-inv y)
                                              (r-pos-inv x) 
                                              (c-pos-inv (cdr pair))
                                              (r-pos-inv (car pair)))))))
                (castle-options c))
        )]
      [(isnite c) 
       (filter 
        (diff-colour/null c)
        (filter 
         in-bound
         (list (cons (+ x 1) (+ y 2))
               (cons (+ x 1) (- y 2))
               (cons (+ x 2) (+ y 1))
               (cons (+ x 2) (- y 1))
               (cons (- x 1) (- y 2))
               (cons (- x 1) (+ y 2))
               (cons (- x 2) (+ y 1))
               (cons (- x 2) (- y 1)))))]
      [(isruke c)
       (append (step x y c 1 0 #f)
               (step x y c 0 1 #f)
               (step x y c -1 0 #f)
               (step x y c 0 -1 #f))]
      [(isbish c) 
       (append (step x y c 1 1 #f)
               (step x y c -1 -1 #f)
               (step x y c 1 -1 #f)
               (step x y c -1 1 #f))]
      [(isquen c)
       (append (step x y c 1 0 #f)
               (step x y c 0 1 #f)
               (step x y c -1 0 #f)
               (step x y c 0 -1 #f)
               (step x y c 1 1 #f)
               (step x y c -1 -1 #f)
               (step x y c 1 -1 #f)
               (step x y c -1 1 #f))]
      [(ispawn c)
       (let ([op (if (isblack c) + -)])
         (append (filter empty 
                         (filter in-bound
                                 (list (cons ((if (isblack c) + -) x 1) y))))
                 (if (and (in-bound (cons (op x 1) y))
                          (isrank2 x c) (empty (cons (op x 1) y))
                          (empty (cons (op x 1) y))) 
                     (filter empty (list (cons (op x 2) y))) '())
                 (if (and (in-bound (cons x (+ y 1))) 
                          (in-bound (cons (op x 1) (+ y 1)))                               
                          ((opp-colour c) (cons (op x 1) (+ y 1))) )
                     (list (cons (op x 1) (+ y 1))) '())
                 (if (and (in-bound (cons (op x 1) (- y 1)))
                          ((opp-colour c) (cons (op x 1) (- y 1))))
                     (list (cons (op x 1) (- y 1))) '())
                 (if (not (var-ref 7))
                     (list )
                     (if (and (adjacent (cons x y) (var-ref 7)) ((opp-colour c) (var-ref 7)))
                         (list (cons (op (car (var-ref 7)) 1) (cdr (var-ref 7)))) (list )))
                 )
         )]))
  (filter 
   (no-problem-for-king x y)
   (h (board-ref x y))))

;;;;;;;;;;;;valid helper functions

;;;;;;;;;;common filtering prdicates used for valid moves
(define in-bound (lambda(pair) (not (out-of-bound pair))))
(define out-of-bound (lambda(pair) 
                       (not (null? (filter (lambda(int) (or (< int 0) (> int 7)))
                                           (list (car pair) (cdr pair)))))))(define (opp-colour c)
  (define (h a b)
    (or (and (equal? a 'w) (equal? b 'b)) (and (equal? a 'b) (equal? b 'w))))
  (lambda(pair) (h (colour c) (colour (board-ref (car pair) (cdr pair))))))
(define (same-colour c)
  (lambda (pair) (equal? (colour (board-ref (car pair) (cdr pair))) (colour c))))
(define (diff-colour/null c)
  (lambda(pair) (not ((same-colour c) pair))))
(define empty (lambda(pair) (equal? 0 (board-ref (car pair) (cdr pair)))))


(define (castling-list p)          ;this is for checking empty
  (define (h p)
    (cond [(= p 1) (list "f8" "g8")]
          [(= p 2) (list "f1" "g1")]
          [(= p 3) (list "b8" "c8" "d8")]
          [(= p 4) (list "b1" "c1" "d1")]))
  (map str->pair (h p)))
(define (getp-c str)
  (cond [(equal? str "e8g8") 1]
        [(equal? str "e1g1") 2]
        [(equal? str "e8c8") 3]
        [(equal? str "e1c1") 4]))
(define (nocheck-reqd-list p)  ;;;check for no risk
  (define (h p)
    (cond [(= p 1) (list "e8" "f8" "g8")]
          [(= p 2) (list "e1" "f1" "g1")]
          [(= p 3) (list "c8" "d8" "e8")]
          [(= p 4) (list "c1" "d1" "e1")]))
  (map str->pair (h p)))
(define (rooke-move-c p)
  (cond [(= p 1) "h8f8"]
        [(= p 2) "h1f1"]
        [(= p 3) "a8d8"]
        [(= p 4) "a1d1"]))
(define (cant-castle i)
  (or (begin  (not (var-ref (getp-c i))))
      (begin  (not (null? (filter (lambda(p) (not (empty p))) (castling-list (getp-c i))))))
      (begin  (not (foldr (lambda(f a) 
                            (if (not a) #f
                                ((not-incheck (r-pos (string-ref i 1)) (c-pos (string-ref i 0))) f)))
                          #t
                          (nocheck-reqd-list (getp-c i)))))
      (begin  #f)))
(define (castle-options c)
  (define (h)
    (if (equal? c #\K) 
        (list (if ks-c-w (cons 7 6) #f)
              (if qs-c-w (cons 7 2) #f))
        (list (if ks-c-b (cons 0 6) #f)
              (if qs-c-b (cons 0 2) #f))
        ))
  ;
  (filter (lambda(p) p) (h)))

;;;;;;;;checking king's safety
(define (incheck-pos x y a b)
  (set! not-incheck (lambda(x y) (lambda(pair) #t)))
  (let* ([val (if (and (= x a) (= y b)) #f (board-move-pos-nolog x y a b))]
         [danger (all-dependencies a b #f)]
         [res (foldr (lambda(fst acc) (if acc #t
                                          (not (not (member (cons a b) (valid (car fst) (cdr fst))))))) #f danger)])
    (if (equal? val #f) void (begin
                               (board-move-pos-nolog a b x y)
                               (board-set! a b val)))
    (set! not-incheck (lambda(x y) (lambda(pair) (not (incheck-pos x y (car pair) (cdr pair))))))
    res))

(define (not-incheck x y)
  (lambda(pair) (not (incheck-pos x y (car pair) (cdr pair)))))
(define (board-move-pos-nolog a b c d)
  (define ret (board-ref c d))
  (board-set! c d (board-ref a b))
  (board-set! a b 0)
  ret)


(define no-problem-for-king
  (lambda(x y)
    (lambda(pair)
      (board-move (string-append (make-SAN (cons x y)) (make-SAN pair)))
      (let* ([a (king-pos (colour (board-ref (car pair) (cdr pair))))]
             [res (if ((not-incheck (car a) (cdr a)) a) #t #f)])
        (undo-move)
        res))))
(define (king-pos c)
  (vec-foldr 
   (lambda (fst i acc)
     (let ([res (vec-foldr 
                 (lambda(fst j acc) (if (equal? (board-ref i j) (if (equal? c 'b) #\k #\K)) (cons i j) acc)) #f fst)])
       (if (equal? res #f) acc
           res))) #f b))
(define (check-for-check c)
  (let ([vec (valid-moves 'b 'w c)]
        [a (king-pos c)])
    (cond [(and (not ((not-incheck (car a) (cdr a)) a)) (null? vec)) (tell-x (losing-string c)) (set! state-force #t)]
          [(null? vec) (tell-x (stalemate-string)) (set! state-force #t)]
          [else void])))
(define (losing-string c)
  (if (equal? c 'b) "1-0 {White mates}"
      "0-1 {Black mates}"))
(define (stalemate-string)
  "1/2-1/2 {Stalemate}")
