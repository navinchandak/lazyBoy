(define (make-my-move)
  (let*([sym (if(equal? iam 'w) 1 0)]
        [amount-of-material (total-material cb)]
        [depth-of-tree 
         (cond[(< amount-of-material 1000) 4]
              [(< amount-of-material 2000) 4]
              [(< amount-of-material 3000) 4]
              [else 4])])   
    (artificial-intelligence b from-this 0 0 0 0 0 0 sym depth-of-tree)))


(define (myundo-move cb a)
  (foldr (lambda(f a) (f)) void (car moves))
  (set! moves (cdr moves))) 

(define (artificial-intelligence cb from-this wa wb wc ba bb bc to-move depth)
  (define (alphaBeta-Max-root depth-left cb)
    (if (= depth-left 0) (evaluate cb from-this 1 wa wb wc ba bb bc)
        (let ([list-white-moves (valid-moves cb from-this 'w)])
          (define ans 0)
          (define alpha -1000000)
          (define (all-moves l1)
            (if(null? l1) ans
               (let([score (alphaBeta-Min alpha 1000000 (- depth-left 1) (make-the-move cb (car l1)))])
                 (cond[(> score alpha)
                       (begin
                         (set! alpha score)
                         (set! ans (car l1))
                         (myundo-move cb (car l1))
                         (all-moves (cdr l1)))]
                      [else (begin
                              (myundo-move cb (car l1))
                              (all-moves (cdr l1)))]))))
          (all-moves list-white-moves))))
;  (let ([a (king-pos 'b)])
;    (not ((not-incheck (car a) (cdr a)) a)))
  (define (alphaBeta-Min-root depth-left cb)
    (if (= depth-left 0) (- (evaluate cb from-this 0 wa wb wc ba bb bc))
        (let ([list-black-moves (valid-moves cb from-this 'b)])
          (define ans 0)
          (define beta 1000000)
          (define (all-moves l1)
            (if(null? l1) ans
               (let([score (alphaBeta-Max -1000000 beta (- depth-left 1) (make-the-move cb (car l1)))])
                 (cond[(< score beta)
                       (begin
                         (set! beta score)
                         (set! ans (car l1))
                         (myundo-move cb (car l1))
                         (all-moves (cdr l1)))]
                      [else (begin
                              (myundo-move cb (car l1))
                              (all-moves (cdr l1)))]))))
          (all-moves list-black-moves))))
  
  (define (alphaBeta-Max alpha beta depth-left cb)
    (if (= depth-left 0) (evaluate cb from-this 1 wa wb wc ba bb bc)
        (let ([list-white-moves (valid-moves cb from-this 'w)])
          (define (all-moves l1)
            (if(null? l1) alpha
               (begin 
                 (let([score (alphaBeta-Min alpha beta (- depth-left 1) (make-the-move cb (car l1)))])
                   (cond[(>= score beta) 
                         (begin
                           (myundo-move cb (car l1))
                           beta)]
                        [(> score alpha)
                         (begin
                           (set! alpha score)
                           (myundo-move cb (car l1))
                           (all-moves (cdr l1)))]
                        [else (begin
                                (myundo-move cb (car l1))
                                (all-moves (cdr l1)))])))))
          (cond [(and (null? list-white-moves) (not (let ([a (king-pos 'w)]) (not ((not-incheck (car a) (cdr a)) a))))) 999999]
                [(null? list-white-moves) -999999]
                [else (all-moves list-white-moves)]))))
  (define (alphaBeta-Min alpha beta depth-left cb)    
    (if (= depth-left 0) (- (evaluate cb from-this 0 wa wb wc ba bb bc))
        (let ([list-black-moves (valid-moves cb from-this 'b)])
          (define (all-moves l1)
            (if(null? l1) beta
               (let([score (alphaBeta-Max alpha beta (- depth-left 1) (make-the-move cb (car l1)))])
                 (cond[(<= score alpha) 
                       (begin
                         (myundo-move cb (car l1))
                         alpha)]
                      [(< score beta)
                       (begin
                         (set! beta score)
                         (myundo-move cb (car l1))
                         (all-moves (cdr l1)))]
                      [else (begin
                              (myundo-move cb (car l1))
                              (all-moves (cdr l1)))]))))
          (cond[(and (null? list-black-moves) (not (let ([a (king-pos 'b)]) (not ((not-incheck (car a) (cdr a)) a))))) -999999]
               [(null? list-black-moves) 999999]
               [else (all-moves list-black-moves)]))))
  (cond[(= to-move 1) (make-final-move (alphaBeta-Max-root depth cb))]
       [else (make-final-move (alphaBeta-Min-root depth cb))]))

(define (make-the-move cb a)
  (begin
    (board-move (string-append (make-SAN (car a)) (make-SAN (cdr a))))
    cb))

(define (make-final-move p1)
  (cond[(not (pair? p1)) (void)]
       [else (move-maker (car p1) (cdr p1))]))


