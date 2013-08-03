;Please note that the artificial intelligence is not so robust and we are working on its completion. As of now, only the part for
;black as the cpu player has been completed.
;Extensive start of game conditions have to be given.
;Castling has to be incorporated.
;Stalemate has to be dealt with.

;A number of heuristic conjectures have been used in the heuritic evaluation function. These are self-explanatory in the following defines.
;variable values have been assigned to these, and these may be altered, to significantly affect the final results.



(define pawn-val 100)
(define bishop-val 330)
(define knight-val 330)
(define rook-val 520)
(define queen-val 980)
(define king-val 100000)
(define isolated-pawn-val (vector -12 -14 -16 -20 -20 -16 -14 -12))
(define doubled-pawn-val -12)
(define backward-pawn-val -4)
(define half-open-val -6)
(define pawn-position-val 2)
(define passed-pawn-val 10)
(define knight-centre-val 5)
(define knight-king-val 1)
(define bishop-bonus-val 100)
(define bishop-squares-attacking-val 50)
;(define bishop-squares-attacking-val 3)
(define bishop-threatening-val 50)
(define bishop-centre-val 5)
;(define bishop-centre-val 5)
(define bishop-adjacent-threatening-val 10)
(define rook-squares-attacking-val 5)
(define rook-self-pawn-absent-val 10)
(define rook-opp-pawn-absent-val 4)
(define rook-king-val 1)
(define queen-king-val 2)
(define queen-squares-attacking-val 15)
;(define queen-squares-attacking-val 3)
(define king-centre-val 12)
(define open-king-val -20)
(define isolated-king-val -8)
(define check-king-penalty 15)
(define castling-bonus-val 10)
(define cannot-castle-val -40)

;widely different results have been used for different situations of gameplay
;the depth of search has been made variable for better results
;the minimax algorithm along with alpha beta pruning has been used in order to make a move, along with a heuristic evaluation function.

;##############################################################################################

; evaluation exclusively for black to move

       
(define (evaluate-black cb from-this wa wb wc ba bb bc)

(define (gameplay-eval cb)
  (let ((a (total-material cb)))
    (cond[(> a 6500) -1]
         [(> a 3500) 0]
         [else 1])))  
  
(define (total-material cb)
   (define (support)
    (define ans 0)
    (define (outer i j)
      (if(> i 7) ans
         (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ((a (my-board-ref cb i j)))
                     (cond[(number? a)(inner i (+ j 1))]
                          [(equal? #\p a)(begin
                                           (set! ans (+ ans pawn-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\b a)(begin
                                           (set! ans (+ ans bishop-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\n a)(begin
                                           (set! ans (+ ans knight-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\r a)(begin
                                           (set! ans (+ ans rook-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\q a)(begin
                                           (set! ans (+ ans queen-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\k a)(begin
                                           (set! ans (+ ans 500))
                                           (inner i (+ j 1)))]
                          [(equal? #\P a)(begin
                                           (set! ans (+ ans pawn-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\B a)(begin
                                           (set! ans (+ ans bishop-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\N a)(begin
                                           (set! ans (+ ans knight-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\R a)(begin
                                           (set! ans (+ ans rook-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\Q a)(begin
                                           (set! ans (+ ans queen-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\K a)(begin
                                           (set! ans (+ ans 500))
                                           (inner i (+ j 1)))]
                          [else (inner i (+ j 1))]))]))
    (outer 0 0))
  (support))
  
(define (pawn-list cb a b gameplay)
  (+ (isolated-pawn cb a b) (doubled-pawn cb a b) (half-open cb a b) (backward-pawn cb a b) (pawn-position cb a b gameplay) (passed-pawn cb a b gameplay)))

(define (knight-list cb a b gameplay)
  (+ (knight-centre cb a b) (knight-king cb a b gameplay)))


;isolated pawn
(define (isolated-pawn cb a b);assumes that the location given definitely has a pawn in the first place
  (define ans (vector-ref isolated-pawn-val b))
  (define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i a) (+ j b))
          (if (and (not (and (= i 0) (= j 0))) (equal? #\p (my-board-ref cb (+ i a) (+ j b)))) 0
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

;doubled pawn
(define (doubled-pawn cb a b)
  (define ans doubled-pawn-val)
  (define (outer i)
    (if(> i 6) 0
       (if(equal? #\p (my-board-ref cb i b)) ans
          (outer (+ i 1)))))
  (outer (+ a 1)))

;half open file
(define (half-open cb a b)
  (define (outer i)
    (if(> i 6) half-open-val
       (if(equal? #\P (my-board-ref cb i b)) 0
          (outer (+ i 1)))))
  (outer (+ a 1)))

;backward pawn
(define (backward-pawn cb a b)
  (cond[(and (isValid (+ a 1) (- b 1)) (equal? #\p (my-board-ref cb (+ a 1) (- b 1)))) backward-pawn-val]
       [(and (isValid (+ a 1) (+ b 1)) (equal? #\p (my-board-ref cb (+ a 1) (+ b 1)))) backward-pawn-val]
       [else 0]))

;pawn position
(define (pawn-position cb a b gameplay)
  (let ((c (abs (- 1 a))))
    (cond[(= -1 gameplay) (* c pawn-position-val)];(* c 10 (- 7 (abs (- (/ 7 2) b))) (if(> a 3) (/ 1 5) (/ 3 2))
         [(= 0 gameplay) (* c 5 pawn-position-val)]
         [else (* c 10 pawn-position-val)])))

;passed pawn
(define (passed-pawn cb a b gameplay)
(define (outer i c)
    (if(> i 6)(* c passed-pawn-val)
       (cond[(and (isValid i (- b 1)) (equal? #\P (my-board-ref cb i (- b 1)))) 0]
            [(and (isValid i (+ b 1)) (equal? #\P (my-board-ref cb i (+ b 1)))) 0]
            [else (outer (+ i 1) c)])))
  (let((c (cond [(= gameplay -1) 1]
                [(= gameplay 0) 3]
                [(and (= gameplay 0) (= a 0)) 15]
                [else 7])))
  (outer (+ a 1) c)))


;knight's proximity to the centre

(define (knight-centre cb a b)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))])
    (* (+ c d) knight-centre-val)))

;knight's proximity to the opposite king

(define (knight-king cb a b gameplay)
  (let*([c (find-white-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        [d (cond [(= gameplay -1) 1]
                 [(= gameplay 0) 2]
                 [else 3])]
        )
    (* d knight-king-val (+ p q))))

;find black king
(define (find-white-king cb)
  (define (outer i j)
  (if(> i 7)(cons -1 -1)
     (inner i j)))
  (define (inner i j)
    (if(> j 7)(outer (+ i 1) 0)
       (if (equal? #\K (my-board-ref cb i j)) (cons i j)
           (inner i (+ j 1)))))
  (outer 0 0))
  
(define (bishop-list cb a b gameplay from-this)
  (+ (bishop-bonus cb) (bishop-centre cb a b) (bishop-squares-attacking cb from-this a b) (bishop-threatening cb from-this a b)))

;bishop bonus for less material on the board
(define (bishop-bonus cb)
  (let ([a (/ (- 9280 (total-material cb)) 9280)])
    (ceiling (* a bishop-bonus-val))))


;bishop bonus for attacking more squares ##
(define (bishop-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count bishop-squares-attacking-val)))

;bishop bonus for threatening special pieces ##
(define (bishop-threatening cb from-moves a b)
  (define (find l1 ans)
    (if(null? l1) ans
       (let* ([a (car l1)]
              [b (my-board-ref cb (car a) (cdr a))])
         (if(or (equal? b #\r) (equal? b #\q) (equal? b #\k)) (find (cdr l1) (+ ans bishop-threatening-val))
            (find (cdr l1) ans)))))
  (let ([l1 (my-board-ref from-moves a b)])
    (find l1 0)))

;the bishop's proximity to the centre
(define (bishop-centre cb a b)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))])
    (* (+ c d) bishop-centre-val)))

;the bishop attacking squares adjacent to the enemy king ##
(define (bishop-adjacent-threatening cb to-black-moves a b)
  (define c (find-white-king cb))
  (define ans 0)
  (define (isAttacking? a b c)
    (contains? (my-board-ref to-black-moves a b) c))
  (define (contains? l1 elem)
    (cond[(null? l1) #f]
         [(equal? elem (car l1) #t)]
         [else (contains? (cdr l1) elem)]))
(define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i (car c)) (+ j (cdr c)))
          (if (and (not (and (= i 0) (= j 0))) (isAttacking? (car c) (cdr c) (cons a b))) (begin (set! ans (+ ans (bishop-adjacent-threatening-val)))
                                                                                                 (inner i (+ j 1)))
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

(define (rook-list cb a b gameplay from-this)
  (+ (rook-self-pawn-absent cb a b) (rook-opp-pawn-absent cb a b) (rook-king cb a b) (rook-squares-attacking cb from-this a b)))

;bonus for rook for attacking more number of squares##
(define (rook-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count rook-squares-attacking-val)))

;rook self-pawn-absent
(define (rook-self-pawn-absent cb a b)
  (define (outer i)
    (if(> i 6) rook-self-pawn-absent-val
       (if(equal? (my-board-ref cb i b) #\p)0
          (outer (+ i 1)))))
  (outer 1))

;rook opp-pawn-absent
(define (rook-opp-pawn-absent cb a b)
  (define (outer i)
    (if(> i 6) rook-opp-pawn-absent-val
       (if(equal? (my-board-ref cb i b) #\P)0
          (outer (+ i 1)))))
  (outer 1))

;rook opp-king-proximity
(define (rook-king cb a b)
  (let*([c (find-white-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        )
    (* rook-king-val (+ p q))))

(define (queen-list cb a b gameplay from-this)
  (+ (queen-king cb a b) (queen-squares-attacking cb from-this a b)))

;bonus for queen attacking more number of squares##
(define (queen-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count queen-squares-attacking-val)))

;bonus for queen's proximity to the opposite king
(define (queen-king cb a b)
  (let*([c (find-white-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        )
    (* queen-king-val (+ p q))))

(define (king-list cb a b gameplay)
  (+ (king-centre cb a b gameplay) (open-king cb a b gameplay) (isolated-king cb a b)))

;bonus for king's proximity to the centre##
(define (king-centre cb a b gameplay)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))]
       [factor
        (cond[(= -1 gameplay) -2]
             [(= 0 gameplay) 1]
             [(whitehasonlypawns? cb) 0]
             [else 3] 
        )])
    (* factor (+ c d) king-centre-val)))


(define (whitehasonlypawns? cb)
  (define (outer i j)
    (if(> i 7) #t
       (inner i j)))
  (define (inner i j)
    (if(> j 7)(outer (+ i 1) 0)
       (let ((a (my-board-ref cb i j)))
         (if(or (equal? a #\R) (equal? a #\B) (equal? a #\N) (equal? a #\Q))#f
            (inner i (+ j 1))))))
  (outer 0 0))
      
;penalty for presence on a half-open file or open file
(define (open-king cb a b gameplay)
  (define (open? cb b)
    (define (white-pawn?)
      (define (outer i)
        (if(< i 1) #f
           (if(equal? (my-board-ref cb i b) #\P)#t
              (outer (- i 1)))))
      (outer 6))
    (define (black-pawn?)
      (define (outer i)        
        (if(< i 1) #f
         (if(equal? (my-board-ref cb i b) #\p)#t
            (outer (- i 1)))))
      (outer 6))
    (cond[(and (white-pawn?) (black-pawn?)) 1]
         [(and (not (white-pawn?)) (not (black-pawn?))) 2]
         [else 0]
         ))
  (let* ([var (open? cb b)]        
         [factor (cond [(= gameplay -1) 3]
                       [(= gameplay 0) 1]
                       [else 0])]
         [factor2 (cond [(= var 0) 1]
                        [(= var 1) 0]
                        [else (/ 5 2)])])
    (* factor factor2 open-king-val)))

;penalty for no adjacent pawns
(define (isolated-king cb a b)
  (define ans isolated-king-val)
  (define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i a) (+ j b))
          (if (and (not (and (= i 0) (= j 0))) (equal? #\p (my-board-ref cb (+ i a) (+ j b)))) 0
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

;penalty for the number of checks on the king##
(define (check-king cb a b to-moves-black)
  (let(( a (length (my-board-ref to-moves-black a b))))
    (* a (check-king-penalty))))
  
  (define final-ans 0)
  (define gameplay (gameplay-eval cb))
  (define (support)
    (define ans 0)
    (define (outer i j)
      (if(> i 7) ans
         (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ((a (my-board-ref cb i j)))
                     (cond[(number? a)(inner i (+ j 1))]
                          [(equal? #\p a)(begin
                                           (set! ans (+ ans pawn-val (pawn-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(equal? #\b a)(begin
                                           (set! ans (+ ans bishop-val (bishop-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\n a)(begin
                                           (set! ans (+ ans knight-val (knight-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(equal? #\r a)(begin
                                           (set! ans (+ ans rook-val (rook-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\q a)(begin
                                           (set! ans (+ ans queen-val (queen-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\k a)(begin
                                           (set! ans (+ ans king-val (king-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(number? a)(inner i (+ j 1))]
                          [(equal? #\P a)(begin
                                           (set! ans (+ ans (- pawn-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\B a)(begin
                                           (set! ans (+ ans (- bishop-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\N a)(begin
                                           (set! ans (+ ans (- knight-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\R a)(begin
                                           (set! ans (+ ans (- rook-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\Q a)(begin
                                           (set! ans (+ ans (- queen-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\K a)(begin
                                           (set! ans (+ ans (- king-val)))
                                           (inner i (+ j 1)))]
                          [else (inner i (+ j 1))]))]))
    (outer 0 0))  
  (support))
      

(define (gameplay-eval cb)
  (let ((a (total-material cb)))
    (cond[(> a 6500) -1]
         [(> a 3500) 0]
         [else 1])))
         
(define cb
  (vector (vector  #\r 0 0 0 #\k 0 0 0)
          (vector  #\p #\p #\p #\p #\p #\p #\p #\p)
          (vector  0 0 0 0 0 0 0 0)
          (vector  0 0 0 0 0 0 0 0)
          (vector  0 0 0 0 0 0 0 0)
          (vector  0 0 0 0 0 0 0 0)
          (vector  0 #\P #\P #\P #\P #\P #\P #\P)
          (vector  #\R #\N #\B #\Q #\K #\B #\N #\R)))

(define to-this-one
  (vector (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())
          (vector '() '() '() '() '() '() '() '())))          

(define (my-board-set! cb x y val)
  (vector-set! (vector-ref cb x) y val))

(define (my-board-ref cb x y)
  (vector-ref (vector-ref cb x) y))



;########################################################
;evaluation function exclusively for white to move





(define (evaluate cb from-this to-move wa wb wc ba bb bc)
  (define final-ans 0)
  (define gameplay (gameplay-eval cb))
  (define (support)
    (define ans 0)
    (define (outer i j)
      (if(> i 7) ans
         (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ((a (my-board-ref cb i j)))
                     (cond[(number? a)(inner i (+ j 1))]
                          [(equal? #\P a)(begin
                                           (set! ans (+ ans pawn-val (pawn-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(equal? #\B a)(begin
                                           (set! ans (+ ans bishop-val (bishop-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\N a)(begin
                                           (set! ans (+ ans knight-val (knight-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(equal? #\R a)(begin
                                           (set! ans (+ ans rook-val (rook-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\Q a)(begin
                                           (set! ans (+ ans queen-val (queen-list cb i j gameplay from-this)))
                                           (inner i (+ j 1)))]
                          [(equal? #\K a)(begin
                                           (set! ans (+ ans king-val (king-list cb i j gameplay)))
                                           (inner i (+ j 1)))]
                          [(number? a)(inner i (+ j 1))]
                          [(equal? #\p a)(begin
                                           (set! ans (+ ans (- pawn-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\b a)(begin
                                           (set! ans (+ ans (- bishop-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\n a)(begin
                                           (set! ans (+ ans (- knight-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\r a)(begin
                                           (set! ans (+ ans (- rook-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\q a)(begin
                                           (set! ans (+ ans (- queen-val)))
                                           (inner i (+ j 1)))]
                          [(equal? #\k a)(begin
                                           (set! ans (+ ans (- king-val)))
                                           (inner i (+ j 1)))]
                          [else (inner i (+ j 1))]))]))
    (outer 0 0))  
  (define (invert cb)
    (define (outer i j)
      (if(> i 3) (void)
         (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ([temp (my-board-ref cb i j)]
                         [temp4 (my-board-ref from-this i j)])
                     (begin
                       (my-board-set! cb i j (my-board-ref cb (- 7 i) j))
                       (my-board-set! cb (- 7 i) j temp)
                       (my-board-set! from-this i j (my-board-ref from-this (- 7 i) j))
                       (my-board-set! from-this (- 7 i) j temp4)
                       (inner i (+ j 1))))
                       ]))
    (define (switch)
      (define (invert-char ch)
        (let ((a (char->integer ch)))
          (cond[(<= a 90)(integer->char (+ a 32))]
               [else (integer->char (- a 32))])))
      (define (outer i j)
        (if(> i 7) (void)
           (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ((a (my-board-ref cb i j)))
                     (if(number? a) (inner i (+ j 1))
                        (begin
                          (my-board-set! cb i j (invert-char a))
                          (invert-each from-this)
                          (inner i (+ j 1)))))]))
      (outer 0 0))
    (begin 
      (outer 0 0)
      (switch)
      ))
  (if (= 1 to-move)(support)
      (evaluate-black cb from-this 0 0 0 0 0 0)))


(define (total-material cb)
   (define (support)
    (define ans 0)
    (define (outer i j)
      (if(> i 7) ans
         (inner i j)))
      (define (inner i j)
        (cond[(> j 7) (outer (+ i 1) 0)]
             [else (let ((a (my-board-ref cb i j)))
                     (cond[(number? a)(inner i (+ j 1))]
                          [(equal? #\P a)(begin
                                           (set! ans (+ ans pawn-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\B a)(begin
                                           (set! ans (+ ans bishop-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\N a)(begin
                                           (set! ans (+ ans knight-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\R a)(begin
                                           (set! ans (+ ans rook-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\Q a)(begin
                                           (set! ans (+ ans queen-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\K a)(begin
                                           (set! ans (+ ans 500))
                                           (inner i (+ j 1)))]
                          [(number? a)(inner i (+ j 1))]
                          [(equal? #\p a)(begin
                                           (set! ans (+ ans pawn-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\b a)(begin
                                           (set! ans (+ ans bishop-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\n a)(begin
                                           (set! ans (+ ans knight-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\r a)(begin
                                           (set! ans (+ ans rook-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\q a)(begin
                                           (set! ans (+ ans queen-val))
                                           (inner i (+ j 1)))]
                          [(equal? #\k a)(begin
                                           (set! ans (+ ans 500))
                                           (inner i (+ j 1)))]
                          [else (inner i (+ j 1))]))]))
    (outer 0 0))
  (support))

(define (isValid a b)
  (cond[(< a 0) #f]
       [(> a 7) #f]
       [(< b 0) #f]
       [(> b 7) #f]
       [else #t]))

(define (invert-each vec)
  (define (outer i j)
    (if(> i 7)(void)
       (inner i j)))
  (define (inner i j)
    (if(> j 7)(outer (+ i 1) 0)
       (let ([l1 (my-board-ref vec i j)])
         (begin
           (map (λ(x)(cons (- 7 (car x)) (cdr x))) l1)
           (inner i (+ j 1))))))
  (outer 0 0))

(define (pawn-list cb a b gameplay)
  (+ (isolated-pawn cb a b) (doubled-pawn cb a b) (half-open cb a b) (backward-pawn cb a b) (pawn-position cb a b gameplay) (passed-pawn cb a b gameplay)))


(define (knight-list cb a b gameplay)
  (+ (knight-centre cb a b) (knight-king cb a b gameplay)))


;isolated pawn
(define (isolated-pawn cb a b);assumes that the location given definitely has a pawn in the first place
  (define ans (vector-ref isolated-pawn-val b))
  (define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i a) (+ j b))
          (if (and (not (and (= i 0) (= j 0))) (equal? #\P (my-board-ref cb (+ i a) (+ j b)))) 0
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

;doubled pawn
(define (doubled-pawn cb a b)
  (define ans doubled-pawn-val)
  (define (outer i)
    (if(> i 6) 0
       (if(equal? #\P (my-board-ref cb i b)) ans
          (outer (+ i 1)))))
  (outer (+ a 1)))

;half open file
(define (half-open cb a b)
  (define (outer i)
    (if(> i 6) half-open-val
       (if(equal? #\p (my-board-ref cb i b)) 0
          (outer (+ i 1)))))
  (outer (+ a 1)))

;backward pawn
(define (backward-pawn cb a b)
  (cond[(and (isValid (+ a 1) (- b 1)) (equal? #\P (my-board-ref cb (+ a 1) (- b 1)))) backward-pawn-val]
       [(and (isValid (+ a 1) (+ b 1)) (equal? #\P (my-board-ref cb (+ a 1) (+ b 1)))) backward-pawn-val]
       [else 0]))

;pawn position
(define (pawn-position cb a b gameplay)
  (let ((c (abs (- 1 a))))
    (cond[(= -1 gameplay) (* c pawn-position-val)]
         [(= 0 gameplay) (* c 5 pawn-position-val)]
         [else (* c 10 pawn-position-val)])))

;passed pawn
(define (passed-pawn cb a b gameplay)
(define (outer i c)
    (if(> i 6)(* c passed-pawn-val)
       (cond[(and (isValid i (- b 1)) (equal? #\p (my-board-ref cb i (- b 1)))) 0]
            [(and (isValid i (+ b 1)) (equal? #\p (my-board-ref cb i (+ b 1)))) 0]
            [else (outer (+ i 1) c)])))
  (let((c (cond [(= gameplay -1) 1]
                [(= gameplay 0) 3]
                [(and (= gameplay 0) (= a 0)) 15]
                [else 7])))
  (outer (+ a 1) c)))


;knight's proximity to the centre

(define (knight-centre cb a b)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))])
    (* (+ c d) knight-centre-val)))

;knight's proximity to the opposite king

(define (knight-king cb a b gameplay)
  (let*([c (find-black-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        [d (cond [(= gameplay -1) 1]
                 [(= gameplay 0) 2]
                 [else 3])]
        )
    (* d knight-king-val (+ p q))))

;find black king
(define (find-black-king cb)
  (define (outer i j)
  (if(> i 7)(cons -1 -1)
     (inner i j)))
  (define (inner i j)
    (if(> j 7)(outer (+ i 1) 0)
       (if (equal? #\k (my-board-ref cb i j)) (cons i j)
           (inner i (+ j 1)))))
  (outer 0 0))

(define (bishop-list cb a b gameplay from-this)
  (+ (bishop-bonus cb) (bishop-centre cb a b) (bishop-threatening cb from-this a b) (bishop-squares-attacking cb from-this a b)))
  
;bishop bonus for less material on the board
(define (bishop-bonus cb)
  (let ([a (/ (- 9280 (total-material cb)) 9280)])
    (ceiling (* a bishop-bonus-val))))


;bishop bonus for attacking more squares ##
(define (bishop-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count bishop-squares-attacking-val)))

;bishop bonus for threatening special pieces ##
(define (bishop-threatening cb from-moves a b)
  (define (find l1 ans)
    (if(null? l1) ans
       (let* ([a (car l1)]
              [b (my-board-ref cb (car a) (cdr a))])
         (if(or (equal? b #\r) (equal? b #\q) (equal? b #\k)) (find (cdr l1) (+ ans bishop-threatening-val))
            (find (cdr l1) ans)))))
  (let ([l1 (my-board-ref from-moves a b)])
    (find l1 0)))

;the bishop's proximity to the centre
(define (bishop-centre cb a b)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))])
    (* (+ c d) bishop-centre-val)))

;the bishop attacking squares adjacent to the enemy king ##
(define (bishop-adjacent-threatening cb to-white-moves a b)
  (define c (find-black-king cb))
  (define ans 0)
  (define (isAttacking? a b c)
    (contains? (my-board-ref to-white-moves a b) c))
  (define (contains? l1 elem)
    (cond[(null? l1) #f]
         [(equal? elem (car l1) #t)]
         [else (contains? (cdr l1) elem)]))
(define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i (car c)) (+ j (cdr c)))
          (if (and (not (and (= i 0) (= j 0))) (isAttacking? (car c) (cdr c) (cons a b))) (begin (set! ans (+ ans (bishop-adjacent-threatening-val)))
                                                                                                 (inner i (+ j 1)))
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

(define (rook-list cb a b gameplay from-this)
  (+ (rook-self-pawn-absent cb a b) (rook-opp-pawn-absent cb a b) (rook-king cb a b) (rook-squares-attacking cb from-this a b)))

;bonus for rook for attacking more number of squares##
(define (rook-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count rook-squares-attacking-val)))

;rook self-pawn-absent
(define (rook-self-pawn-absent cb a b)
  (define (outer i)
    (if(> i 6) rook-self-pawn-absent-val
       (if(equal? (my-board-ref cb i b) #\P)0
          (outer (+ i 1)))))
  (outer 1))

;rook opp-pawn-absent
(define (rook-opp-pawn-absent cb a b)
  (define (outer i)
    (if(> i 6) rook-opp-pawn-absent-val
       (if(equal? (my-board-ref cb i b) #\p)0
          (outer (+ i 1)))))
  (outer 1))

;rook opp-king-proximity
(define (rook-king cb a b)
  (let*([c (find-black-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        )
    (* rook-king-val (+ p q))))

(define (queen-list cb a b gameplay from-this)
  (+ (queen-king cb a b) (queen-squares-attacking cb from-this a b)))

;bonus for queen attacking more number of squares##
(define (queen-squares-attacking cb from-moves a b)
  (let ([count (length (my-board-ref from-moves a b))])
    (* count queen-squares-attacking-val)))

;bonus for queen's proximity to the opposite king
(define (queen-king cb a b)
  (let*([c (find-black-king cb)]
        [p (- 14 (abs (- a (car c))))]
        [q (- 14 (abs (- b (cdr c))))]
        )
    (* queen-king-val (+ p q))))

(define (king-list cb a b gameplay)
  (+ (king-centre cb a b gameplay) (open-king cb a b gameplay) (isolated-king cb a b)))

;bonus for king's proximity to the centre
(define (king-centre cb a b gameplay)
  (let([c (- (/ 7 2) (abs (- (/ 7 2) a)))]
       [d (- (/ 7 2) (abs (- (/ 7 2) b)))]
       [factor
        (cond[(= -1 gameplay) -2]
             [(= 0 gameplay) 1]
             [(blackhasonlypawns? cb) 0]
             [else 3] 
        )])
    (* factor (+ c d) king-centre-val)))


(define (blackhasonlypawns? cb)
  (define (outer i j)
    (if(> i 7) #t
       (inner i j)))
  (define (inner i j)
    (if(> j 7)(outer (+ i 1) 0)
       (let ((a (my-board-ref cb i j)))
         (if(or (equal? a #\r) (equal? a #\b) (equal? a #\n) (equal? a #\q))#f
            (inner i (+ j 1))))))
  (outer 0 0))
      
;penalty for presence on a half-open file or open file
(define (open-king cb a b gameplay)
  (define (open? cb b)
    (define (white-pawn?)
      (define (outer i)
        (if(< i 1) #f
           (if(equal? (my-board-ref cb i b) #\P)#t
              (outer (- i 1)))))
      (outer 6))
    (define (black-pawn?)
      (define (outer i)
        
        (if(< i 1) #f
         (if(equal? (my-board-ref cb i b) #\p)#t
            (outer (- i 1)))))
      (outer 6))
    (cond[(and (white-pawn?) (black-pawn?)) 1]
         [(and (not (white-pawn?)) (not (black-pawn?))) 2]
         [else 0]
         ))
  (let* ([var (open? cb b)]        
         [factor (cond [(= gameplay -1) 3]
                       [(= gameplay 0) 1]
                       [else 0])]
         [factor2 (cond [(= var 0) 1]
                        [(= var 1) 0]
                        [else (/ 5 2)])])
    (* factor factor2 open-king-val)))

;penalty for no adjacent pawns
(define (isolated-king cb a b)
  (define ans isolated-king-val)
  (define (outer i j)
    (if(> i 1) ans
       (inner i j)))
  (define (inner i j)
    (if(> j 1)(outer (+ i 1) -1)
       (if(isValid (+ i a) (+ j b))
          (if (and (not (and (= i 0) (= j 0))) (equal? #\P (my-board-ref cb (+ i a) (+ j b)))) 0
              (inner i (+ j 1)))
          (inner i (+ j 1)))))
  (outer -1 -1))

;penalty for the number of checks on the king##
(define (check-king cb a b to-moves-black)
  (let(( a (length (my-board-ref to-moves-black a b))))
    (* a (check-king-penalty))))
    
;General Checks
;bonus for castling and cannot castle##
(define (castling-bonus wa wb wc ba bb bc to-move)
  (if(= to-move 1)
     (cond[wa castling-bonus-val]
          [(not (or wb wc)) cannot-castle-val]
          [else 0])
     (cond[ba castling-bonus-val]
          [(not (or bb bc)) (cannot-castle-val)]
          [else 0])))
