#lang racket
;;includes
(include "static-evaluation-function.scm")
(include "AI.scm")
(include "Valid-moves.scm")
(include "parser.scm")
;;variables and board
(define ks-c-b #t)
(define ks-c-w #t)
(define qs-c-b #t)
(define qs-c-w #t)
(define cas-w #f)
(define cas-b #f)
(define enp #f)
(define (var-set! p val)
  (cond [(= p 1) (set! ks-c-b val)]
        [(= p 2) (set! ks-c-w val)]
        [(= p 3) (set! qs-c-b val)]
        [(= p 4) (set! qs-c-w val)]
        [(= p 5) (set! cas-b val)]
        [(= p 6) (set! cas-w val)]
        [(= p 7) (set! enp val)]))
(define (var-ref p)
  (cond [(= p 1) ks-c-b]
        [(= p 2) ks-c-w]
        [(= p 3) qs-c-b]
        [(= p 4) qs-c-w]
        [(= p 5) cas-b]
        [(= p 6) cas-w]
        [(= p 7) enp]))
(define state-force #f)
(define iam 'b)
(define moves '())
(define this-move '())
;(define from-here (build-vector 8 (lambda(x) (make-vector 8 '()))))
(define b
  (vector (vector  #\r #\n #\b #\q #\k #\b #\n #\r)
          (make-vector 8 #\p)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 #\P)
          (vector  #\R #\N #\B #\Q #\K #\B #\N #\R)))
(define standard-board
  (vector (vector  #\r #\n #\b #\q #\k #\b #\n #\r)
          (make-vector 8 #\p)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 0)
          (make-vector 8 #\P)
          (vector  #\R #\N #\B #\Q #\K #\B #\N #\R)))

(define (2dvec-ref v i j)
  (vector-ref (vector-ref v i) j))
(define (2dvec-set! v i j val)
  (vector-set! (vector-ref v i) j val))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;board functions
(define (board-ref x y)
  (vector-ref (vector-ref b x) y))
(define (board-ref-str s)
  (board-ref (r-pos (string-ref s 1)) (c-pos (string-ref s 0))))
(define (board-set! x y val)
  (vector-set! (vector-ref b x) y val))
(define (bd)
  (define (vec-reverse v)
    (vec-foldr (lambda(fst i acc) (vector-append acc (vector fst))) (vector ) v))
  (define (show-all v)
    (vec-foldr (lambda (fst i acc) (display fst) (display " ")) 'done (vec-reverse v)))
  (define one-eight (build-vector 8 (lambda(x) (+ x 1))))
  (define a-h (build-vector 8 (lambda(x) (integer->char (+ 97 x)))))
  (define separator (make-vector 8 #\-))
  (define (print i)
    (if (= i 8) (display "")
        (begin (display (integer->char (- 56 i))) (display " |")
               (show-all (vector-ref b i))
               (newline)
               (print (+ i 1))
               )))
  (print 0)
  (display "   ") (show-all separator) (newline)
  (display "   ") (show-all a-h) (newline))
(define (vec-foldr f id v)
  (define (h i)
    (if (= i 8) id
        (f (vector-ref v i) i (h (+ i 1)))))
  (h 0))
;;;;forsythe edward notation code
(define (set-rank r str)
  (define (h i str)
    (if (= i 8) void 
        (begin (let* ([c (string-ref str 0)]
                      [int (- (char->integer c) 48)])
                 (if (char-range c 49 56)
                     (begin 
                       (board-set-0 r i int)
                       (h (+ i int) (substring str 1)))
                     (begin
                       (board-set! r i c)
                       (h (+ i 1) (substring str 1))))))))
  (h 0 str))
(define (board-set-0 i j p)
  (if (= p 0) void
      (begin (board-set! i j 0)
             (board-set-0 i (+ j 1) (- p 1)))))
(define non-backslash-space-p
  (pred-p (lambda(c) (not (or (equal? c #\/) (equal? c #\ ))))))
(define till-backslash-space-p
  (lambda(str) ((one-or-more non-backslash-space-p combine-cs) str)))
(define (epsilon-p-id id) (lambda(str) (cons id str)))
(define (zero-or-more-id p combine id)
  (lambda(str) ((alt (seq p (zero-or-more-id p combine id) combine) (epsilon-p-id id)) str)))
(define (board-p  str)
  (let ([list_ ((zero-or-more-id (seq till-backslash-space-p (alt (char-p #\/) epsilon-p) combine-x) combine-list '()) str)])
    (define (h i l)
      (if (null? l) void
          (begin 
            (set-rank i (car l))
            (h (+ i 1) (cdr l)))))
    (h 0 (car list_))
    list_))
(define combine-list (lambda (x y) (cons x y)))
(define combine-x (lambda(x y) x))
(define combine-y (lambda(x y) y))
(define combine-null (lambda(x y) void))
(define colour-p 
  (lambda(str) 
    (let ([parse ((seq whitespace-p single-alphabet-p combine-y) str)])
      (if (equal? (car parse) #\w) (set! iam 'w) (set! iam 'b))
      parse)))

(define castling-p
  (lambda(str) (begin (set! ks-c-w #f) (set! qs-c-w #f) (set! ks-c-b #f) (set! qs-c-b #f)
                      (let ([parse (identifier-helper str)])
                        (if (equal? parse 'fail) 
                            ((char-p #\-) str)
                            (begin (setboard-c-set (car parse))
                                   parse))))))
(define (setboard-c-set str)
  (define (h c)
    (cond [(equal? c #\K) (set! ks-c-w #t)]
          [(equal? c #\k) (set! ks-c-b #t)]
          [(equal? c #\Q) (set! qs-c-w #t)]
          [(equal? c #\q) (set! qs-c-b #t)]))
  (foldr (lambda(fst acc) (h fst)) void (string->list str)))
(define enp-p (lambda(str) (let ([p (identifier-helper str)])
                             (if (equal? p 'fail)
                                 ((char-p #\-) str)
                                 (begin (var-set 7 (cons (r-pos (string-ref str 1)) (c-pos (string-ref str 0))))
                                        (cons "" (cdr p)))))))

(define fen-p
  (lambda(str) (begin ((seq board-p (seq colour-p (seq castling-p enp-p combine-null) combine-null) combine-null) str) )))


;board moves
(define (undo-move)
  (foldr (lambda(f a) (f)) void (car moves))
  (set! moves (cdr moves)))
(define (board-move i)
  
  (cond [(ispawnpro i)
         
         (let ([s (if (= (string-length i) 4) (string-append i (myqueen)) i)])
           (board-move-str (substring s 0 4))
           (let ([v (board-ref (r-pos (string-ref s 3)) (c-pos (string-ref s 2)))])
             
             (set! moves (cons (append 
                                (car moves) 
                                (list (lambda() (begin
                                                  (board-set! (r-pos (string-ref s 3)) (c-pos (string-ref s 2)) v)
                                                  ;(set-from-dependencies (r-pos (string-ref s 3)) (c-pos (string-ref s 2)))
                                                  ))))
                               (cdr moves)))
             
             )
           (board-set! (r-pos (string-ref s 3)) (c-pos (string-ref s 2)) 
                       (get-pawn (board-ref (r-pos (string-ref s 3)) (c-pos (string-ref s 2))) (string-ref s 4))))
         ]
        [(iscastling i)
         (if (cant-castle i)
             (tell-x (string-append "Illegal move : " i))
             (begin (board-move-str i)
                    (board-move-str (rooke-move-c (getp-c i)))
                    (if (or (= (getp-c i) 1) (= (getp-c i) 3))
                        (var-set 5 #t)
                        (var-set 6 #t))
                    (set! moves (cons (append (car (cdr moves)) (car moves) ) (cdr (cdr moves))))))]
        [(isenp i)
         (let ([int-loc (enp-int-move i)])
           (board-move-str (string-append (substring i 0 2) int-loc))
           (board-move-str (string-append int-loc (substring i 2 4)))
           (set! moves (cons (append (cadr moves) (car moves) ) (cddr moves))))]
        [else (board-move-str i)]))
(define (board-move-str str)
  (board-move-pos (r-pos (string-ref str 1))
                  (c-pos (string-ref str 0))
                  (r-pos (string-ref str 3))
                  (c-pos (string-ref str 2))
                  str))
(define (board-move-pos p1r p1c p2r p2c str)
  (begin 
    (castling-set! p1r p1c)
    (castling-set! p2r p2c)
    (enp-activate! p1r p1c p2r p2c)
    (set! this-move (let ([v1 (board-ref p1r p1c)]
                          [v2 (board-ref p2r p2c)])
                      (cons (lambda () (begin
                                         (board-set! p1r p1c v1) 
                                         (board-set! p2r p2c v2)
                                         ;(set-from-dependencies p1r p1c)
                                         ;(set-from-dependencies p2r p2c)
                                         ))
                            this-move)))
    (board-set! p2r p2c (board-ref p1r p1c))
    (board-set! p1r p1c 0)
    ;(set-from-dependencies p1r p1c)
    ;(set-from-dependencies p2r p2c)
    (set! moves (cons this-move moves))
    ;(display (car moves))
    
    (set! this-move '())
    ))

;;;;;;;;;;;;;;;;;action command;;;;;;;taking input from user and processing it
(define (action! s)
  (let* ([ident (identifier-helper s)]
         [i (if (not (equal? 'fail ident)) (car ident) 'fail)])
    (cond [(ispos i)
           
           (board-move i) 
           
           (check-for-check iam)
           
           (cond [(not state-force) (make-my-move)])]
          
          [(ispawnpro i)
           
           (board-move i)
           
           (check-for-check iam)
           (cond [(not state-force) (make-my-move)])
           ]
          [(equal? i "xboard") void ]
          [(equal? i "navin") (set! a 1)]
          [(equal? i "black") (set! iam 'b)]
          [(equal? i "white") (set! iam 'w)]
          [(equal? i "new") (set! b standard-board)]
          [(equal? i "bd") (bd) (newline)]
          [(equal? i "protover") (tell-x "feature sigint=0 sigterm=0 setboard=1 done=1")]
          [(equal? i "force") (set! state-force #t)]
          [(equal? i "go") (set! state-force #f) (make-my-move)]
          [(equal? i "remove") (undo-move) (undo-move)]
          [(equal? i "setboard") (begin (fen-p (cdr (whitespace-p (cdr ident)))) (set-from-here!))]
;          [(equal? i "from") (let ([s (car ((seq whitespace-p identifier-helper combine-y) (cdr ident)))])
;                               (display (foldr (lambda(a b) (cons (make-SAN a) b)) '() 
;                                               (2dvec-ref from-here 
;                                                          (r-pos (string-ref s 1))
;                                                          (c-pos (string-ref s 0)))))
;                               (newline))]
          )))
;;;;;;;;;;;;;;;helper functions for board and action!
(define (enp-activate! p1r p1c p2r p2c)
  (if (and (ispawn (board-ref p1r p1c))
           (= (- p2r p1r) 2))
      (let ([p (if (isblack (board-ref p1r p1c)) 7 8)])
        (var-set p (cons p2r p2c)))
      (var-set 7 #f)))
(define (castling-set! p2r p2c)
  (cond [(= p2c 4) (if (= p2r 0) (begin (var-set 1 #f) (var-set 3 #f))
                       (if (= p2r 7) (begin  (var-set 2 #f) (var-set 4 #f)) void))]
        [(= p2c 0) (if (= p2r 0) (var-set 3 #f) 
                       (if (= p2r 7)  (var-set 4 #f) void))]
        [(= p2c 7) (if (= p2r 0) (var-set 1 #f)
                       (if (= p2r 7) (var-set 2 #f) void))]))
(define (set-from-dependencies x y)
  ;(update-from (all-dependencies x y #t))
  ;(2dvec-set! from-here x y (valid x y)))
  void)

(define (ispawnpro s)
  (and (string? s)
       (>= (string-length s) 4)
       (ispos (substring s 0 4)) 
       (ispawn (board-ref (r-pos (string-ref s 1)) (c-pos (string-ref s 0))))
       (or (= (r-pos (string-ref s 3)) 7) (= (r-pos (string-ref s 3)) 0))))
(define (get-pawn p c)
  (if (char-range p 65 92) (integer->char ( - (char->integer c) 32))
      c)) 
(define (isenp i)
  (and (ispos i)
       (ispawn (board-ref-str (substring i 0 2)))
       (iszero (board-ref-str (substring i 2 4)))
       (= 1 (abs (- (c-pos (string-ref i 0)) (c-pos (string-ref i 2)))))
       (= 1 (abs (- (r-pos (string-ref i 1)) (r-pos (string-ref i 3)))))
       ))
(define (enp-int-move i)
  (string (string-ref i 2)
          (string-ref i 1)))
(define (iscastling s) 
  (and (ispos s) 
       (isking (board-ref-str (substring s 0 2)))
       (= 2 (abs (- (c-pos (string-ref s 0)) (c-pos (string-ref s 2)))))))
(define (adjacent p1 p2)
  (and (= (car p1) (car p2)) (= 1 (abs (- (cdr p1) (cdr p2))))))
(define (isrank2 x c)
  (if (isblack c) (if (= x 1) #t #f)
      (if (= x 6) #t #f)))
(define (ispos s)
  (and (string? s) 
       (equal? (string-length s) 4) 
       (row? (string-ref s 0)) (col? (string-ref s 1)) 
       (row? (string-ref s 2)) (col? (string-ref s 3))))


(define (update-from l) ;(foldr (lambda(fst acc) 
  ;         (2dvec-set! from-here (car fst) (cdr fst) (valid (car fst) (cdr fst))))) void l))
  void)
(define (set-from-here!) void)
;(vec-foldr 
;                          (lambda(fst i acc) 
;                            (vec-foldr 
;                             (lambda(fst j acc) 
;                               (2dvec-set! from-here i j (valid i j))) void fst)) void b))



;;;;;;;;;;;;;;;;;;;;;;;general functions used throughout
(define (var-set v val)
  (if (equal? (var-ref v) val) void
      (begin (set! this-move (cons (let ([val-b4-change (var-ref v)]) (lambda() (var-set! v val-b4-change))) this-move))
             (var-set! v val))))


(define (all-dependencies x y need-same-colour)
  (let ([t need-same-colour]
        [c (board-ref x y)])
    (append (step x y c 1 0 t)
            (step x y c -1 0 t)
            (step x y c 0 1 t)
            (step x y c 0 -1 t)
            (step x y c 1 1 t)
            (step x y c 1 -1 t)
            (step x y c -1 1 t)
            (step x y c -1 -1 t)
            (filter (if need-same-colour (lambda(pair) #t) (diff-colour/null c))
                    (filter in-bound
                            (list (cons (+ x 2) (- y 1))
                                  (cons (+ x 2) (+ y 1))
                                  (cons (+ x 1) (- y 2))
                                  (cons (+ x 1) (+ y 2))
                                  (cons (- x 1) (+ y 2))
                                  (cons (- x 1) (- y 2))
                                  (cons (- x 2) (+ y 1))
                                  (cons (- x 2) (- y 1)))))
            )))
(define (step x y c s1 s2 need-same-colour)
  (define (h x y)
    (cond [(out-of-bound (cons x y)) '()]
          [(equal? (board-ref x y) 0) (cons (cons x y) (h (+ x s1) (+ y s2)))]
          [else (if need-same-colour (list (cons x y)) (if ((diff-colour/null c) (cons x y)) (list (cons x y)) '()))]  ))
  (h (+ x s1) (+ y s2)))

(define (r-pos c)
  (- 56 (char->integer c)))
(define (c-pos c)
  (- (char->integer c) 97))
(define (c-pos-inv int)
  (integer->char (+ 97 int)))
(define (r-pos-inv int)
  (integer->char (- 56 int)))
(define (ispawn c) (or (equal? c #\p) (equal? c #\P)))
(define (isking c) (or (equal? c #\k) (equal? c #\K)))
(define (isbish c) (or (equal? c #\b) (equal? c #\B)))
(define (isnite c) (or (equal? c #\n) (equal? c #\N)))
(define (isruke c) (or (equal? c #\R) (equal? c #\r)))
(define (isquen c) (or (equal? c #\q) (equal? c #\Q)))
(define (iszero c) (equal? c 0))
(define (isblack c) (and (char? c) (char-range c 97 122)))
(define (iswhite c) (and (char? c) (char-range c 65  92)))
(define (colour c) (if (isblack c) 'b (if (iswhite c)'w 'empty-location)))
(define (ispiece c) (or (isblack c) (iswhite c)))
(define (row? c) (char-range c 97 104))
(define (col? c) (char-range c 49 56))
(define (make-SAN pair) 
  (string (c-pos-inv (cdr pair)) (r-pos-inv (car pair))))
(define (char-range c a b)
  (and (>= (char->integer c) a) (<= (char->integer c) b)))
(define (str->pair s) (cons (r-pos (string-ref s 1)) (c-pos (string-ref s 0))))
(define (tell-x str)
  (display str)
  (newline)
  (flush-output))
(define a 0)
(define (myqueen)
  (if (equal? iam 'b) "q" "Q"))
(define (myking-char)
  (if (equal? iam 'b) #\k #\K))
(define (oppking-char)
  (if (equal? iam 'b) #\K #\k))
;(set-from-here!)

;;;;;;;;;;;;;final lines of code
(define (move-maker pair1 pair2)
  (let* ([str-h (string-append (make-SAN pair1) (make-SAN pair2))]
         [str (if (and (ispawn (board-ref (car pair1) (cdr pair1)))
                       (or (= (car pair2) 0) (= (car pair2) 8))) 
                  (string-append str-h (myqueen))
                  str-h)])
    (tell-x (string-append "move " str))
    (board-move str)
    (check-for-check (if (equal? iam 'b) 'w 'b))
    ))
(define (run-chess)
  (define (run-h str)
    (if (equal? str "quit")
        void
        (begin (action! str) 
               (run-h (read-line)))))
  (run-h (read-line)))
(run-chess)
