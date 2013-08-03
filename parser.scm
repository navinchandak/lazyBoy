;number :- one or more digits
;identifier := a letter followed by zero or more letters or digits
;variable :- identifier | identifier `[` expression `]`
;term :- number | variable | `(`expression`)`
;expression :- term { `+` expression | epsilon }
;assignment :- variable = expression

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)


(define (isempty? str)
  (if (string=? str "") #t #f))

(define (first str)
  (string-ref str 0))

(define (rest str)
  (substring str 1))

(define (pred-p p)
  (lambda (str)
    (cond ((string=? str "") `fail)
          ((not (p (first str))) `fail)
          (else (cons (first str) (rest str))))))  

(define single-digit-p (lambda (str) ((pred-p char-numeric?) str)))

(define single-alphabet-p (lambda (str) ((pred-p char-alphabetic?) str)))

(define (seq p1 p2 f)
  (lambda (str)
    (let ((parse-result1 (p1 str)))
      (if (equal? parse-result1 `fail) `fail
          (let ((parse-result2 (p2 (cdr parse-result1))))
            (if (equal? parse-result2 `fail) `fail
                (cons (f (car parse-result1) (car parse-result2))
                                      (cdr parse-result2))))))))

(define (alt p1 p2)
  (lambda (str)
    (let ((parse-result1 (p1 str)))
      (if (equal? parse-result1 `fail) (p2 str)
          parse-result1))))  



(define (combine-cc char1 char2)
  (list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str) (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1) (string->list str2))))

(define epsilon-p (lambda (str) (cons "" str)))

(define (zero-or-more p f)
  (lambda (str) 
    ((alt (seq  p (zero-or-more p f) f) epsilon-p) str))) 

(define (one-or-more p f)
  (lambda (str) 
    ((seq  p (zero-or-more p f) f)  str))) 


;(define variable-p
;  (lambda (str)
;    ((seq single-alphabet-p 
;              (zero-or-more (alt single-alphabet-p single-digit-p) combine-cs)
;              combine-cs) str)))

(define number-helper
  (lambda (str)
    ((seq whitespace-p
          (one-or-more single-digit-p combine-cs)
          combine-ss) str)))

(define number-p
  (lambda (str) 
    (let ((parse-result (number-helper str)))
      (if (equal? parse-result `fail) `fail
          (cons (make-num (string->number (car parse-result)))
                (cdr parse-result))))))
    
(define variable-p 
  (lambda (str) 
    ((seq identifier-p 
          (alt (seq (char-p #\[)
                    (seq expression-p 
                         (char-p #\])  
                         (lambda (x y) x))
                    (lambda (x y) y))
               epsilon-p)
          (lambda (x y) (if (string? y) x
                            (make-gnode `ARRAY (list x y))))) str)))

(define assignment-p
  (lambda (str)
    ((seq variable-p
         (seq (char-p #\=)
              expression-p
              (lambda (x y) y))
         (lambda (x y) (make-gnode `ASSIGN (list x y)))) str)))
          
    
(define identifier-p 
  (lambda (str) 
    (let ((parse-result (identifier-helper str)))
      (if (equal? parse-result `fail) `fail
          (cons (make-ident (car parse-result))
                (cdr parse-result))))))



(define identifier-helper
  (lambda (str)
    ((seq whitespace-p
            (seq single-alphabet-p 
                 (zero-or-more (alt single-alphabet-p 
                                    single-digit-p) 
                               combine-cs)
                 combine-cs)
            combine-ss)
    str)))

;(define number-p
;  (lambda (str)
;    ((one-or-more single-digit-p combine-cs) str)))

(define (char-p char)
  (lambda (str) 
    ((seq whitespace-p 
          (pred-p (lambda (x) (char=? x char)))
          combine-sc)  str)))

(define (throw-away x y) 0)

(define expression-p 
  (lambda (str)
    ((seq term-p
          (alt (seq (char-p #\+) 
                    expression-p 
                    (lambda (x y) (cons x y)))
               epsilon-p)
          make-plus-exp) str)))

(define term-p
  (lambda (str)
    ((alt (alt variable-p 
               number-p)
          (seq (char-p #\()
               (seq expression-p 
                    (char-p #\))
                    (lambda (x y) x))
               (lambda (x y) y))) str)))

(define (make-plus-exp arg1 arg2)
  (if (equal? "" arg2) arg1
      (make-gnode `PLUS (list arg1 (cdr arg2)))))

(define single-ws-p
  (lambda (str) ((pred-p (lambda (x) (char-whitespace? x))) str))) 

(define whitespace-p
  (zero-or-more single-ws-p (lambda (x y) "")))

