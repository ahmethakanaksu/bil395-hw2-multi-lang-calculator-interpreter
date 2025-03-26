(define vars '()) ; değişken listesi: ((x . 5) (y . 8) ...)

(define (lookup var)
  (let ((pair (assoc var vars)))
    (if pair
        (cdr pair)
        (begin
          (display "Unknown variable: ") (display var) (newline)
          #f)))) ; hata durumunda #f döndür

(define (assign var val)
  (let ((pair (assoc var vars)))
    (if pair
        (set-cdr! pair val)
        (set! vars (cons (cons var val) vars)))))

(define (format-number val)
  (if (= val (round val))
      (inexact->exact (round val))
      val))

(define (calc expr)
  (define (f x) (exact->inexact x))
  (cond
    ((number? expr) (f expr))
    ((symbol? expr)
     (let ((v (lookup expr)))
       (if (eq? v #f) #f (f v))))
    ((list? expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (cond
         ((eq? op '+)
          (let ((a (calc (car args))) (b (calc (cadr args))))
            (if (or (eq? a #f) (eq? b #f)) #f (f (+ a b)))))
         ((eq? op '-)
          (let ((a (calc (car args))) (b (calc (cadr args))))
            (if (or (eq? a #f) (eq? b #f)) #f (f (- a b)))))
         ((eq? op '*)
          (let ((a (calc (car args))) (b (calc (cadr args))))
            (if (or (eq? a #f) (eq? b #f)) #f (f (* a b)))))
         ((eq? op '/)
          (let ((a (calc (car args))) (b (calc (cadr args))))
            (if (or (eq? a #f) (eq? b #f))
                #f
                (if (= b 0)
                    (begin (display "Error: Division by zero!") (newline) #f)
                    (f (/ a b))))))
         ((eq? op 'expt)
          (let ((a (calc (car args))) (b (calc (cadr args))))
            (if (or (eq? a #f) (eq? b #f)) #f (f (expt a b)))))
         (else
          (display "Invalid operator: ") (display op) (newline)
          #f))))
    (else
     (display "Invalid expression.") (newline)
     #f)))

(define (repl)
  (display "Scheme Calculator (enter 'exit' to quit)\n\n")
  (let loop ()
    (display ">> ")
    (let ((line (read)))
      (cond
        ((eof-object? line) (display "\nGoodbye!\n"))
        ((equal? line 'exit) (display "Goodbye!\n"))
        ((and (list? line) (eq? (car line) 'define))
         (let* ((name (cadr line))
                (val (calc (caddr line))))
           (if (not (eq? val #f))
               (begin
                 (assign name val)
                 (display name) (display " = ") (display (format-number val)) (newline)))
           (loop)))
        (else
         (let ((result (calc line)))
           (if (not (eq? result #f))
               (begin
                 (display "Result: ") (display (format-number result)) (newline))))
         (loop))))))

(repl)
