; Akshai Sarma
; akshai.sarma@macaulay.cuny.edu

; CSc 33500
; Homework 6
; Due by midnight, Wednesday, November 10

;;;A good job.


; Problem 1.  Dynamic binding (or dynamic scoping) is an alternative design in which the procedure
; body is evaluated in an environment obtained by extending the environment at the point of call.  For
; example, in

;((lambda (a)
;   ((lambda (p a)
;      (cons a (p 2)))
;    (lambda (x) (cons x a))
;    5))
; 3)

; the a in the body of p would be bound to 5, not 3.  

; In this problem you are to modify tls-scheme so that it uses dynamic rather than lexical binding.  

; The idea is to store all environment bindings on a global table, and to have all environment operations
; apply to this table.  Regarding the table as a stack, bindings are pushed onto this 
; stack when a procedure is called and popped from the stack when the procedure returns.  

; Thus one would not implement closures - instead, *lambda would be given

;(define *lambda
;  (lambda (e)
;    (build (quote non-primitive)
;           (cdr e))))

; Similarly, there would never be a need to pass a table parameter, as the environment
; is global.  

; Test the modified interpreter, and argue that you have correctly implemented dynamic binding.



; Problem 2.  Modify make-account (given in lecture11.scm) so that it expects a single argument,
; opening-balance, and modify the body accordingly

; Problem 3. Develop a function, counted, which expects as argument
; a function f of one argument, and which returns a function which is like
; f except for keeping track of the number of times f has been called

; thus:  if (define counted-sq (counted sq)) when (define sq (lambda (x) (* x x))),
; ((counted-sq 'func) 2) = 4 but, after 10 calls, (counted-sq 'count) = 10. 

; Problem 4.  Use the environment model to explain your solutions to the previous
; two exercises.


; ******************Answers******************************

; ******************Problem 2****************************


(define (make-account opening-balance)
  
  (define balance opening-balance)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error "Insufficient Funds")))
  
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  
  (define (dispatch m)
    (cond ((eq? m 'balance) balance)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unimplemented Method"))))
  
  dispatch)

; ******************Problem 3****************************

(define (counted func)
  
  (define count 0)
  
  (define (increment x)
    (begin
      (set! count (add1 count))
      (func x)))
  
  (define (dispatch m)
    (cond ((eq? m 'func) increment)
          ((eq? m 'count) count)
          (else (error "Unimplemented Method")))
    )
  dispatch)

(display 'q2)
(newline)
(define myacc (make-account 100))
((myacc 'withdraw) 50)
((myacc 'deposit) 50)
(myacc 'balance) 
;((myacc 'withdraw) 150)

(display 'q3)
(newline)
(define afunc (counted add1))
((afunc 'func) 2)
(afunc 'count)
((afunc 'func) 4)
(afunc 'count)
((afunc 'func) 6)
(afunc 'count)