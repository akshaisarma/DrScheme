;    Akshai Sarma
;    akshai.sarma@macaulay.cuny.edu
;    CSc 335 Homework 1

;;;;bad answer to 1
;;;bad proofs. Induction proof has three parts, you need all of them. once you learned how to do it, your proofs don't have to be this long.

;;;;D+
;;;;changed to B-
;;;;structure of proofs not clear enough.

;1.  Give a Backus-Naur definition of atoms, using the idea that
;    an atom is a string of letters, digits, or characters other than ( or )

;2.  Give an inductive proof that the definition of the predicate lat? in lecture
;    2 is correct.  

;3.  Give an inductive proof that alt-lat?, also defined in lecture 2, is correct.

;4.  Specify, write and prove correct a program s-exp? which implements a predicate
;    for s-exps, as defined in lecture 1.

;================================ANSWERS=========================================

;1   Answer

;    <atom>              ::= <special_character> | <atom><atom> 
;;;;;just special characters? how about digits and letters?

;    <special_character> ::= any character excluding "(" | ")" | " "

;2   Answer

;    The definition of lat? is reproduced below 

(define lat?
  (lambda (lst)
    (cond ((null? lst) #t)
          (else 
           (and
            (atom? (car lst)) 
            (lat? (cdr lst)))))))

;    We need to show that lat? returns #t iff lst is a list of a atoms and otherwise
;    returns false. We argue by induction on the length of lat.

;    For convenience, lat ::= () | (cons atom lat)

;    If lst has length 0, then lst = () and therefore, lst = lat and the function
;    correctly returns #t as expressed by (null? lst), satisfying the specification.

;    Assume that (atom? (car lst)) works correctly and returns #t iff (car lst) is an
;    atom. Also assume that (lat? (cdr lst)) works correctly and consider (lat? lst).

;;;;;you don't need to assume the atom? part works correct. The second assumption is ok now.
;;;;;however, you can also make an assumption on the correctness of the whole function when the input is smaller.

;    If (car lst) is not an atom, (atom? (car lst)) returns #f and the (and ...) 
;    returns #f. This is then the value of (lat? lst), which is correct since the car
;    of lst was not an atom and consequently, lst was not a lat, satisfying the 
;    specification. 

;    On the other hand, if (car lst) is an atom, (atom? (car lst)) returns #t and the
;    the function now computes (and #t (lat? (cdr lst))). Now, lst is a lat iff the
;    car of lst is an atom and (cdr lst) is a lat. Since we assumed that the recursive
;    call works correctly, (and #t (lat? (cdr lst))) returns #t iff (lat? (cdr lst))
;    returns #t. Otherwise, it returns #f. These would then be the values returned by
;    (lat? lst) We see that if (car lst) is an atom and (cdr lst) is a lat, our 
;    function should return #t and otherwise return #f, satisfying its specification.


;3   Answer

;    The definition of a;t-lat? is reproduced below 

(define alt-lat?
  (lambda (lst)
    (cond ((null? lst) #t)
          ((atom? (car lst)) (alt-lat? (cdr lst)))
          (else #f))))

;    We need to show that alt-lat? returns #t iff lst is a list of atoms and 
;    otherwise returns false. We argue by induction on the length of alt-lat?.

;    For convenience, lat ::= () | (cons atom lat)

;    If lst has length 0, then lst = () and therefore, lst = lat and the function 
;    correctly returns #t as expressed by (null? lst), satisfying the specification.

;    If lst is not null, that is (null? lst) returned #f, the function computes 
;    ((atom? (car lst)) (alt-lat? (cdr lst))). Assume that (atom? (car lst)) works 
;    correctly and returns #t iff (car lst) is an atom. Also assume that 
;    (alt-lat? (cdr lst)) works correctly. 

;;;;it would be better if you clearly state your IH in a seperate paragraph, rather than mixing it with the IS.

;;;If (car lst)is not an atom, 
;    (atom? (car lst)) is #f. In this case, the next statement is executed, which is 
;    (else #f). This is always true and (alt-lat? lst) returns #f as it should,
;    satisfying the specification. lst is not a lat if it it does not have an atom 
;    as its car (and it is not the null list)

;    If (atom? (car lst)) is #t, the function computes (alt-lat? (cdr lst)). Since we 
;    assumed that the recursive call works correctly, (alt-lat? (cdr lst)) returns #t
;    iff (cdr lst) is a lat and #f otherwise.This is then the value returned by 
;    (alt-lat? lst). This is correct because lst is a lat iff its car is an atom and
;    its cdr is a lat. We have shown that the function satisfies its specification.

;4   Answer


;   Specifications
;   Pre:  x is any expression of characters that DrScheme allows to be entered as an 
;         argument to s-exp? 
;   Post: (s-exp? x) = #t if x is an S-Expression
;                    = #f otherwise

;   Using the following definition of atom? to make s-exp? more modular

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define s-exp?
  (lambda (x)
    (cond 
      ((null? x) #t)
      ((atom? x) #t) 
      ((pair? x) (and (s-exp? (car x)) (s-exp? (cdr x))))
      (else #f))))

;   Since DrScheme does not allow anything other than valid s-exps to be entered, this
;   function will practically never return #f. We could just returned #t for whatever
;   argument, since the arguments will all be s-exps, as enforced by DrScheme.

;   Proof

;   We need to show that s-exp? returns #t iff x is an S-Expression. We argue by 
;   induction on the length of x.

;   For convenience, s-exp ::=  () | atom | (s-exp ... s-exp)

;   If x is the empty list, the function returns #t, as it should because () is an
;   s-exp.

;   If x is not the empty list, that is if (null? x) returned #f, the function checks
;   (atom? x). If x is an atom, it is also an s-exp and the function returns #t as it 
;   as it should.

;   If x is not an atom or the empty list, x must be a pair in order for it to be an
;   s-exp. (pair? x) returns #t iff x is a pair and #f otherwise. If x is not pair, 
;   the function computes (else #f), which returns #f as the value of (s-exp? x). This
;   is also correct as if x is not null, not an atom and not a pair, then it is not an
;   s-exp.

;   On the other hand, if x is a pair, (pair? x) returns #t and the function computes
;   (and (s-exp? (car x)) (s-exp? (cdr x))). Assume that the recursive call works 
;   correctly

;;;; Same problem. it would be better if you do this assumption seperately.

;;;and consider (s-exp? x). (and (s-exp? (car x)) (s-exp? (cdr x))) returns 
;   #t iff (car x) and (cdr x) are both s-exp. If (s-exp? (car x)) is #f, this is then 
;   the value returned by (s-exp? x). If (s-exp? (car x)) is #t, (s-exp? (cdr x)) is 
;   then computed. The result is then returned as the value of (s-exp? x), which is 
;   clearly correct as x is an s-exp if its car is an s-exp and if its cdr is an s-exp.
;   This proves that (s-exp? x) satisfies its specification.
  