; Akshai Sarma
; akshai.sarma@macaulay.cuny.edu
; Homework 2 CSc 33500 Fall 2010

;;;A+, good job on the proofs, very clear.


; Solutions are due by midnight, Friday September 24.  Please submit your 
; solutions to Mr. Wu, as per the instructions for the first homework set. 

; 1. Write and prove correct a procedure that takes an arbitrary list as its argument and counts the number of
; opening and closing parentheses in the list. For example, the result for '() ought to be 2, the result for
; '((a b c)) ought to be 4,  and the result for '(((a b) (a b c)) (d (e (f g h i)))) ought to be 14.  

; 2. Write and prove correct a procedure to compute the width of a widest subtree in a tree t. You may wish to use the 
; scheme primitive, max.  On input '(((a b) (a (b c d e f) g)) (d (e (f g h i)))), for example, your procedure 
; ought to return 5.

; 3. Perhaps using your solution to the previous problem, write and prove correct a procedure which returns a widest
; subtree in a tree t.  Do not worry about efficiency - this exercise seeks to develop your understanding of recursion.
; On input '(((a b) (a (b c (d e) f g) h)) (d (e (f g h i)))), for example, the result ought to be 
; (b c (d e) f g)

;============Answers=================

;1  Answer

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
    ))

(define parancount
  (lambda (l)
    (cond
    ((null? l) '2)
    ((atom? (car l)) (parancount (cdr l)))
    (else
    (+ (parancount (car l)) (parancount (cdr l))) 
    ))))


; To prove the procedure correct, I argue by tree induction on the list l

; Basis Step: If l is the null list, parancount returns 2, which is the correct
; number of parantheses

; Induction Hypothesis: parancount returns the correct number of parantheses on 
; an all recursive calls of smaller sizes than the input list.

; Induction Step: If l is not the empty list, the car of l is an atom or it is not.
; If (car l) is an atom, it has no parantheses around it and parancount is applied 
; to the cdr of l, which contains all the parantheses of l. (cdr l) is certainly 
; smaller than l because it is "narrower" than l in the tree representation. So,
; by IH, (parancount (cdr l)) returns the correct number of parantheses in (cdr l).

; If (car l) is not an atom, it is an s-expression. The number of parantheses in l 
; is then the sum of the number of parantheses in (car l) and (cdr l). The (car l) 
; is "shorter" than l in the tree representation of l in terms of height. Therefore,
; it is smaller than l and by the IH, the correct number of parantheses is returned
; for the recursive call. Similarly, (cdr l) is "narrower" and therefore, smaller 
; than l and the recursive call returns the correct number of parantheses. The
; correct number of parantheses in l is the arithmetic sum of these two recursive
; calls, showing that parancount works correctly for any input list l.

;2 Answer

(define lenwidestsubtree
  (lambda (l)
    (cond
      ((null? l)'0)
      ((atom? (car l)) (max (length l) (lenwidestsubtree (cdr l))))
      (else
       (max (length l) (lenwidestsubtree (car l)) (lenwidestsubtree (cdr l)))
      ))))

; To prove the procedure correct, I argue by tree induction on the list/tree l.

; Basis Step: If l is the null list, lenwidestsubtree returns 0, which is the correct
; width of the widestsubtree in l.

; Induction Hypothesis: lenwidestsubtree returns the width of the widest subtree in a
; list on all recursive calls of lists smaller than the input list.

; Induction Step: If l is not the empty list, l has a car. Now, the car of l
; is either an atom or it is not. If it is an atom, the widest subtree is either l 
; itself or it is in (cdr l). Using the max function, the length of l is compared to
; the value returned by lenwidestsubtree (cdr l). By the IH, since (cdr l) is 
; "narrower" and consequently smaller than l, lenwidestsubtree returns the width of the
; widest subtree in (cdr l). The larger of the two values is returned as the width of
; the widest subtree in l, which is correct, as the larger of the two values is the 
; length of the widest subtree in l. Note, "larger" heretofore also includes equal lengths.

; If (car l) is not an atom, the widest subtree is either l and/or in (car l) and/or in 
; (cdr l). Using the max function again, the length of l is compared to the values returned by
; lenwidestsubtree (car l) and lenwidestsubtree(cdr l). (car l) is "shorter" and therefore 
; smaller than l. (cdr l) is "narrower" and therefore smaller than l. Then, both these recursive 
; calls return the width of the widest subtree in their respective arguments due to IH. The 
; max function returns the largest of these values, which is the correct width of the widest
; subtree in l.

; Thus, lenwidestsubtree returns the width of the widest subtree in any list/tree l. Here, I am
; using lists to mean trees because that is one way they can be represented in Scheme.

;3 Answer

(define widestsubtree
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
       (cond
         ((>= (length l) (lenwidestsubtree (cdr l))) l)
         (else (widestsubtree (cdr l)))))
      (else
       (cond
         ((>= (length l) (max (lenwidestsubtree (car l)) (lenwidestsubtree (cdr l)))) l)
         ((>= (lenwidestsubtree (car l)) (lenwidestsubtree (cdr l))) (widestsubtree (car l)))
         (else (widestsubtree (cdr l)))))
      )))

; To prove the procedure correct, I argue by tree induction on the list/tree l. At this point,
; lenwidestsubtree has been shown to work correctly and return the length of the widest
; subtree in its argument.

; Basis Step: If l is the null list, widestsubtree correctly returns the empty list, 
; which is the widest subtree in l. 

; Induction Hypothesis: widestsubtree returns the widest subtree on all recursive
; calls of smaller sizes than the input list.

; Induction Step: If l is not null, it has a (car l). (car l) is either an atom or
; it is not. If (car l) is an atom, then it does not have any subtrees. Then, the
; widest subtree of l is either l itself or it is in (cdr l).If the width of l is >= 
; the width of the widest subtree of (cdr l), then the widest subtree of l is l itself, 
; which is returned.

; If the width of l is < the width of the widest subtree of (cdr l), the function
; widestsubtree is applied to (cdr l). By IH, this call returns the widest subtree in
; (cdr l), as it is smaller than l. This is then the correct widest subtree in l.

; If (car l) is not an atom. The widest subtree can be l itself, and/or can be in (car l)
; and/or can be in (cdr l).  If the width of l is >= to the larger of the widest subtrees in 
; (car l)and (cdr l), then l itself is the widest subtree in l. This is computed by comparing
; the max of the widths of the widest subtrees in the (car l) and (cdr l) to (length l). If 
; the width of l is >= the width of the widest subtrees in (car l) and  (cdr l), then the
; widest subtree is l itself, which is correctly returned. 

; If (length l) was < (max (lenwidestsubtree (car l)) (lenwidestsubtree (cdr l))), then the 
; widest subtree is in (car l) and/or (cdr l). If the width of the widest subtree in (car l) >= 
; width of the widest subtree in (cdr l), the widest subtree is in (car l) and the function 
; is applied recursively to (car l). This recursive call works correctly, by the IH, since
; (car l) is "shorter" and consquently smaller than l and returns the widest subtree of (car l),
; which is the widest subtree of l.

; If lenwidestsubtree(car l) < lenwidestsubtree (cdr l), the widest subtree is clearly in 
; (cdr l) and widestsubtree is applied recursively to (cdr l). By the IH, the widest subtree 
; in (cdr l) is returned as (cdr l) is "narrower" and therefore, smaller than l. This is also 
; the widest subtree of l.

; This proves that widestsubtree returns the widest subtree in any arbitrary list/tree l.
(display '(my testcases) )
(newline)
(display 'q1) 
(newline)
(parancount '())
(parancount '(a ()))
(parancount '(()(a b c)))
(parancount '(a (a ((b)) c)))
(parancount '(((a b) (a b c)) (d (e (f g h i)))))
(display 'q2) 
(newline)
(lenwidestsubtree '())
(lenwidestsubtree '(a b c))
(lenwidestsubtree '(a (b c) (d e f g)))
(lenwidestsubtree '((a b c) (d e f g) (h i j k) l m ))
(lenwidestsubtree '(((a b) (a (b c (d e) f g) h)) (d (e (f g h i)))))
(display 'q3) 
(newline)
(widestsubtree '())
(widestsubtree '(a b c))
(widestsubtree '(a (b c) (d e f g)))
(widestsubtree '((a b c) (d e f g) (h i j k) l m ))
(widestsubtree '(((a b) (a (b c (d e) f g) h)) (d (e (f g h i)))))