#lang racket

(require rackunit)
(require rackunit/text-ui)


;CS 270 Math Foundations of CS
;Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson
;Drexel University
;Homework 4

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;In this exercise, we will directly relate integers and boolean values.
;We will use this ability to generate partial truth tables.
;Specifically, we will generate the inputs to the function.
;Next week, we will create the truth table outputs.
;Afterwards, will will combine these parts to solve a problems.

;Question 1
;Convert an Integer to a List of true/false values.
;If we want to convert the integer 9 to a list of t/f values
;we start by finding out its remainder and quotient when divided by 2.
;(remainder 9 2) = 1
;(quotient 9 2) = 4
;This tells us the least significant bit is a 1
;We will represent 1 as #t, so the list is currently (#t)
;Next, we repeat the proccess with the quotient 4
;(remainder 4 2) = 0
;(quotient 4 2) = 2
;Zero is false, so we add this to the list (#f #t)
;Repeat with the quotient 2
;(remainder 2 2) = 0
;(quotient 2 2) = 1
;Zero is false, the list becomes (#f #f #t)
;(remainder 1 2) = 1
;(quotient 1 2) = 0
;One is true, the list because (#t #f #f #t)
;The quotient was zero meaning we can stop.

;Implement the following function
;Since we want to line up with standard binary 2^0 being the last value,
;It is easiest to make L a parameter and use a helper function.
;Note: 0 is not supported by this function.
;We will deal with it in the next part.
(define (int_to_bool n)
  (int_to_bool_h n '())
)
;NOTE: You MAY NOT use reverse to solve this problem.
(define (int_to_bool_h n L)
  (cond
    ((equal? 0 n) L)
    ((equal? (remainder n 2) 0) (int_to_bool_h (quotient n 2) (cons #f L)))
    (else(int_to_bool_h (quotient n 2) (cons #t L)))
  )
)
;Test to see if you function works correctly
(define-test-suite test_int_to_bool
  (check-equal? (int_to_bool 0) '())
  (check-equal? (int_to_bool 1) '(#t))
  (check-equal? (int_to_bool 2) '(#t #f))
  (check-equal? (int_to_bool 3) '(#t #t))
  (check-equal? (int_to_bool 4) '(#t #f #f))
  (check-equal? (int_to_bool 5) '(#t #f #t))
  (check-equal? (int_to_bool 6) '(#t #t #f))
  (check-equal? (int_to_bool 7) '(#t #t #t))
  (check-equal? (int_to_bool 8) '(#t #f #f #f))
  (check-equal? (int_to_bool 9) '(#t #f #f #t))
  (check-equal? (int_to_bool 10) '(#t #f #t #f))
  (check-equal? (int_to_bool 11) '(#t #f #t #t))
  (check-equal? (int_to_bool 12) '(#t #t #f #f))
  (check-equal? (int_to_bool 13) '(#t #t #f #t))
  (check-equal? (int_to_bool 14) '(#t #t #t #f))
  (check-equal? (int_to_bool 15) '(#t #t #t #t))
)
(display "Question 1.) int_to_bool Results (8 points)\n")
(define q1_score (* (/ 1.0 2.0) (- 16 (run-tests test_int_to_bool 'verbose))))


;Question 2
;Only significant binary digits are stored by the above function.
;In reality, we would want every number to have the same bit length.
;Write a function to pad #f onto the front of the list.
(define (pad num_bits bit_list)
  (cond
    ((equal? (- num_bits (length bit_list)) 0) bit_list)
    (else(cons #f (pad (- num_bits 1) bit_list)))
  )
)
;Check your function with the below tests
(define-test-suite test_pad
  (check-equal? (pad 5 (int_to_bool 0))  '(#f #f #f #f #f))
  (check-equal? (pad 5 (int_to_bool 1))  '(#f #f #f #f #t))
  (check-equal? (pad 5 (int_to_bool 2))  '(#f #f #f #t #f))
  (check-equal? (pad 5 (int_to_bool 3))  '(#f #f #f #t #t))
  (check-equal? (pad 5 (int_to_bool 4))  '(#f #f #t #f #f))
  (check-equal? (pad 5 (int_to_bool 5))  '(#f #f #t #f #t))
  (check-equal? (pad 5 (int_to_bool 6))  '(#f #f #t #t #f))
  (check-equal? (pad 5 (int_to_bool 7))  '(#f #f #t #t #t))
  (check-equal? (pad 5 (int_to_bool 8))  '(#f #t #f #f #f))
  (check-equal? (pad 5 (int_to_bool 9))  '(#f #t #f #f #t))
  (check-equal? (pad 5 (int_to_bool 10)) '(#f #t #f #t #f))
  (check-equal? (pad 5 (int_to_bool 11)) '(#f #t #f #t #t))
  (check-equal? (pad 5 (int_to_bool 12)) '(#f #t #t #f #f))
  (check-equal? (pad 5 (int_to_bool 13)) '(#f #t #t #f #t))
  (check-equal? (pad 5 (int_to_bool 14)) '(#f #t #t #t #f))
  (check-equal? (pad 5 (int_to_bool 15)) '(#f #t #t #t #t))
)
(display "Question 2.) pad Results (8 points)\n")
(define q2_score (* (/ 1.0 2.0) (- 16 (run-tests test_pad 'verbose))))

;Question 3
;Generate a Truth Table
;Given a number of variables n
;generate a truth table will all variable settings.
;The truth table should have rows with values starting at
;2^n-1 and ending at 0.
;For example, the truth tables for n=2 is
;( (#t #t) (#t #f) (#f #t) (#f #f) )
;Notice: A "Table" is a list of lists
;As integers this is (3 2 1 0)
;The number of bits is n.

;Define the below function
(define (tt_inputs n)
  (tt_inputs_h n (- (expt 2 n) 1))
)
(define (tt_inputs_h bits row_val)
  (cond
    ((equal? 0 row_val) (cons (pad bits (int_to_bool row_val)) '()))
    (else(cons(pad bits (int_to_bool row_val)) (tt_inputs_h bits (- row_val 1))))
  )
)
;Check your function with the following tests
(define-test-suite test_tt
  (check-equal? (tt_inputs 0)
                '(())
  )
  (check-equal? (tt_inputs 1)
                '( (#t) (#f) )
  )
  (check-equal? (tt_inputs 2)
                '( (#t #t) (#t #f) (#f #t) (#f #f))
  )
  (check-equal? (tt_inputs 3)
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   )
   )
   (check-equal? (tt_inputs 4)
                '(
                   (#t #t #t #t)
                   (#t #t #t #f)
                   (#t #t #f #t)
                   (#t #t #f #f)
                   (#t #f #t #t)
                   (#t #f #t #f)
                   (#t #f #f #t)
                   (#t #f #f #f)
                   (#f #t #t #t)
                   (#f #t #t #f)
                   (#f #t #f #t)
                   (#f #t #f #f)
                   (#f #f #t #t)
                   (#f #f #t #f)
                   (#f #f #f #t)
                   (#f #f #f #f)
                   )
   )
)
(display "Question 3.) tt_inputs Results (10 points)\n")
(define q3_score (- 10 (* 2 (run-tests test_tt 'verbose))))

;Question 4
;The inputs we made above have the format '(#t #f #f #t).
;We need boolean expressions that work with this format.
;Example: (a -> b) = (~a \/ b)
(define (implies_verify boolean_vars)
  (let (;Start of name list
        (a (list-ref boolean_vars 0));Pairs (name value)
        (b (list-ref boolean_vars 1))
      );End of name list
    (equal? (implies a b ) (or (not a) b))
 );end of let
)
;Test Implies Def
(define-test-suite test_implies
  (check-equal? (implies_verify '(#t #t)) #t)
  (check-equal? (implies_verify '(#t #f)) #t)
  (check-equal? (implies_verify '(#f #t)) #t)
  (check-equal? (implies_verify '(#f #f)) #t)
)
(display "Example.) Results of Implies Verify\n")
;(run-tests test_implies)


;Write the following three simple boolean expressions as functions. 

;a.) Demorgan's Law
;implement ~(a /\ b) = (~a \/ ~b)
(define (demorgan_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
       )
    (equal? (not(and a b))(or (not a) (not b)))
  )
)
;Test Implies Def
(define-test-suite test_demorgan
  (check-equal? (demorgan_verify '(#t #t)) #t)
  (check-equal? (demorgan_verify '(#t #f)) #t)
  (check-equal? (demorgan_verify '(#f #t)) #t)
  (check-equal? (demorgan_verify '(#f #f)) #t)
)
(display "4a.) Results of Demorgan Verify (8 points)\n")
(define q4a_score (- 8 (* 2 (run-tests test_demorgan 'verbose))))

;b.) Absorption
;implement (x /\ (x \/ y)) = x
(define (absorp_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
       )
    (equal? (and a (or a b)) a)
  )
)
;Test Implies Def
(define-test-suite test_absorp
  (check-equal? (absorp_verify '(#t #t)) #t)
  (check-equal? (absorp_verify '(#t #f)) #t)
  (check-equal? (absorp_verify '(#f #t)) #t)
  (check-equal? (absorp_verify '(#f #f)) #t)
)
(display "4b.) Results of Absorption Verify (8 points)\n")
(define q4b_score (- 8 (* 2 (run-tests test_absorp 'verbose))))

;c.) Associativity
;implement x \/ (y \/ z) = (x \/ y) \/ z
(define (assoc_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
        (c (list-ref bool_vars 2))
       )
    (equal? (or a (or b c)) (or (or a b) c))
  )
)
;Test Implies Def
(define-test-suite test_assoc
  (check-equal? (assoc_verify '(#t #t #t)) #t)
  (check-equal? (assoc_verify '(#t #t #f)) #t)
  (check-equal? (assoc_verify '(#t #f #t)) #t)
  (check-equal? (assoc_verify '(#t #f #f)) #t)
  (check-equal? (assoc_verify '(#f #t #t)) #t)
  (check-equal? (assoc_verify '(#f #t #f)) #t)
  (check-equal? (assoc_verify '(#f #f #t)) #t)
  (check-equal? (assoc_verify '(#f #f #f)) #t)
)
(display "4c.) Results of Associativity Verify (16 points)\n")
(define q4c_score (- 16 (* 2 (run-tests test_assoc 'verbose))))

;Question 5
;Write a function that takes
;fun - a function that takes a list of boolean values and returns a boolean
;tt - a truth table (list of lists of T/F values)
;And returns a list of T/F values with results
;For example if fun computes (not a)
;and tt = ( (#t) (#f) )
;Then the return of
;(evaluate_tt fun tt) should be (#f #t)
(define (evaluate_tt  fun tt)
  (map fun tt)
)
;Test your function
(define-test-suite test_eval_tt
  (check-equal?
   (evaluate_tt implies_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt demorgan_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt absorp_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt assoc_verify '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   ))
   '(#t #t #t #t #t #t #t #t)
  )
)
(display "5.) Results of Evaluate on Truth Table (12 points)\n")
(define q5_score (- 12 (* 3 (run-tests test_eval_tt 'verbose))))

;Question 6
;Write a function that takes a list of true/false values
;and determines if that are a tautology.
;A null list is by definition a tautology
(define (tautology result_list)
  (cond
    ((null? result_list) #t)
    ((equal? #t (first result_list)) (tautology (rest result_list)))
    (else #f)
  )
)
;Test your function
(define-test-suite test_taut
  (check-equal? (tautology '()) #t)
  (check-equal? (tautology '(#t)) #t)
  (check-equal? (tautology '(#f)) #f)
  (check-equal? (tautology '(#t #t)) #t)
  (check-equal? (tautology '(#t #f)) #f)
  (check-equal? (tautology '(#f #t)) #f)
  (check-equal? (tautology '(#f #f)) #f)
  (check-equal? (tautology '(#t #t #t #t #t #t #t)) #t)
  (check-equal? (tautology '(#t #t #t #t #t #f #t)) #f)
)
(display "6.) Results of Evaluation on Truth Table (18 points)\n")
(define q6_score (- 18 (* 2 (run-tests test_taut 'verbose))))

;Question 7
;Write a function that takes a function and the number of variables it has
;and determines if the function is a tautology
(define (is_taut func n)
  (tautology (map func (tt_inputs n)))
)

;Test your function
(define-test-suite test_is_taut
  (check-equal? (is_taut implies_verify 2) #t)
  (check-equal? (is_taut demorgan_verify 2) #t)
  (check-equal? (is_taut absorp_verify 2) #t)
  (check-equal? (is_taut assoc_verify 3) #t)
  (check-equal? (is_taut (lambda (X) (and (first X) (second X))) 2) #f)
  (check-equal? (is_taut (lambda (X) (or (first X) (second X))) 2) #f)
)
(display "7.) Results of Is Tautology Question (12 points)\n")
(define q7_score (- 12 (* 2 (run-tests test_is_taut 'verbose))))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/8\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/8\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/10\n")
(display "Q4A Scored: ")
(display q4a_score)
(display "/8\n")
(display "Q4B Scored: ")
(display q4b_score)
(display "/8\n")
(display "Q4C Scored: ")
(display q4c_score)
(display "/16\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/12\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/18\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/12\n")

(define grand_total (+ q1_score q2_score q3_score q4a_score q4b_score q4c_score q5_score q6_score q7_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")