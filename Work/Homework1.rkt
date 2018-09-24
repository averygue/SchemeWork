#lang racket

;Avery Guething cosc3410
(define updown ; list of numbers --> list of numbers
  (lambda (inList)
  (cond [(null? inList) inList]
        [(even? (car inList))  (cons (+ 1 (car inList)) (updown (cdr inList)))]
        [(odd? (car inList))  (cons (+ -1 (car inList)) (updown (cdr inList)))]
        )))

;Tests included;
; All odd numbers, all even numbers, empty list, negative even numbers, all repeated
(display (updown '(2 4 10 3 6)))
(display (updown '(1 3)))
(display (updown '()))
(display (updown '(-2 -4 -10 -4 -6)))
(display (updown '(1 1 1 1 1 1 1 1 1)))

(define zip ; list list --> list
  (lambda (a b)
    (cond[(and (null? a) (null? b)) '()]
         [(not (eq? (length  a) (length  b))) (error 'zip "list length are not equal")]
         [else (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))]

    )
  )
)

;Tests included;
; All numbers, numerical and letter list, two empty lists, two unequal lists, decimal list test, two same lists,
;display ((zip '() '() ))
;display((zip '((a b) c (d e f)) '(c (a) (b c)) ))
;display((zip '(1 2 -3) '(50 40 30 20)))
;display((zip '(1.5 2.2 3.0) '(50 40 30 20)))
;display((zip '(3 3 3) '(3 3 3))

(define deep-mult ; list--> product of all numbers inside of the list
  (lambda (inList)
    (cond [(null? inList) 1]
          [(number? (car inList)) (* (car inList) (deep-mult (cdr inList)))]
          [(list? (car inList)) (* (deep-mult(car inList)) (deep-mult(cdr inList)))]
          [else (deep-mult (cdr inList))]
    )
  )
)

;Tests included;
; All numbers, numbers and letters, empty list, no number list, lots of parentheses, lots of repeated patterns
;display((deep-mult '(5 a b 8 2)))
;display((deep-mult '((4 (6 1)) 2 3 (4))))
;display((deep-mult '(these (aren't 77) (all 32 (numbers 93 here)))))
;display((deep-mult '()))
;display((deep-mult '(abcdefghikla)))

(define drop-parens ; list --> list
  (lambda (inList)
    (cond [(null? inList) inList]
    [(pair? (car inList)) (append (drop-parens (car inList)) (drop-parens (cdr inList)))]
    [else (cons (car inList) (drop-parens (cdr inList)))]
    )))

;Tests included;
; All numbers, numbers and letters, empty list, lots of parentheses, same pattern over and over again, same number.
; display( (drop-parens '((a 34)(b 77)(g 6)) ))
;display ((drop-parens '(a b c) ))
;display((drop-parens '(()((() x)())) ))
;display(drop-parens '(()((() )())) ))
;display((drop-parens '(a b c d e f g h i j k l 123) ))