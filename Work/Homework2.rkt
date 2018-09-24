#lang racket

;Avery Guething cosc3410
(define in-range? ;in-range?: atom, atom, list --> boolean
  (lambda (least great inList)
  (cond [(null? inList) #t]
        [(>= (car inList) least) #f]
        [(<= (car inList) great) #f]
        [else (in-range? least great (cdr inList))]
        )))
;In-range tests included:
;True Test: (in-range? 3 12 '(5 3 9) )
;Null Test: (in-range? 3 12 '() )
;Same Two Parameter Test: (in-range? 4 4 '(5 3 9) )
;False Test: (in-range? 4 6 '(5 4 6) )
;Same Number Test: (in-range? 4 4 '(4 4 4 4 4) )

(define atom-count ;atom-count: atom List --> integer
  (lambda (atomTested atomList)
  (cond [(null? atomList) 0]
        [(eq? atomTested (car atomList))(+ 1 (atom-count atomTested (cdr atomList)))]
        [else (atom-count atomTested (cdr atomList))]
        )))
;atom-count tests included:
;True Test: (atom-count 'b '(a b g a b c b) )
;Empty Test: (atom-count 'b '() )
;Combination Test: (atom-count 'as '(as asa as asaf as asasas) )
;False Test: (atom-count '1 '(2 3 4 5 6 7 8) )

(define lookup ;lookup: atom List --> expression
  (lambda (atomTested atomList)
  (cond [(null? atomList) 'UNKNOWN]
        [(if(eq? atomTested (car (car atomList))) (car (cdr (car atomList))) (lookup atomTested (cdr atomList)))]
        )))

;lookup tests included:
;Included Test: (lookup 'b '((a 34)(b 77)(g 6)) )
;Different atom-type: (lookup 'a '((a "apple")(b "boy")(g "gate")) )
;Not Included Test:(lookup 'notInList '((listA 1)(listB 2)(listC 3)) )
;Empty Test: (lookup 'emptyTest '() )
;Decimal Test: (lookup 'food '((lodging 250.0)(gas 98.60)(food 120.44)) )