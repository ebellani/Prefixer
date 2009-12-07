#!/usr/bin/mzscheme
#lang scheme
;;;; Scheme infix/prefix conversion utility
;;;
;;;; Licence LGPL
;;;
;;;; Copyright: Eduardo Bellani (ebellani at gmail.com)
;;;
;;; based on the work of 
;;; Joern Inge Vestgaarden (jivestgarden at gmail com)
;;; http://folk.uio.no/jornv/infpre/infpre.html
;;; 

(require test-engine/scheme-tests)
(require srfi/1) ;; for list-index

;; Default operators
(define standart-operations (list '+ '- '* '/))


;; remove-parens : (listof X) -> X
;; if there is only one element on the list
;; return it, else return the list
(define (remove-parens a-list)
  (cond
    [(not (list? a-list)) a-list]
    [(or (empty? a-list)
         (not (empty? (rest a-list))))
     a-list]
    [else
     (first a-list)]))

(check-expect (remove-parens (list 1))
              1)

(check-expect (remove-parens '(x))
              'x)

(check-expect (remove-parens '(x 1 b))
              '(x 1 b))


;; infix->prefix-simple : infix-expression symbol -> (listof X) or false
;; Returns list of sub-sequences defined by separator,
;; Ex: (x + 1 - b * c) breaked by the * symbol should be
;; (* (x + 1 - b) c)
(define (infix->prefix-simple initial-infix-expression separator)
  (local [(define (separate-list infix-expression result)
            (let ([position (list-index (λ (symbol)
                                          (equal? symbol separator))
                                        infix-expression)])
              (cond
                [(empty? infix-expression) result]
                [(false? position) (append result (list infix-expression))]
                [else
                 (separate-list
                  (list-tail infix-expression
                             (add1 position))
                  (append result
                          (list (take infix-expression position))))])))]
    (cond
      [(or (not (list? initial-infix-expression)) (empty? initial-infix-expression))
       initial-infix-expression]
      [else
       (let ([initial-list
              (cond [(equal? (first initial-infix-expression) separator) 
                     (rest initial-infix-expression)]
                    [else initial-infix-expression])])
         (map remove-parens
              (separate-list initial-list
                             (list separator))))])))


(check-expect (infix->prefix-simple '(x + 1 - b * c) '*)
              '(* (x + 1 - b) c))

(check-expect (infix->prefix-simple '(x + 1 - b * c) '-)
              '(- (x + 1) (b * c)))

(check-expect (infix->prefix-simple '(x + 1 - b * c) '+)
              '(+ x (1 - b * c)))

(check-expect (infix->prefix-simple '(x + 1 - 4) '-)
              '(- (x + 1) 4))

(check-expect (infix->prefix-simple 4 '-)
              4)

(check-expect (infix->prefix-simple '(1 + 1) '-)
              '(- (1 + 1)))

;; infix->prefix-composed : infix-expression symbol -> prefix-expression
;; applies infix->prefix-simple to every sublist
(define (infix->prefix-composed initial-infix-expression separator)
  (cond
    [(or (not (list? initial-infix-expression))
         (empty? initial-infix-expression))]
    [else
     (let ([infix-expression (map (λ (sublist-or-atom)
                                    (cond
                                      [(list? sublist-or-atom)
                                       (infix->prefix-composed sublist-or-atom
                                                               separator)]
                                      [else sublist-or-atom]))
                                  initial-infix-expression)])
       (cond
         [(false? (member separator (rest infix-expression))) infix-expression]
         [else (infix->prefix-simple infix-expression separator)]))]))


(check-expect (infix->prefix-composed '(x + (1 - 4)) '-)
              '(x + (- 1 4)))

(check-expect (infix->prefix-composed '(x + (1 - (2 - 5))) '-)
              '(x + (- 1 (- 2 5))))

(check-expect (infix->prefix-composed '(x + 1 - b * c) '*)
              '(* (x + 1 - b) c))

(check-expect (infix->prefix-composed '(x + 1 - b * c) '-)
              '(- (x + 1) (b * c)))

(check-expect (infix->prefix-composed '(x + 1 - b * c) '+)
              '(+ x (1 - b * c)))


;; infix->prefix : infix-expression (listof symbol) -> prefix-expression
;; uses infix->prefix-composed to generate a prefix expression
(define (infix->prefix infix-exp (separators0 standart-operations))
  (local [(define (infix->prefix-acc result separators)
            (cond
              [(empty? separators) result]
              [else
               (infix->prefix-acc
                (infix->prefix-composed result (first separators))
                (rest separators))]))]
    (infix->prefix-acc infix-exp separators0)))

;; some vanilla infix exp
(check-expect (infix->prefix '(1 + 2 * 3 + 5))
              '(+ 1 (* 2 3) 5))

(check-expect (infix->prefix '(1 + 2 / 2.5 + b - c))
              '(+ 1 (/ 2 2.5) (- b c)))

;; some nested infix exp
(check-expect (infix->prefix '((1 + 2) / 2.5 + b - c))
              '(+ (/ (+ 1 2) 2.5) (- b c)))

(check-expect (infix->prefix '((1 + (55.27 / x)) / 2.5 + b - c))
              '(+ (/ (+ 1 (/ 55.27 x)) 2.5) (- b c)))



;; reduce-all : prefix-expression -> prefix-expression or number
;; Reduce all expressions as much as possible.
;; Ex: (* (+ 1 5) 2) -> 12
;;     (+ (* 3 4) b) -> (+ 12 b)
(define (reduce-all initial-expression)
  (local [;; get-function : symbol -> (X..N -> X)
          (define (get-function symbol)
            (cond
              [(equal? symbol '*) *]
              [(equal? symbol '+) +]
              [(equal? symbol '/) /]
              [(equal? symbol '-) -]))
          
          ;; reducible-expression? : (prefix-expression or number) 
          ;;                         (listof symbol) -> boolean
          ;; checks if an expression contains only numbers
          ;; besides the initial operator. Returns false
          ;; if the expression is not a list
          (define (reducible-expression? expression (operators standart-operations))
            (and (list? expression)
                 (and (not (false? (member (first expression)
                                           operators)))
                      (andmap number? (rest expression)))))
          
          ;; reduce-expression : prefix-expression -> prefix-expression or number
          ;; check if there is an operator, if so evaluate
          ;; the expression. If not just return it.
          (define (reduce-expression expression (operators standart-operations))
            (cond
              [(false? (member (first expression)
                               operators)) expression]
              [else (apply
                     (get-function (first expression))
                     (rest expression))]))
          
          ;; check-for-termination : prefix-expression -> prefix-expression or number
          ;; verifies if a reduced exp is different than the initial
          ;; if it is continue with the reduction. Else stop.
          (define (check-for-termination expression)
            (cond
              [(equal? expression initial-expression) expression]
              [else (reduce-all expression)]))
          
          (define (reduce expression already-parsed-pieces)
            (cond
              [(not (list? expression)) expression]
              [(empty? expression) already-parsed-pieces]
              [(reducible-expression? expression)
               (check-for-termination (append already-parsed-pieces
                                              (reduce-expression expression)))]
              [(list? (first expression))
               (check-for-termination
                (append already-parsed-pieces
                        (list (reduce (first expression) empty))
                        (rest expression)))]
              [else
               (reduce (rest expression)
                       (append already-parsed-pieces
                               (list (first expression))))]))]
    (reduce initial-expression empty)))

;; simple expressions

(check-expect (reduce-all 8)
              8)

(check-expect (reduce-all empty)
              empty)

(check-expect (reduce-all '(+ 1 5))
              6)

(check-expect (reduce-all '(* (+ 1 5) 2)) 12)

(check-expect (reduce-all '(- (/ (+ 1 5) 3) 5)) -3)
(check-expect (reduce-all '(+ (/ 1 5) 3/25)) 8/25)

;; expressions with variables
(check-expect (reduce-all '(* (+ 1 5) a))
              '(* 6 a))

(check-expect (reduce-all '(- (/ 6 x) 5))
              '(- (/ 6 x) 5)) 

(check-expect (reduce-all '(+ (/ 1 5) (* (+ 1 5) x)))
              '(+ 1/5 (* 6 x)))

;; string->infix-expression : string -> infix-expression
;; takes a string and reads it into an infix expression
;; Ex: "1 + (2 * 4)" -> (1 + (2 * 4))
(define (string->infix-expression the-string)
  (cond
    [(or (not (equal? (string-ref the-string 0) #\())
         (not (equal? (string-ref the-string
                                  (sub1 (string-length the-string))) #\))))
         (read (open-input-string (string-append "(" the-string ")")))]
    [else
     (read (open-input-string the-string))]))

(check-expect (string->infix-expression "1 + (2 * 4)")
              '(1 + (2 * 4)))

(check-expect (string->infix-expression "1 + 1")
              '(1 + 1))


(define (main (parameters (current-command-line-arguments)))
  (let ([prefix-expression
         (infix->prefix
          (string->infix-expression
           (read-line (open-input-file (vector-ref parameters 0)))))])
    (cond
      [(equal? (vector-ref parameters (sub1 (vector-length parameters))) "-r")
       (display (reduce-all prefix-expression))
       (newline)]
      [else
       (display prefix-expression)
       (newline)])))

;; should be 3/2
(check-expect (main #("expression-without-letters.txt" "-r"))
              (void))

;; should be (/ (+ 1 2) 2)
(check-expect (main #("expression-without-letters.txt"))
              (void))

;; should be (+ 4.4 (- b c))
(check-expect (main #("expression-with-letters.txt" "-r"))
              (void))

;; should be (+ (/ (+ 1 (/ 40 4)) 2.5) (- b c))
(check-expect (main #("expression-with-letters.txt"))
              (void))
(main)
;(test)