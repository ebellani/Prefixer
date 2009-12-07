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
(require srfi/1) ;; list-index

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


;; infix->prefix-simple : (listof X) symbol -> (listof X) or false
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

;; infix->prefix-composed : (listof X) symbol -> (listof X)
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


;; infix->prefix : (listof X) (listof symbol) -> (listof X)
;; uses infix->prefix-composed to generate a prefix expression
(define (infix->prefix infix-exp separators0)
  (local [(define (infix->prefix-acc result separators)
            (cond
              [(empty? separators) result]
              [else
               (infix->prefix-acc
                (infix->prefix-composed result (first separators))
                (rest separators))]))]
    (infix->prefix-acc infix-exp separators0)))

;; some vanilla infix exp
(check-expect (infix->prefix '(1 + 2 * 3 + 5) standart-operations)
              '(+ 1 (* 2 3) 5))

(check-expect (infix->prefix '(1 + 2 / 2.5 + b - c) standart-operations)
              '(+ 1 (/ 2 2.5) (- b c)))

;; some nested infix exp
(check-expect (infix->prefix '((1 + 2) / 2.5 + b - c) standart-operations)
              '(+ (/ (+ 1 2) 2.5) (- b c)))

(check-expect (infix->prefix '((1 + (55.27 / x)) / 2.5 + b - c)
                             standart-operations)
              '(+ (/ (+ 1 (/ 55.27 x)) 2.5) (- b c)))

;; reducible-expression? : ((listof X) or X) (listof symbol) -> boolean
;; checks if an expression contains only numbers
;; besides the initial operator. Returns false
;; if the expression is not a list
(define (reducible-expression? expression (operators standart-operations))
  (and (list? expression)
       (or (andmap number? expression)
           (and (not (false? (member (first expression)
                                     operators)))
                (andmap number? (rest expression))))))

(check-expect (reducible-expression? (list '+ 1 2 2.4)) true)
(check-expect (reducible-expression? 4) false)
(check-expect (reducible-expression? '(1 2)) true)
(check-expect (reducible-expression? '*) false)
(check-expect (reducible-expression? '(- 1 x 2.4)) false)
(check-expect (reducible-expression? '((+ 1 5) 2)) false)

;; reduce-all : (listof X) -> (listof X)
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
          
          ;; reduce-expression : (listof X) -> (listof X)
          ;; check if there is an operator, if so evaluate
          ;; the expression. If not just return it.
          (define (reduce-expression expression (operators standart-operations))
            (cond
              [(false? (member (first expression)
                               operators)) expression]
              [else (apply
                     (get-function (first expression))
                     (rest expression))]))
          
          (define (reduce expression already-parsed-pieces)
            (cond
              [(not (list? expression)) expression]
              [(empty? expression) already-parsed-pieces]
              [(reducible-expression? expression)
               (reduce-all (append already-parsed-pieces
                                   (reduce-expression expression)))]
              [(reducible-expression? (first expression))
               (reduce-all (append already-parsed-pieces
                                   (list (reduce-expression (first expression)))
                                   (rest expression)))]
              [(list? (first expression))
               (reduce (rest expression)
                       (append already-parsed-pieces
                               (list (reduce (first expression) empty))))]
              [else
               (reduce (rest expression)
                       (append already-parsed-pieces
                               (list (first expression))))]))]
    (reduce initial-expression empty)))

;(cond
;  [(empty? expression) initial-expression]
;  [(reducible-expression? expression)
;   (reduce-expression expression)]
;  [(list? (first expression))
;   (append
;    (reduce
;     (rest expression)
;     (reduce (first expression) already-parsed-pieces)))]
;  [else
;   (reduce (rest expression)
;           (append (list already-parsed-pieces)
;                   (list (first expression))))])

;; simple expressions

;(check-expect (reduce-all '(+ 1 5))
;              6)
;
;(check-expect (reduce-all '(* (+ 1 5) 2)) 12)
;
;(check-expect (reduce-all '(- (/ (+ 1 5) 3) 5)) -3)
;(check-expect (reduce-all '(+ (/ 1 5) 3/25)) 8/25)

;; expressions with variable
;(check-expect (reduce-all '(* (+ 1 5) a))
;              '(* 6 a))

(check-expect (reduce-all '(- (/ (+ 1 5) x) 5))
              '(- (/ 6 x) 5)) 




(test)