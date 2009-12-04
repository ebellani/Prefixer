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
(define (infix->prefix-simple a-list0 separator)
  (local [(define (separate-list a-list result)
            (let ([position (list-index (λ (symbol)
                                          (equal? symbol separator))
                                        a-list)])
              (cond
                [(empty? a-list) result]
                [(false? position) (append result (list a-list))]
                [else
                 (separate-list
                  (list-tail a-list
                             (add1 position))
                  (append result
                          (list (take a-list position))))])))]
    (cond
      [(or (not (list? a-list0)) (empty? a-list0))
       a-list0]
      [else
       (let ([initial-list
              (cond [(equal? (first a-list0) separator) (rest a-list0)]
                    [else a-list0])])
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
(define (infix->prefix-composed a-list0 separator)
  (cond
    [(or (not (list? a-list0))
         (empty? a-list0))]
    [else
     (let ([a-list (map (λ (sublist-or-atom)
                          (cond
                            [(list? sublist-or-atom)
                             (infix->prefix-composed sublist-or-atom
                                                     separator)]
                            [else sublist-or-atom]))
                        a-list0)])
       (cond
         [(false? (member separator (rest a-list))) a-list]
         [else (infix->prefix-simple a-list separator)]))]))


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

(test)