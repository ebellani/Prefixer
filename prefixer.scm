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
(define separators (list '+ '- '* '/))


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
  $expr$ ---)

(defun separate-tree (lst separator test)
  "Apply separate-list on all sublists"
  (if (or (not (consp lst)) (eql (first lst) 'quote))
      lst
      (progn
       (setf lst (mapcar #'(lambda (x) 
                             (if (not (consp x))
                                 x
                                 (separate-tree x separator test)))
                         lst))
       (if (not (find separator (rest lst)))
           lst
           (separate-list lst separator test)))))


(test)