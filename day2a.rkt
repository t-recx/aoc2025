#lang racket
(require racket/file)

(define (digit-count n)
  (define (digit-count-iter x count)
    (if (< x 10)
        count
        (digit-count-iter (quotient x 10) (+ count 1))))
  (digit-count-iter n 1))

(define (digit-from-right n i)
  (modulo (quotient n (expt 10 i)) 10))

(define (first-number-of-digits d)
  (expt 10 (- d 1)))

(define (is-invalid? n)
  (define (is-invalid-iter n dch i)
    (if (= dch i)
        #t
        (let* ([a (digit-from-right n i)]
               [b (digit-from-right n (+ dch i))])
          (if (not (= a b))
              #f
              (is-invalid-iter n dch (+ i 1))))))
  (is-invalid-iter n (quotient (digit-count n) 2) 0))

(define (get-invalid-ids x y)
  (define (get-invalid-ids-iter x y lst)
    (if (> x y)
        lst
        (let ([dc (digit-count x)])
          (if (= (modulo dc 2) 0)
              (if (is-invalid? x)
                  (get-invalid-ids-iter (+ x 1) y (cons x lst))
                  (get-invalid-ids-iter (+ x 1) y lst))
              (get-invalid-ids-iter (first-number-of-digits (+ dc 1)) y lst)
              ))))
  (get-invalid-ids-iter x y '()))


(define input (
               map (lambda (x)
                     (map string->number (string-split x "-")))
                   (string-split (file->string "day2.txt") ",")))

(apply +
       (apply append
              (map (lambda (x)
                     (get-invalid-ids (car x) (last x)))
                   input)))
