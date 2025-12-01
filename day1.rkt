#lang racket
(require racket/file)

(define (split-instruction s)
  (define first-char (string-ref s 0))
  (define rest-number (string->number (substring s 1)))
  (cons first-char rest-number))

(define (get-increase c n)
  (if (char=? c #\L)
      (- n)
      n))

(define (get-password lst n value new-value-fn)
  (if (null? lst)
      value
      (let* ((res (split-instruction (car lst)))
             (type (car res))
             (times (cdr res))
             (increase (get-increase type times))
             (increased-n (+ n increase))
             (recomputed-n (modulo increased-n 100))
             (new-value (+ value (new-value-fn n recomputed-n increase))))       
        (get-password (cdr lst) recomputed-n new-value new-value-fn))))

(define input (file->lines "day1.txt"))

; Part A
(get-password input 50 0 (lambda (n rn i) (if (= rn 0) 1 0)))

; Part B
(define (count-times-at-zero n rn i)
  (if (>= i 0)
      (quotient (+ n i) 100)
      (quotient (+ (modulo (- 100 n) 100) (abs i)) 100)))

(get-password input 50 0 count-times-at-zero)