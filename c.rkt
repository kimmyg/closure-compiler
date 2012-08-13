#lang racket

(define empty-set (list))

(define (singleton x)
  (list x))

(define (set-add s x)
  (if (empty? s)
      (singleton x)
      (let ((x* (first s)))
        (if (eq? x x*)
            s
            (cons x* (set-add (rest s) x))))))

(define (set-union s t)
  (if (empty? s)
      t
      (set-union (rest s) (set-add t (first s)))))

(define (set-remove s x)
  (if (empty? s)
      s
      (let ((x* (first s)))
        (if (eq? x x*)
            (rest s)
            (cons x* (set-remove (rest s) x))))))

(define (set-difference s t)
  (if (empty? t)
      s
      (set-difference (set-remove s (first t)) (rest t))))

(define globals '(+))

(define (free-vars e)
  (set-difference
   (match e
     [(list 'λ (list xs ...) e1)
      (set-difference (free-vars e1) xs)]
     [(list x xs ...)
      (set-union (free-vars x) (free-vars xs))]
     [(list)
      empty-set]
     [(? symbol? x)
      (singleton x)]
     [(? number? x)
      empty-set])
   globals))

(define (compile e)
  (match e
    [(list)]))

(define a \x.\.x+5)

(display ((\x.\.x+5 3)))





