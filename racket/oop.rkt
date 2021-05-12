#lang racket

(define super-dispatch (list "super-dispatch"))

(define (top . message)
  (error "Unknown message: ~s" message))

(define-syntax object
  (syntax-rules (inherit method)
    ((_ (self-var)
        (method (name args ...)
          body
          ...)
        ...)
     (object (self-var)
       (inherit top top)
       (method (name args ...)
          body
          ...)
        ...))
    ((_ (self-var)
        (inherit super-var super-init)
        (method (name args ...)
          body
          ...)
        ...)
     (let ((super-var super-init))
       (letrec ((dispatch
                 (lambda (self-var message)
                   (define (self-dispatch message)
                     (match message
                       ((list 'name args ...)
                        body
                        ...)
                       ...
                       (else
                        (super-var super-dispatch self-var message))))
                   (match message
                     ((list special actual-object actual-message)
                      (if (eq? special super-dispatch)
                          (dispatch actual-object actual-message)
                          (self-dispatch message)))
                     (else (self-dispatch message))))))
         (define (self . message)
           (dispatch self message))
         self)))))



(define (2d-point label x y)
  (object (self)
    
    (method (label) label)
    (method (x) x)
    (method (y) y)
    
    (method (move-to new-x new-y)
     (set! x new-x)
     (set! y new-y))
    
    (method (format)
      (format "~a(~a, ~a)" label x y))
    
    (method (println)
      (printf "~a\n" (self 'format)))))



(define (3d-point label x y z)
  (object (self)
    (inherit super (2d-point label x y))

    (method (z) z)

    (method (move-to new-x new-y new-z)
      (super 'move-to new-x new-y)
      (set! z new-z))

    (method (format)
      (format "~a(~a, ~a, ~a)" label (self 'x) (self 'y) z))))



(define (main)
  (define a (2d-point 'A 1 2))
  (define b (2d-point 'B -3 -4))
  
  (define c (3d-point 'C 42 -9 12))
  
  (a 'println)
  (b 'println)
  (a 'move-to 5.6 7.8)
  (a 'println)
  (printf "Point ~a is on x: ~a, y: ~a\n"
          (b 'label) (b 'x) (b 'y))

  (c 'println)
  (c 'move-to 1 2 3)
  (c 'println))











