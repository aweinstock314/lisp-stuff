; take a function and its arity, and return the curried form of it
(define (curry f n)
    (letrec ((curry-step (lambda (acc) (lambda (x)
                (let ((new-acc (cons x acc)))
                    (if (= (length new-acc) n)
                        (apply f (reverse new-acc))
                        (curry-step new-acc)
                    )
                )
            ))))
        (curry-step '())
    )
)

(define (curried-apply f-or-v args)
    (if (null? args)
        f-or-v
        (curried-apply (f-or-v (car args)) (cdr args))
    )
)

(define (curried-funcall f . args) (curried-apply f args))

#|
;example uses
(define +c (curry + 2))
(map (+c 1) '(1 2 3 4 5))
;note how much shorter this is than:
(map (lambda (x) (+ x 1)) '(1 2 3 4 5))
(curried-funcall +c 1 1)
|#
