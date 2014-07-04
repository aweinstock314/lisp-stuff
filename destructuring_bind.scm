(define (foldl fn lst acc)
    (if (null? lst) acc
        (foldl fn (cdr lst) (fn acc (car lst)))
    )
)

; (make-cxr '(a d)) acts like cadr
(define (make-cxr path)
    (lambda (lst)
        (foldl (lambda (acc elem)
            (cond ((eqv? elem 'a) (car acc))
                  ((eqv? elem 'd) (cdr acc))
                  (else (error "element of path was not an a or d"))
            )
        ) (reverse path) lst)
    )
)

(define (replace-leaves-with-getters tree)
    (letrec ((aux (lambda (subtree path)
                (cond ((null? subtree) '())
                      ((pair? subtree) (cons
                          (aux (car subtree) (cons 'a path))
                          (aux (cdr subtree) (cons 'd path))
                      ))
                      (else (make-cxr path))
                )
            )))
        (aux tree '())
    )
)

(define (flatten-tree tree)
    (if (pair? tree)
        (apply append (map flatten-tree tree))
        (list tree)
    )
)
;; example fragment:
;(define x '(((a) b) c (d e)))
;(define xgetters (flatten-tree (replace-leaves-with-getters x)))
;(map (lambda (f) (f x)) xgetters)


(define (destructuring-bind-runtime pattern val fn)
    (let* ((getters (flatten-tree (replace-leaves-with-getters pattern))))
        (apply fn (map (lambda (getter) (getter val)) getters))
    )
)

; example: (destructuring-bind-runtime '((a) b) '((1) 2) (lambda (a b) (list a b)))

