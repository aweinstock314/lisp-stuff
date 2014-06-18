; take a function and its arity, and return the curried form of it
(defun curry (f n)
    (labels ((curry-step (acc) (lambda (x)
                (let ((new-acc (cons x acc)))
                    (if (= (length new-acc) n)
                        (apply f (reverse new-acc))
                        (curry-step new-acc)
                    )
                )
            )))
        (curry-step '())
    )
)

(defun curried-apply (f-or-v args)
    (cond ((not args) f-or-v)
          ((functionp f-or-v) (curried-apply (funcall f-or-v (car args)) (cdr args)))
          (t (error "Too many arguments to curried function"))
    )
)

(defun curried-funcall (f &rest args) (curried-apply f args))

(defmacro defcurry (name args &rest body)
    (let ((internal-name (gensym)))
        `(let ((,internal-name (lambda ,args ,@body)))
                (defun ,name (&rest args)
                    (curried-apply (curry ,internal-name ,(length args)) args)
                )
        )
    )
)

#|
;example uses
(defcurry +c (a b) (+ a b))
(mapcar (+c 1) '(1 2 3 4 5))
;note how much shorter this is than:
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))
(+c 1 1)
|#
