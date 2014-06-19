(defmacro with-gensyms (varnames &rest body)
    `(let (,@(mapcar (lambda (varname) `(,varname (gensym))) varnames))
        ,@body
    )
)

; form of destructuring bind that allows an else clause if the pattern don't match the tree (instead of throwing an error)
(defmacro destructure-if (pattern tree then-clauses else-clauses)
	(with-gensyms (condition-name)
		`(handler-case
			(destructuring-bind ,pattern ,tree ,@then-clauses)
			(error (,condition-name) (declare (ignore ,condition-name)) ; catch the error thrown by destructuring-bind if there's no match
			,@else-clauses)
		)
	)
)
#|
; sample uses
(destructure-if (x y) '(1 2) ((list y x)) ("Didn't match"))
(destructure-if (x y) '(1 2 3) ((list y x)) ("Didn't match"))
|#

(defmacro destructure-cond ((firstpattern firsttree &rest firstbody) &rest other-clauses)
	(let (
			(else-clauses
				(if other-clauses
					`((destructure-cond ,@other-clauses))
					'((error "Fell off the end of destructure-cond."))
				)
			)
		)
		`(destructure-if ,firstpattern ,firsttree ,firstbody ,else-clauses)
	)
)
#|
; sample uses
(let ((a '(1 2 3))) (destructure-cond ((x y) a (list x y)) ((x y z) a (list x y z))))
|#

(defmacro destructure-case (tree &rest bodies)
;	(princ (mapcar (lambda (first-body &rest rest-bodies) (declare (ignore rest-bodies)) `(,(car first-body) ,tree ,@(cdr first-body))) bodies)) (terpri)
	(with-gensyms (tree-val) ; to ensure single-evaluation
		`(let ((,tree-val ,tree))
			(destructure-cond
				,@(mapcar
					(lambda (first-body &rest rest-bodies)
						(declare (ignore rest-bodies))
						`(,(car first-body) ,tree-val ,@(cdr first-body))
					)
					bodies
				)
			)
		)
	)
)

#|
(destructure-case '(1 2 3)
	((x y) "Shouldn't match")
	((x y z) (with-output-to-string (format t "Got input '(~a ~a ~a)." x y z)))
)

(destructure-case '(1 2 3) ((x y) (list x y)) ((x y z) (list x y z)))
|#

(defmacro defvariadic (func-name &rest bodies)
	(with-gensyms (args-name)
		`(defun ,func-name (&rest ,args-name)
			(destructure-case ,args-name ,@bodies)
		)
	)
)



; simple example, implementing variadic plus given only binary plus-pair ( (lambda (&rest args) (reduce #'plus-pair args)) is a better way )
(defun plus-pair (x y) (+ x y))

(defvariadic plus
	(() 0)
	((x) x)
	((&rest args) (plus-pair (car args) (apply #'plus (cdr args))))
)
