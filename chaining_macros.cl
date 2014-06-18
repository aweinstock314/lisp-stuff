#|
; simple macro for chaining functions
(defmacro -> (func1 &rest functions)
	(if functions
		`(-> (,(car functions) ,func1) ,@(cdr functions))
		func1
	)
)
|#
#| example expansion:
(-> 1 sin cos)
(-> (sin 1) cos)
(-> (cos (sin 1)))
(cos (sin 1))
|#

(defun listify (x)
	(if (listp x)
		x
		(list x)
	)
)

; more complicated version of the chaining macro, for passing multiple arguments
(defmacro -> (func1 &rest functions)
	(if functions
		(let ((next (listify (car functions))))
			`(-> (,@next ,func1) ,@(cdr functions))
		)
		func1
	)
)

; distance formula defined using map-reduce and more complicated version of ->
(defun distance (&rest args)
	(sqrt
		(->
			(mapcar (lambda (x) (expt x 2)) args)
			(reduce #'+)
		)
	)
)

;anaphoric progn, named similarly to Paul Graham's aand, alambda, etc...
;executes a series of statements, binding the symbol 'it' to the value of the previous, and returns the value of the last
(defmacro aprogn (&rest body)
	`(let ((it nil))
		,@(mapcar (lambda (stmt) `(setf it ,stmt)) body)
	)
)
;(aprogn 1 (sin it) (cos it)) is equivalent to the above example with ->
