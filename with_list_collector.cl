#|
(defun func1 (x)
	(loop for i from 1 to x
		collect i
	)
)

(defun func2 (x)
	(with-list-collector col
		(dotimes (i x)
			(col (+ i 1))
		)
	)
)
|#

; first implementation, uses macrolet to do the push/nreverse idiom
(defmacro with-list-collector1 (colname &rest body)
	(let ((listname (gensym)) (argname (gensym)))
		`(let ((,listname nil))
			(macrolet 
				((,colname (,argname)
					`(push ,,argname  ,',listname)
				))
				,@body
				(nreverse ,listname)
			)
		)
	)
)

; second implementation, expands into a more complicated form in order to only make one pass through the list (keep track of head and current tail, setf the tail)
(defmacro with-list-collector2 (colname &rest body)
	(let ((headname (gensym)) (tailname (gensym)) (argname (gensym)))
		`(let ((,headname nil) (,tailname nil))
			(macrolet 
				((,colname (,argname)
					`(if (eq ,',headname nil)
						(progn
							(setf ,',headname (cons ,,argname nil))
							(setf ,',tailname ,',headname)
						)
						(progn
							(setf (cdr ,',tailname) (cons ,,argname nil))
							(setf ,',tailname (cdr ,', tailname))
						)
					)
				))
				,@body
				,headname
			)
		)
	)
)

; returns a closure that, when called with one argument, accumulates to the end of a list, and when called with no arguments, returns a copy of the current accumulator
(defun list-collector ()
	(let (head tail)
		#'(lambda (&optional (elem nil elem-supplied))
			(if elem-supplied
				(if head
					(progn
						(setf (cdr tail) (cons elem nil))
						(setf tail (cdr tail))
					)
					(setf head (setf tail (cons elem nil)))
				)
				(copy-list head) ; return a copy so that (let ((a (list-collector))) (funcall a 1) (funcall a (funcall a)) (funcall a)) returns (1 (1)) rather than #1=(1 #1#)
			)
		)
	)
)

; third implementation, uses a closure to encapsulate the one-pass append, and uses flet instead of macrolet to allow passing the collector function as an argument
(defmacro with-list-collector (colname &rest body)
	(let ((lcname (gensym)))
		`(let ((,lcname (list-collector)))
			(flet ((,colname (x) (funcall ,lcname x))) ; don't need a gensym for x, since users' code won't be in that scope
				,@body
			)
			(funcall ,lcname)
		)
	)
)
