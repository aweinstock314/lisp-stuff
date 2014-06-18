#|
#define SWAP(T,A,B) {T tmp = A; A = B; B = tmp;}
|#


(defmacro with-gensyms (varnames &rest body)
	`(let (,@(mapcar (lambda (varname) `(,varname (gensym))) varnames))
		,@body
	)
)

(defmacro for (bindings test update &rest body)
	(with-gensyms (start end)
		`(let ,bindings
			(tagbody
				,start
				(unless ,test (go ,end))
				,@body
				,update
				(go ,start)
				,end
			)
		)
	)
)

#|
template <class T> void percolate_down(T array, size_t size, size_t element)
{
	const size_t lchild = 2*element + 1;
	const size_t rchild = 2*element + 2;
	if(lchild >= size)return;
	size_t max_child = lchild;
	if((rchild < size) && (array[rchild] > array[lchild]))max_child = rchild;
	if(array[max_child] > array[element])
	{
		std::swap(array[max_child],array[element]);
		percolate_down(array,size,max_child);
	}
}
|#
(defun percolate-down (arr size elem)
	(let ((lchild (+ (* 2 elem) 1)) (rchild (+ (* 2 elem) 2)) max-child)
		(unless (>= lchild size)
			(setf max-child
				(if (and (< rchild size) (> (aref arr rchild) (aref arr lchild)))
					rchild
					lchild
				)
			)
			(when (> (aref arr max-child) (aref arr elem))
				(rotatef (aref arr max-child) (aref arr elem))
				(percolate-down arr size max-child)
			)
		)
	)
)

#|
template <class T> void make_bin_heap(T array[], size_t size)
{
	for(size_t i=size; ((int)i)>=0; i--)
		percolate_down(array,size,i);
}
|#
(defun make-bin-heap (arr size)
	(for ((i size)) (>= i 0) (decf i)
		(percolate-down arr size i)
	)
)

#|
template <class T> T bin_heap_extract_max(T array[], size_t size)
{
	T largest = array[0];
	std::swap(array[0],array[size-1]);
	percolate_down(array,size-1,0);
	return largest;
}
|#
(defun bin-heap-extract-max (arr size)
	(let ((largest (aref arr 0)))
		(rotatef (aref arr 0) (aref arr (- size 1)))
		(percolate-down arr (- size 1) 0)
		largest
	)
)

#|
template <class T> void heapsort(T array[], size_t size)
{
	make_bin_heap(array,size);
	for(size_t i=size; i > 0; i--)
	{
		//no need to use the return value since extract_max swaps the root out of
		//the portion of the array that represents the heap
		bin_heap_extract_max(array,i);
	}
}
|#
(defun heapsort (arr size)
	(make-bin-heap arr size)
	(for ((i size)) (> i 0) (decf i)
		(bin-heap-extract-max arr i)
	)
)

#|
int* make_random_int_array(size_t size, int maxval)
{
	int* tmp = new int[size];
	for(size_t i=0;i<size;i++)tmp[i] = rand() % maxval;
	return tmp;
}
|#
(defun make-random-int-array (size maxval)
	(let ((tmp (make-array size)))
		(dotimes (i size)
			(setf (aref tmp i) (random maxval))
		)
		tmp
	)
)
