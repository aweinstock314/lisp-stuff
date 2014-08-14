(require <scheme_util_general>)
(require 'list-lib)

(with-all-forms-exported

(java-import java.lang.Number)

(define (clamp lo hi) (lambda (val) (max lo (min hi val))))
(define (wrap lo hi) (lambda (val)
    (cond ((< val lo) hi)
          ((> val hi) lo)
          (else val)
    )
))
(define (within? lo hi) (lambda (val::Number)::boolean (<= lo val hi)))

(define tau (* 8 (atan 1)))
(define atan2 java.lang.Math:atan2)
(define (rad->deg r) (/ (* r 360) tau))
(define (average . nums) (/ (fold + 0 nums) (length nums)))
(define (random x)::float (* x (java.lang.Math:random)))
(define (random-range lo hi)::float (+ lo (* (- hi lo) (java.lang.Math:random))))
(define (box-double x::java.lang.Double)::gnu.math.DFloNum (gnu.math.DFloNum:asDFloNumOrNull x))
(define (parse-double x::String)::gnu.math.DFloNum (box-double (Double:parseDouble x)))

(define (apply-polar-movement x y mag rot) (values
    (+ x (* mag (cos rot)))
    (+ y (* mag (sin rot)))
))

(define (cart->polar v) (receive (x y) v (values (java.lang.Math:sqrt (+ (* x x) (* y y))) (atan2 y x))))
(define (polar->cart p) (receive (m t) p (values (* m (cos t)) (* m (sin t)))))
(define (cart+ v1 v2) (let-values (((x1 y1) v1) ((x2 y2) v2)) (values (+ x1 x2) (+ y1 y2))))

(define (push-outside x y w h)
    (let* ( (cx (average x (+ x w)))
            (cy (average y (+ y h)))
            (hbound? (within? x (+ x w)))
            (vbound? (within? y (+ y h)))
            (epsilon .01)
          )
        (lambda (px py) (receive (m t) (cart->polar (values (- px cx) (- py cy)))
            (do ((tx px (+ tx (* epsilon (cos t))))
                 (ty py (+ ty (* epsilon (sin t)))))
                ((not (and (hbound? tx) (vbound? ty))) (values tx ty))
            )
        ))
    )
)

(define (ensure-proper-angle t)
    (cond ((< t 0) (ensure-proper-angle (+ t tau)))
          ((>= t tau) (ensure-proper-angle (- t tau)))
          (#t t)
    )
)

(define (wpe-helper funcs forms)
    (letrec (
            (evaluatable? (lambda (form)
                ;(returning (tmp
                (if (pair? form)
                    (and (member (car form) funcs)
                         (fold (lambda (e a) (and a (evaluatable? e))) #t (cdr form))
                    )
                    (not (symbol? form))
                )
                ;) (display "in (evaluatable? ") (display form) (display "): ") (display tmp) (newline))
            ))
            (treewalk (lambda (replacer) (lambda (tree)
                ;(display "-----") (newline)
                ;(display tree) (newline)
                ;(display replacer) (newline)
                (aif/nn (replacer tree) it
                    (if (pair? tree)
                        (map (treewalk replacer) tree)
                        tree
                    )
                )
            )))
            (pe-replacer (lambda (tree)
                (if (evaluatable? tree)
                    (eval tree)
                    #!null
                )
            ))
        )
        (map (treewalk pe-replacer) forms)
    )
)
; getting some bytecode verify errors without this indirection, not quite sure why
(define-macro (with-partial-evaluation funcs . forms) (apply wpe-helper funcs forms))

; NOTE: uses 1d-arrays with column-major indexing, since this is to be used with openGL
(define-macro (ct-matrix-mult m n p elttype)
    (define arraytype `($bracket-apply$ ,elttype)) ; this depends on kawa internals, but the type system is already kawa-dependent
    (define (index row col height) (+ (* col height) row))
    (define (emit-matmult-expr r c)
        `(+ ,@(accumulate-range (i 0 n 1)
            `(* (mxn ,(index r i n)) (nxp ,(index i c p)))
        ))
    )
    `(lambda (mxn :: ,arraytype nxp :: ,arraytype)
        (,arraytype length: ,(* m p)
            ,@(with-list-collector col
                (pascal-for (c 0 p 1) (pascal-for (r 0 m 1)
                    (col (emit-matmult-expr r c))
                ))
            )
        )
    )
)

) ; end of with-all-forms-exported
