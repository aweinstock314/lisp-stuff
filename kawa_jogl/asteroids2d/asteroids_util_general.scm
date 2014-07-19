(require 'list-lib)

(define-macro (with-all-forms-exported . forms)
    (letrec ((helper (lambda (form acc)
                (if (not (list? form)) acc
                (case (car form)
                    ((define-alias define-simple-class) (cons (cadr form) acc))
                    ((define define-constant define-macro)
                        (cons (cond
                            ((pair? (cadr form)) (caadr form))
                            ((symbol? (cadr form)) (cadr form))
                            (else (error "Unexpected type as the second element of a define form"))
                        ) acc)
                    )
                    (else acc)
                ))
            )))
        (define export-list (fold helper '() forms))
        (display "Automagically exporting the following from with-all-forms-exported: ") (newline)
        (display export-list) (newline)
        `(begin ,@forms ,@(if (not (null? export-list)) `((module-export ,@export-list)) '()))
    )
)
(module-export with-all-forms-exported)

(with-all-forms-exported

(define printf java.lang.System:out:printf)
(define-alias ArrayList java.util.ArrayList)
(define-alias Integer java.lang.Integer)

(define-macro (thunk . body) `(lambda (. ,(gentemp)) ,@body))
;(define-macro (mvlist expr) `(call-with-values (thunk ,expr) list))
(define-macro (returning let-pair . body) `(let (,let-pair) ,@body ,(car let-pair)))
(define-macro (set!* vals exprs) `(begin ,@(map (lambda (val expr) `(set! ,val ,expr)) vals exprs)))
(define-macro (define-gensyms . names) `(begin ,@(map (lambda (name) `(define ,name (gentemp))) names)))
(define-macro (set-values! vars expr)
    (define tmps (map (thunk (gentemp)) vars))
    `(receive ,tmps ,expr ,@(map (lambda (var tmp) `(set! ,var ,tmp)) vars tmps))
)
(define-macro (define-values vars expr)
    `(begin
        ,@(map (lambda (var) `(define ,var #!null)) vars)
        (set-values! ,vars ,expr)
    )
)

; the first way has multiple-evaluation problems, but works fine for simple cases
; the second way (commented), should be the right way, but generates bytecode that sometimes gives verify errors (revision 7952 fixed a simplified version of this error, which turned out to have been oversimplified)
(define-macro (inc! var delta)
    `(set! ,var (+ ,var ,delta))
    ;(let ((loc (gentemp))) `(let ((,loc (location ,var))) (set! (,loc) (+ (,loc) ,delta))))
)
(define-macro (inplace! fn var)
    `(set! ,var (,fn ,var))
    ;(let ((loc (gentemp))) `(let ((,loc (location ,var))) (set! (,loc) (,fn (,loc)))))
)

(define-macro (java-iterate iterable-expr varname . body)
    (define real-varname (if (list? varname) (car varname) varname))
    (define vartype (if (list? varname) (cadr varname) java.lang.Object))
    (define iterator-name (if (and (list? varname) (>= (length varname) 3)) (caddr varname) (gentemp)))
    `(let ((,iterator-name ::java.util.Iterator (invoke ,iterable-expr 'iterator))
           (,real-varname ::,vartype #!null))
        (do ()
            ((not (invoke ,iterator-name 'hasNext)) #!void)
            (set! ,real-varname (invoke ,iterator-name 'next))
            ,@body
        )
    )
)

(define-macro (while test . body)
    (define-gensyms loop)
    `(let ,loop ()
        (when ,test ,@body (,loop))
    )
)

(define-macro (pascal-for rng . body)
    (define var (car rng))
    (define-gensyms lo hi step)
    `(let ((,lo ,(cadr rng)) (,hi ,(caddr rng)) (,step ,(cadddr rng)))
        (do ((,var ,lo (+ ,var ,step)))
            ((= ,var ,hi) #!void)
            ,@body
        )
    )
)

(define-macro (with-list-collector col . body)
    (define-gensyms head tail elem)
    `(let* ((,head (cons '() '())) (,tail ,head)
            (,col (lambda (,elem)
                (set! (cdr ,tail) (cons ,elem (cdr ,tail)))
                (inplace! cdr ,tail)
           )))
        ,@body
        (cdr ,head)
    )
)

(define-macro (accumulate-range rng . body)
    (define-gensyms col)
    `(with-list-collector ,col
        (pascal-for ,rng
            (,col (begin ,@body))
        )
    )
)

(define-macro (with-min-ms-per-iteration millis . body)
    (define-gensyms starttime endtime delta)
    (define gettime `(invoke-static java.lang.System 'currentTimeMillis))
    `(let ((,starttime 0) (,endtime 0) (,delta 0))
        (while #t
            (set! ,starttime ,gettime)
            ,@body
            (set! ,endtime ,gettime)
            (set! ,delta (max 0 (- ,millis (- ,endtime ,starttime))))
            (if (> ,delta 0) (invoke-static java.lang.Thread 'sleep ,delta))
        )
    )
)

(define (slurp-file filename::String)::String
    (define buf (java.lang.StringBuilder))
    (define file (open-input-file filename))
    (do ((line (read-line file 'concat) (read-line file 'concat)))
        ((equal? line '#!eof) (buf:toString))
        (buf:append line)
    )
)

; Would be nice to have CL's sharp-dot read-macro for this (so the callsite is just #.(slurp-file "foo"), and this extra macro is unneccessary)
(define-macro (file-as-string-constant filename) (slurp-file filename))

(define-macro (curry expr)
    (define-gensyms argsname)
    `(lambda (. ,argsname) (apply ,@expr ,argsname))
)
(define (complement pred) (lambda (. args) (not (apply pred args))))
(define (compose f g) (lambda (. args) (f (apply g args))))
(define (upto x) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i x) (reverse! acc))))
(define (constantly x) (lambda (. args) x))

(define (values-map f v) (gnu.mapping.Values:make (vector-map f (vector (gnu.mapping.Values:getValues v)))))

(define (ArrayList-map fn . arraylists)
    (returning (rv ::ArrayList (ArrayList))
        (define endidx (fold min Integer:MAX_VALUE (map (lambda (x::ArrayList) (x:size)) arraylists)))
        (pascal-for (idx 0 endidx 1)
            (rv:add (apply fn (map (lambda (x::ArrayList) (x:get idx)) arraylists)))
        )
    )
)

(define (ArrayList-foldl fn al::ArrayList initial)
    (returning (acc initial) (pascal-for (i 0 (al:size) 1)
        (set! acc (fn acc (al:get i)))
    ))
)

) ; of end with-all-forms-exported
