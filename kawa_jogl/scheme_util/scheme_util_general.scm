(require 'list-lib)

; hack to get an effect similar to CL's (eval-when (:COMPILE-TOPLEVEL))
(define-macro (shortened-java-name)
    `(lambda (fullsym)
        (define str (symbol->string fullsym))
        (string->symbol (str:substring (+ 1 (str:lastIndexOf "."))))
    )
)

(define-macro (join-strings-form)
    '(lambda (sep::String #!rest strs::String[]) 
        (do ((acc::java.lang.StringBuilder (java.lang.StringBuilder) acc) (i 0 (+ i 1)))
            (#1=(equal? i (- strs:length 1)) #2=(acc:append (strs i)) (acc:toString))
            #2# (if (not #1#) (acc:append sep))
        )
    )
)

(define-macro (make-symbol-form)
    '(lambda (str . strs) (symbol (apply (join-strings-form) "" (cons str strs))))
)

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
                    ((java-import) (append (map (shortened-java-name) (cdr form)) acc))
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

(define join-strings (join-strings-form))

(define make-symbol (make-symbol-form))

(define (java-imports-aux . paths-and-classes)
    (define (make-imports prefix classnames)
        (map (lambda (name)
            (let ((classname ((make-symbol-form) (symbol->string prefix) "." (symbol->string name))))
                `(define-alias ,((shortened-java-name) classname) ,classname)
            )
        ) classnames)
    )
    `(begin ,@(apply append (map (lambda (path-and-classes)
        (make-imports (car path-and-classes) (cdr path-and-classes))
    ) paths-and-classes)))
)
(define-macro java-imports java-imports-aux)

(define-macro (java-import . classnames)
    (define (make-import classname)
        `(define-alias ,((shortened-java-name) classname) ,classname)
    )
    `(begin ,@(map make-import classnames))
)

(define printf java.lang.System:out:printf)
(define get-time java.lang.System:nanoTime)
(java-import java.util.ArrayList java.lang.Integer java.lang.Double)

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
(define-macro (with-output-to-string . body)
    (define port (gentemp))
    `(call-with-output-string (lambda (,port)
        (parameterize ((current-output-port ,port))
            ,@body
        )
    ))
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
        (do ((,var ::integer ,lo (+ ,var ,step)))
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

(define-macro (print-exceptions . body)
    `(try-catch
        (begin ,@body)
        (e java.lang.Exception (invoke e 'printStackTrace) #f)
    )
)

(define-macro (aif/nn test then-clause #!optional (else-clause #!void))
    (define-gensyms tmp)
    `(let ((,tmp ,test))
        (if (not (equal? ,tmp #!null))
            (let ((it ,tmp)) ,then-clause)
            ,else-clause
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

(define-macro (cxr-with-default path default)
    (define (make-accessor sym)
        (cond ((eqv? sym 'a) 'car)
              ((eqv? sym 'd) 'cdr)
              (else (error "element of path was not an a or d"))
        )
    )
    (define (step-expander path-remainder)
        (if (null? path-remainder)
            `(lambda (x) x)
            `(lambda (lst)
                (if (pair? lst)
                    (,(step-expander (cdr path-remainder)) (,(make-accessor (car path-remainder)) lst))
                    ,default
                )
            )
        )
    )
    (step-expander (reverse path))
)

; less general than CL's, due to not taking more than one list
(define (maplist f lst)
    (with-list-collector col
        (do ((l lst (cdr l)))
            ((null? l) '())
            (col (f l))
        )
    )
)

; Intended usage: (set-variables-from-cmdline (varname default (flagname*) converter?)*)
(define (set-variables-from-cmdline-aux immediates . optsets)
    (let* (
            (opt-g (gentemp)) (name-g (gentemp)) (arg-g (gentemp)) (seeds-g (gentemp))
            (varnames (map car optsets))
            (defaults (map cadr optsets))
            (flagnames-es (map caddr optsets))
            (converters (map (cxr-with-default (a d d d) `(lambda (y) y)) optsets))
            (immediate-flagnames-es (map car immediates))
            (immediate-bodies (map cdr immediates))
            (code-to-set-defaults (map (lambda (varname default) `(define ,varname ,default)) varnames defaults))
            (setter-options (map
                (lambda (varname flagnames converter)
                    `(option ',flagnames #t #f
                        (lambda (,opt-g ,name-g ,arg-g . ,seeds-g)
                            (try-catch (set! ,varname (,converter ,arg-g))
                            (e java.lang.Exception #!void))
                        )
                    )
                ) varnames flagnames-es converters
            ))
            (immediate-options (map
                (lambda (flagnames body)
                    `(option ',flagnames #f #f (lambda (,opt-g ,name-g ,arg-g . ,seeds-g) ,@body #;(invoke-static java.lang.System 'exit 0)))
                ) immediate-flagnames-es immediate-bodies
            ))
            (unrecognized-option-proc `(lambda (opt name arg . seeds) (display "Warning: unrecognized option \"") (display name) (display "\".") (newline)))
            ;(operand-proc `(lambda (operand . seeds) (display "Received operand \"") (display operand) (display "\".") (newline)))
            (operand-proc `(lambda (operand . seeds) #!void))
            (displayable-flagname (lambda (flag)
                (cond ((char? flag) (string #\- flag))
                      ((string? flag) (String:format "--%s" flag))
                      (#t flag)
                )
            ))
            (mapcan (lambda (. args) (apply append (apply map args))))
            (display-all-options-proc `(lambda ()
                (define (printf . args) (apply invoke (static-field java.lang.System 'out) 'printf args))
                (printf "Standalone options: \n")
                ,@(mapcan (lambda (flagnames) (append (map (lambda (flag) `(printf "%s, " ,(displayable-flagname flag))) flagnames) `((printf "\n")))) immediate-flagnames-es)
                (printf "Settings:\n")
                ,@(map (lambda (varname default flagnames)
                    `(when (not (null? ',flagnames))
                        (printf "%s\t(default: %s)\t(settable by: " ',varname ,default)
                        ,@(maplist (lambda (flags)
                            (let ((flag (car flags)) (next? (not (null? (cdr flags)))))
                                `(printf "%s%s" ,(displayable-flagname flag) ,(if next? ", " ""))
                            )
                        ) flagnames)
                        (printf ")\n")
                    )
                ) varnames defaults flagnames-es)
            ))
        )
        `(begin
            (require 'args-fold)
            ,@code-to-set-defaults
            (let ((svfc-display-all-options ,display-all-options-proc))
                (args-fold (command-line) (list ,@setter-options ,@immediate-options) ,unrecognized-option-proc ,operand-proc)
            )
        )
    )
)
; without this indirection, a verify error occurs ("Incompatible argument to function", even in code that doesn't call it)
(define-macro (set-variables-from-cmdline . args) (apply set-variables-from-cmdline-aux args))

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

(define (jarray-map f arr::Object[])
    (returning (arr2::Object[] (Object[] length: arr:length))
        (do ((i 0 (+ i 1)))
            ((= i arr:length) #!void)
            (set! (arr2 i) (f (arr i)))
        )
    )
)

(define (jarray-foldl f arr::Object[] init)
    (do ((i 0 (+ i 1))
         (acc init (f acc (arr i))))
        ((= i arr:length) acc)
    )
)

(define identity-hash java.lang.System:identityHashCode)

) ; end of with-all-forms-exported
