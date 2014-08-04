(require 'list-lib)
(require 'args-fold)

;(define (mapcan . args) (apply append (apply map args)))

(define-macro (cxr-with-default path default)
    ; TODO: unroll the loop at compile time
    `(lambda (lst)
        (if (pair? lst)
            (fold (lambda (elem acc)
                (if (not (pair? acc)) ,default
                    (cond ((eqv? elem 'a) (car acc))
                          ((eqv? elem 'd) (cdr acc))
                          (else (error "element of path was not an a or d"))
                    )
                )
            ) lst (reverse ',path))
            ,default
        )
    )
)

; Intended usage: (set-variables-from-cmdline (varname default (flagname*) converter?)*)
(define-macro (set-variables-from-cmdline immediates . optsets)
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
                    `(option ',flagnames #f #f (lambda (,opt-g ,name-g ,arg-g . ,seeds-g) ,@body (invoke-static java.lang.System 'exit 0)))
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
                    `(begin
                        (printf "%s (default: \"%s\") : " ',varname ,default)
                        ,@(map (lambda (flag) `(printf "%s, " ,(displayable-flagname flag))) flagnames)
                        (printf "\n")
                    )
                ) varnames defaults flagnames-es)
            ))
        )
        `(begin
            ,@code-to-set-defaults
            (let ((svfc-display-all-options ,display-all-options-proc))
                (args-fold (command-line) (list ,@setter-options ,@immediate-options) ,unrecognized-option-proc ,operand-proc)
            )
        )
    )
)
;(display (command-line)) (newline)

(set-variables-from-cmdline
    (
        ((#\h #\? "help") (display "This is a help message.") (newline) (svfc-display-all-options))
        ((#\v "version") (display "Version number epsilon.") (newline))
    )
    (+message+ "Hello, world!" (#\m "message"))
    (+number+ 6.28 (#\n "number") java.lang.Double:parseDouble)
)

(display "+message+ is \'") (write +message+) (display "\'.") (newline)
(display "+number+ is \'") (write +number+) (display "\'.") (newline) ; yields strings when obtained from command line. fix with optional converter proc?
