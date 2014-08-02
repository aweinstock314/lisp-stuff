(require 'list-lib)
(require 'args-fold)

;(define (mapcan . args) (apply append (apply map args)))

; Intended usage: (set-variables-from-cmdline (varname (flagname*) default)*)
(define-macro (set-variables-from-cmdline . optsets)
    (let* (
            (varnames (map car optsets))
            (flagnames-es (map cadr optsets))
            (defaults (map caddr optsets))
            (code-to-set-defaults (map (lambda (varname default) `(define ,varname ,default)) varnames defaults))
            (setter-options (map
                (lambda (varname flagnames)
                    `(option ',flagnames #t #f (lambda (opt name arg . seeds) (set! ,varname arg)))
                ) varnames flagnames-es
            ))
            (unrecognized-option-proc `(lambda (opt name arg . seeds) (display "Warning: unrecognized option \"") (display name) (display "\".") (newline)))
            ;(operand-proc `(lambda (operand . seeds) (display "Received operand \"") (display operand) (display "\".") (newline)))
            (operand-proc `(lambda (operand . seeds) #!void))
        )
        `(begin
            ,@code-to-set-defaults
            (args-fold (command-line) (list ,@setter-options) ,unrecognized-option-proc ,operand-proc)
        )
    )
)
;(display (command-line)) (newline)

(set-variables-from-cmdline
    (+message+ (#\m "message") "Hello, world!")
    (+number+ (#\n "number") 6.28)
)

(display "+message+ is \"") (display +message+) (display "\".") (newline)
(display "+number+ is \"") (display +number+) (display "\".") (newline)
