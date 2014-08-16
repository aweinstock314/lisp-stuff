(require <scheme_util_general>)

;(define (mapcan . args) (apply append (apply map args)))
;(display (command-line)) (newline)

(set-variables-from-cmdline
    (
        ((#\h #\? "help") (display "This is a help message.") (newline) (svfc-display-all-options) (java.lang.System:exit 0))
        ((#\v "version") (display "Version number epsilon.") (newline) (java.lang.System:exit 0))
    )
    (+message+ "Hello, world!" (#\m "message"))
    (+number+ 6.28 (#\n "number") java.lang.Double:parseDouble)
)

(display "+message+ is \'") (write +message+) (display "\'.") (newline)
(display "+number+ is \'") (write +number+) (display "\'.") (newline)
