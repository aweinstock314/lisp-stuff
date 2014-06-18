(set-macro-character #\3 
    (lambda (stream char)
        (declare (ignore char))
        (do ((x #\0 (read-char stream nil #\4 t)))
            ((equalp x #\4) x))
        (values)
    )
    t
)


'(1 2 3 hello world! )( mismatched parentheses!!!! oneoneone ())))) ((()()))())))) 4 5)


'(1 3 this
is
a
mutiple-line

comment
4 5)

'(1 #| multiple
line
comment |# 2)
