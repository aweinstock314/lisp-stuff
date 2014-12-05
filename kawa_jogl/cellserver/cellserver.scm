(require <scheme_util_general>)
(require 'regex)
(require 'list-lib)

(java-imports (com.sun.net.httpserver HttpServer HttpExchange)
              (java.io PrintStream)
              (java.net InetSocketAddress)
              (java.lang StringBuilder)
              (java.util.regex Pattern Matcher)
)

(define *seed-mode* 'rlebin)
(define *port-number* 8100)
(define *minimum-width* 100)
(define *render-depth* 50)

(define (serve-string e::HttpExchange code::int str::String)
    (e:sendResponseHeaders code (str:length))
    (define ps (PrintStream (e:getResponseBody)))
    (ps:print str)
    (ps:close)
)

(define (extract-parameters requestPath::String mode)
    (case mode
        ((hex) (regex-match #/^\/rule([0-9]+)seed([0-9a-fA-F]+)$/ requestPath))
        ((bin) (regex-match #/^\/rule([0-9]+)seed([01]+)$/ requestPath))
        ((rlebin) (regex-match #/^\/rule([0-9]+)seed((?:(?:\([0-9]+\))?[01])+)$/ requestPath))
        (else (error (String:format "Unknown mode %s" mode)))
    )
)

(define (css-header)
    (define color (String[]  "#000000" "#ffffff"))
    (define (style c) (String:format "{color: %s; background-color: %s;}" (color c) (color (- 1 c))))
    (unescaped-data (String:format "<style>.blackcell %s .whitecell %s</style>" (style 1) (style 0)))
)

(define (emit-page bodycontent) :: String
    (define titlestr "Cellserver - dynamically serving 1d cellular automata")
    (html:html (html:head (html:title titlestr) (css-header)) (html:body bodycontent))
)

(define (bitvector-of-binstring s::String)
    (returning (vec (make-u8vector (s:length)))
        (pascal-for (i 0 (s:length) 1)
            (u8vector-set! vec i (if (equal? (s:charAt i) #\0) 0 1))
        )
    )
)

(define-macro (with-matcher re str varname . body)
    `(let ((,varname (invoke ,re 'matcher ,str)))
        (while (invoke ,varname 'find)
            ,@body
        )
    )
)

(define (regex-map re::Pattern str::String f::procedure)
    (with-list-collector col (with-matcher re str m (col (f m))))
)

(define (bitvector-of-rle-binstring s::String)
    (apply u8vector-concat (regex-map #/(?:\(([0-9]+)\))?([01])/ s
        (lambda (m::Matcher)
            (let* ( (size (aif/nn (m:group 1) (Integer:valueOf it) 1))
                    (bit (Integer:valueOf (m:group 2)))
                )
                (make-u8vector size bit)
            )
        )
    ))
)

(define (rlestring-of-bitvector vec::u8vector rle-threshhold::integer) :: String
    (define len (u8vector-length vec))
    (define (emit sb last occs)
        (if (< occs rle-threshhold)
            (pascal-for (i 0 occs 1) (sb:append last))
            (sb:append (String:format "(%s)%s" occs last))
        )
    )
    (returning (sb::StringBuilder (StringBuilder)) (let aux ((idx 0) (last #!null) (occs 0))
        (if (< idx len)
            (if (or (eq? last #!null) (equal? last (vec idx)))
                (aux (+ idx 1) (vec idx) (+ occs 1))
                (begin (emit sb last occs) (aux (+ idx 1) (vec idx) 1))
            )
            (aif/nn last (emit sb it occs))
        )
    ))
)

(define (bitvector-of-int x::integer)
    (returning (vec (make-u8vector (bitwise-length x)))
        (pascal-for (i 0 (bitwise-length x) 1)
            (u8vector-set! vec i (if (bitwise-bit-set? x i) 1 0))
        )
    )
)

;(define (u8vector-concat . vecs) (list->u8vector (apply append (map u8vector->list vecs))))
(define (u8vector-concat #!rest vecs::u8vector[])
    (define total-len::int (jarray-foldl (lambda (a e) (+ a (u8vector-length e))) vecs 0))
    (returning (vec::u8vector (make-u8vector total-len))
        (define idx::int 0)
        (pascal-for (i 0 vecs:length 1)
            (pascal-for (j 0 (u8vector-length (vecs i)) 1)
                (u8vector-set! vec idx ((vecs i) j))
                (inc! idx 1)
            )
        )
    )
)

(define (ensure-minimum-length vec::u8vector n)
    (define padding (make-u8vector (max 0 (- n (u8vector-length vec)))))
    (u8vector-concat vec padding)
)

(define (intersperse-spacing vec n)
    (returning (newvec (make-u8vector (* (+ n 1) (u8vector-length vec))))
        (pascal-for (i 0 (u8vector-length vec) 1)
            (set! (newvec (* i (+ n 1))) (vec i))
        )
    )
)

(define (make-toggle-link rulenum vec::u8vector i)
    (let* ( (tmpvec (list->u8vector (u8vector->list vec))) ; TODO: efficiency?
            (newseed (begin (set! (tmpvec i) (- 1 (tmpvec i))) (rlestring-of-bitvector tmpvec 5)))
        )
        (String:format "<a href=\"/rule%sseed%s\">%s</a>" rulenum newseed (vec i))
    )
)

(define (table-row-of-bitvector rulenum vec::u8vector insert-toggle?::boolean)
    (apply html:tr (accumulate-range (i 0 (u8vector-length vec) 1)
        (cond ((or (= (vec i) 0) (= (vec i) 1))
                (define color-class (if (> (vec i) 0) "blackcell" "whitecell"))
                (define content (if insert-toggle? (make-toggle-link rulenum vec i) (vec i)))
                (unescaped-data (String:format "<td class=\"%s\">%s</td>" color-class content))
            )
            (#t (html:td))
        )
    ))
)

(define (table-of-bitvector-list rulenum lst insert-toggle?)
    (apply html:table border: 1 style: "{display: inline-table;}"
        (if insert-toggle?
            (cons (table-row-of-bitvector rulenum (car lst) #t)
                (map (cut table-row-of-bitvector rulenum <> #f) (cdr lst))
            )
            (map (cut table-row-of-bitvector rulenum <> #f) lst)
        )
    )
)

(define (rulevec-of-rulenum rulenum) (ensure-minimum-length (bitvector-of-int rulenum) 8))

(define (make-rules-table rulenum)
    (define (make-rule-cell idx val)
        (table-of-bitvector-list #!null (list (ensure-minimum-length (bitvector-of-int idx) 3) (u8vector -1 val -1)) #f)
    )
    (define rulevec (rulevec-of-rulenum rulenum))
    (apply html:div "Rewrite rules: " (html:br)
        (accumulate-range (i 0 8 1) (make-rule-cell i (rulevec i)))
    )
)

(define (step-automaton rulevec::u8vector state::u8vector)
    (define len (u8vector-length state))
    (define (cell x) (state (mod (+ x len) len)))
    (list->u8vector (accumulate-range (i 0 len 1)
        (let ((left (cell (- i 1))) (up (cell i)) (right (cell (+ i 1))))
            (rulevec (+ (* 4 left) (* 2 up) right))
        )
    ))
)

(define (calculate-grid rulenum initial depth)
    (define rulevec (rulevec-of-rulenum rulenum))
    (define (aux i prev::u8vector)
        (define cur (step-automaton rulevec prev))
        (if (< i depth)
            (cons prev (aux (+ i 1) cur))
            (list cur)
        )
    )
    (aux 0 initial)
)

(define (make-seed-vector seedstr mode)
    (printf "using seed \"%s\"\n" seedstr)
    (case mode
        ((hex) (bitvector-of-int (Integer:valueOf seedstr 16)))
        ((bin) (bitvector-of-binstring seedstr))
        ((rlebin) (bitvector-of-rle-binstring seedstr))
        (else (error (String:format "Unknown mode %s" mode)))
    )
)

(define (make-cell-grid rulenum seedvec min-width depth)
    (table-of-bitvector-list
        rulenum
        (calculate-grid rulenum (ensure-minimum-length seedvec min-width) depth)
        #t
    )
)

(define (make-links rulenum seedstr)
    (define (mod256 x) (mod (+ x 256) 256))
    (define (make-rule-link num anchortext) (unescaped-data (String:format "<a href=\"/rule%sseed%s\">%s</a> " num seedstr anchortext)))
    (html:span  (make-rule-link (mod256 (- rulenum 1)) "Previous rule")
                (make-rule-link (mod256 (+ rulenum 1)) "Next rule")
                (html:br)
    )
)

(define (valid-response e::HttpExchange match)
    (let* ( (rulestr (match 1)) (seedstr (match 2))
            (rulenode (list "Rule:" rulestr (html:br)))
            (seednode (list (String:format "Seed: %s" seedstr) (html:br)))
            (rulenum (Integer:valueOf rulestr 10))
            ;(sometable (list (table-of-bitvector-list (accumulate-range (i 1 100 1) (ensure-minimum-length (bitvector-of-int i) 10)))))
            (links (list (make-links rulenum seedstr)))
            (ruletable (list (make-rules-table rulenum)))
            (datatable (list (make-cell-grid rulenum (make-seed-vector seedstr *seed-mode*) *minimum-width* *render-depth*)))
        )
        (apply html:span (append links rulenode seednode ruletable (list (html:br)) datatable))
    )
)

(define (invalid-response e::HttpExchange)
    (html:span "Invalid parameters, proper requests are of the form \"/rule[0-255]seed{run-length-encoded binary}\"" (html:br)
        "For example: " (html:br)
        (html:a href: #1="/rule30seed1" #1#) (html:br)
        (html:a href: #1="/rule90seed(50)01" #1#) (html:br)
        (html:a href: #1="/rule110seed(25)0(5)1(5)0(30)1" #1#) (html:br)
    )
)

(define-macro (handle-request-form) '(lambda (e::HttpExchange)
    (print-exceptions
        (printf "Handling a request to %s\n" (e:getRemoteAddress))
        (define requestPath ((e:getRequestURI):getPath))
        (printf "Request URI: \"%s\"\n" requestPath)
        (define bodycontent (aif (extract-parameters requestPath *seed-mode*)
            (valid-response e it)
            (invalid-response e)
        ))
        (serve-string e 200 ((emit-page bodycontent):replaceAll "><" ">\n<")) ; maybe change the code for an invalid response?
        (e:close)
    )
))

(define serv (HttpServer:create (InetSocketAddress *port-number*) -1))
(serv:start)
(serv:createContext "/" (handle-request-form))
(printf "Bound to port %s\n" *port-number*)
