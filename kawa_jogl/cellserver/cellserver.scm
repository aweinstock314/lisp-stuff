(require <scheme_util_general>)
(require 'regex)

(java-imports (com.sun.net.httpserver HttpServer HttpExchange)
              (java.io PrintStream)
              (java.net InetSocketAddress)
)

(define (serve-string e::HttpExchange code::int str::String)
    (e:sendResponseHeaders code (str:length))
    (define ps (PrintStream (e:getResponseBody)))
    (ps:print str)
    (ps:close)
)

(define (extract-parameters requestPath::String)
    (regex-match #/^\/rule([0-9]+)seed([0-9a-fA-F]+)$/ requestPath)
)

(define (emit-page bodycontent)
    (define titlestr "Cellserver - dynamically serving 1d cellular automata")
    (html:html (html:head (html:title titlestr)) (html:body bodycontent))
)

(define (valid-response e::HttpExchange match)
    (let* ( (rulestr (match 1)) (seedstr (match 2))
            (rulenode (list "Rule:" rulestr (html:br)))
            (seednode (list (String:format "Seed: 0x%s" seedstr) (html:br)))
            (rulenum (Integer:valueOf rulestr 10))
            (seednum (Integer:valueOf seedstr 16))
        )
        (apply html:span (append rulenode seednode))
    )
)

(define (invalid-response e::HttpExchange)
    (html:span "Invalid Parameters - try page " (html:a href: #1="/rule30seed1" #1#) (html:br))
)

(define-macro (handle-request-form) '(lambda (e::HttpExchange)
    (print-exceptions
        (printf "Handling a request to %s\n" (e:getRemoteAddress))
        (define requestPath ((e:getRequestURI):getPath))
        (printf "Request URI: \"%s\"\n" requestPath)
        (define parameters-match (extract-parameters requestPath))
        (define bodycontent (if parameters-match (valid-response e parameters-match) (invalid-response e)))
        (serve-string e 200 (emit-page bodycontent)) ; maybe change the code for an invalid response?
        (e:close)
    )
))

(define port-number 8100)

(define serv (HttpServer:create (InetSocketAddress port-number) -1))
(serv:start)
(serv:createContext "/" (handle-request-form))
(printf "Bound to port %s\n" port-number)
