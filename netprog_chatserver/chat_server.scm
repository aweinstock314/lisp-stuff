(define-alias Integer java.lang.Integer)
(define-alias Character java.lang.Character)
(define-alias Thread java.lang.Thread)
(define-alias StringBuilder java.lang.StringBuilder)
(define-alias Short java.lang.Short)
(define-alias Socket java.net.Socket)
(define-alias SocketAddress java.net.SocketAddress)
(define-alias ServerSocket java.net.ServerSocket)
(define-alias DatagramSocket java.net.DatagramSocket)
(define-alias DatagramPacket java.net.DatagramPacket)
(define-alias ServerSocketChannel java.nio.channels.ServerSocketChannel)
(define-alias DatagramChannel java.nio.channels.DatagramChannel)
(define-alias SocketChannel java.nio.channels.SocketChannel)
(define-alias Set java.util.Set)
(define-alias Map java.util.Map)
(define-alias Math java.lang.Math)
(define-alias HashMap java.util.HashMap)
(define-alias SelectionKey java.nio.channels.SelectionKey)
(define-alias ByteBuffer java.nio.ByteBuffer)
(define-alias BufferedInputStream java.io.BufferedInputStream)
(define-alias OutputStreamWriter java.io.OutputStreamWriter)
(define-alias PrintStream java.io.PrintStream)
(define-alias BufferedReader java.io.BufferedReader)
(define-alias InputStreamReader java.io.InputStreamReader)
(define-alias InputStream java.io.InputStream)
(define-alias ByteArrayInputStream java.io.ByteArrayInputStream)
(define-alias SynchronousQueue java.util.concurrent.SynchronousQueue)
(define-alias LinkedBlockingQueue java.util.concurrent.LinkedBlockingQueue)
(define-alias BlockingQueue java.util.concurrent.BlockingQueue)

(define (synchronizedHashMap) (java.util.Collections:synchronizedMap (HashMap)))
(define (random maximum) ((Math:floor (* maximum (Math:random))):intValue))

(define *verbose-mode* #f)
(define *debug-mode* #f)
(define *datagram-buffer-max* 500)
;(define *message-queue* ::BlockingQueue (SynchronousQueue))
;(define tcp-address-map ::HashMap (HashMap)) ; String->Socket
;(define udp-address-map ::HashMap (HashMap)) ; String->SocketAddress
(define *address-map* ::java.util.Map (java.util.Collections:synchronizedMap (HashMap))) ; String->sender-context
;(define *revaddr-map* ::java.util.Map (java.util.Collections:synchronizedMap (HashMap))) ; sender-context->String
(define *forge-message-count* 3)
(define *last-senders* ::Map (synchronizedHashMap)) ; username->list, stores senders to each user, resets to empty list when reaching the counter for a forged message


(define-macro (printf fmt #!rest args) `(java.lang.System:out:printf ,fmt ,@args))

(define-macro (dbprintf fmt #!rest args) `(when *debug-mode* (java.lang.System:out:printf ,fmt ,@args)))

(define-macro (verbose-printf fmt #!rest args) `(when *verbose-mode* (java.lang.System:out:printf ,fmt ,@args)))

;(define (verbose-printf fmt ::string #!rest args ::Object[]) (if *verbose-mode* (printf fmt args) (printf "not in verbose mode")))

; "anaphoric if" - binds the variable "it" to avoid redundant computation
(define-macro (aif test then-clause #!optional (else-clause '()))
	`(let ((it ,test))
		(if it
			,then-clause
			,else-clause
		)
	)
)

(define-macro (acond  #!rest clauses)
	(if (not (null? clauses))
		(let ((clause (car clauses)))
			(if (not (null? clause))
				(let (
					(test (car clause))
					(body (cdr clause))
				)
					`(aif ,test
						(begin ,@body)
						(acond ,@(cdr clauses))
					)
				)
			'()
			)
		)
		'()
	)
)

; if any exceptions are encountered while executing body, just return false
(define-macro (ignore-exceptions #!rest body)
	`(try-catch
		,@body
		(e java.lang.Exception (when *debug-mode* (e:printStackTrace)) #f)
	)
)

(define-macro (loop-forever #!rest body)
	`(do ()
		(#f #f)
		,@body
	)
)

(define-macro (regex-case string-expr matcher-name #!rest args)
	(let ((strvar (gentemp)))
		`(let ((,strvar ::String ,string-expr) (,matcher-name ::java.util.regex.Matcher #!null))
			(cond
				,@(map (lambda (arg)
					(let ((regex-str ::String (car arg)) (body-exprs (cdr arg)))
						`((begin
							(set! ,matcher-name (invoke (invoke-static java.util.regex.Pattern 'compile ,regex-str ,(bitwise-ior java.util.regex.Pattern:DOTALL java.util.regex.Pattern:MULTILINE)) 'matcher ,strvar))
							(*:find ,matcher-name)
							)
							,@body-exprs
						)
					)
				) args)
				(#t (error "Fell off the end of regex-case."))
			)
		)
	)
)

(define-macro (aif/nn test-c then-c #!optional (else-c '()))
	`(let ((it ,test-c))
		(if (not (equal? it #!null)) ,then-c ,else-c)
	)
)

(define (null-default val default) (if (equal? val #!null) default val))

(define-macro (let/cc ccname #!rest body) `(call-with-current-continuation (lambda (,ccname) ,@body)))

;;postfix type-annotations interact poorly with macros
;(define-macro (foreach-iter var-name iter-name #!rest body)
;	`(while (*:hasNext ,iter-name)
;		(let ((,var-name (*:next ,iter-name)))
;			,@body
;		)
;	)
;)

;(define (extract-datagram datagram-socket ::DatagramSocket)
;	(let* (
;			(bytearr (byte[] *datagram-buffer-max*))
;			(packet ::DatagramPacket (DatagramPacket bytearr bytearr:length))
;		)
;		(datagram-socket:receive packet)
;		packet
;	)
;)

(define (read-datagram datagram-channel ::DatagramChannel)
	(let* (
			(bytearr (byte[] length: *datagram-buffer-max*))
			(buffer ::ByteBuffer (ByteBuffer:wrap bytearr))
			(sourceaddr ::java.net.SocketAddress (datagram-channel:receive buffer))
			(packet ::DatagramPacket (DatagramPacket bytearr bytearr:length sourceaddr))
		)
		;(dbprintf "bytearr: %s\n" (String bytearr))
		packet
	)
)

;  class that listens on a port and provides a receive method that returns either a UDP datagram or a TCP connection
;; some details assisted by "http://stackoverflow.com/questions/2819274/listening-for-tcp-and-udp-requests-on-the-same-port"
(define-simple-class tcp-udp-socket ()
	(tcpchannel ::ServerSocketChannel)
	(udpchannel ::DatagramChannel)
	(channel-selector ::java.nio.channels.Selector)
	((*init* port ::int) access: 'public
		(let ((sockaddr ::java.net.SocketAddress (java.net.InetSocketAddress port)))
			(set! tcpchannel (ServerSocketChannel:open))
			(set! udpchannel (DatagramChannel:open))
			((tcpchannel:socket):bind sockaddr)
			((udpchannel:socket):bind sockaddr)
			(tcpchannel:configureBlocking #f)
			(udpchannel:configureBlocking #f)
			(set! channel-selector (java.nio.channels.Selector:open))
			(tcpchannel:register channel-selector SelectionKey:OP_ACCEPT)
			(udpchannel:register channel-selector SelectionKey:OP_READ)
		)
	)
	((receive) access: 'public
		(let* (
				(num-selected ::int (channel-selector:select))
				(keys ::Set (channel-selector:selectedKeys))
				(iter ::java.util.Iterator (keys:iterator))
				(channel #!null)
				(retval #f)
			)
			(set! retval #f)
			(do () ((not (iter:hasNext)) retval)
				(let ((key ::SelectionKey (iter:next)))
					(iter:remove)
					(set! channel (key:channel))
					(cond
						((and (key:isAcceptable) (eqv? channel tcpchannel)) (set! retval ((tcpchannel:accept):socket)))
						((and (key:isReadable) (eqv? channel udpchannel)) (set! retval (read-datagram udpchannel)))
					 	(#f (set! retval #!null))
					)
				)
			)
		)
	)
)

(define (string->bytebuf str ::String) (ByteBuffer:wrap (str:getBytes)))

; instantiated with either a tcp socket or a (datagram channgel,address) pair for udp, and allows sending messages
(define-simple-class sender-context ()
	(tcpsock ::Socket #!null)
	(udpchan ::DatagramChannel #!null)
	(addr ::SocketAddress #!null)
	(username ::String #!null)
	(ctxtype #!null)
	((*init* sock ::Socket un ::String) access: 'public
		(set! tcpsock sock)
		(tcpsock:setTcpNoDelay #t)
		(set! addr (sock:getRemoteSocketAddress))
		(set! ctxtype 'tcp)
		(set! username un)
	)
	((*init* channel ::DatagramChannel address ::SocketAddress un ::String) access: 'public
		(set! udpchan channel)
		(set! addr address)
		(set! ctxtype 'udp)
		(set! username un)
	)
	((send msg ::String forged) access: 'public
		(dbprintf "ctxtype: %s\n" ctxtype)
		(case ctxtype
			;('tcp (let ((osw ::OutputStreamWriter (OutputStreamWriter (tcpsock:getOutputStream)))) (osw:write msg 0 (msg:length)) (osw:flush)))
			;('tcp (let ((ps ::PrintStream (PrintStream (tcpsock:getOutputStream)))) (ps:print msg) (ps:flush)))
			('tcp (let ((sockchannel ::SocketChannel (tcpsock:getChannel))) (sockchannel:write (string->bytebuf msg))))
			;('udp (udpsock:send (DatagramPacket (msg:getBytes) (msg:getBytes):length addr))) ;doesn't quite work, for some reason
			('udp
				(udpchan:connect addr)
				(udpchan:write (string->bytebuf msg))
				(udpchan:disconnect)
			)
		)
		(verbose-printf "SENT%s to %s: %s\n" (if forged " (randomly!)" "") (if (equal? username #!null) addr (String:format "%s (%s)" username addr)) msg)
	)
	((toString) ::String access: 'public
		;; to replace the following line from a printf
		;;(if (Socket? addr/sock) (addr/sock:getRemoteSocketAddress) addr/sock)
		(if (not (equal? addr #!null)) addr "#<sender-context, uninitialized>")
	)
)

#|(define-simple-class chat-world ()
	(address-map ::java.util.Map (java.util.Collections:synchronizedMap (HashMap))) ; String->sender-context
	(revaddr-map ::java.util.Map (java.util.Collections:synchronizedMap (HashMap))) ; sender-context->String
	((*init* ports ::int[]) access: 'public
		; move stuff from instantiate-server here?
		#f
	)
)|#

(define (parse-int x ::String) (ignore-exceptions (Integer:valueOf x)))
(define (fits-unsigned-short? x) (and (>= x 0) (<= x 65565) x))
(define (port-number? x ::String) (fits-unsigned-short? (parse-int x)))

(define (instantiate-server port-number ::int)
	(verbose-printf "Instantiating a server listening on port %s\n" port-number)
	(let ((listener-socket (tcp-udp-socket port-number)))
		(future (loop-forever
			(let ((client-socket-or-dgram (listener-socket:receive)))
				(future (ignore-exceptions (handle-connection-or-dgram client-socket-or-dgram listener-socket:udpchannel)))
			)
		))
	)
)

(define (handle-connection-or-dgram socket-or-dgram dgchannel ::DatagramChannel)
	(cond
		((Socket? socket-or-dgram) (make-client socket-or-dgram))
		((DatagramPacket? socket-or-dgram) (handle-dgram socket-or-dgram dgchannel))
		(#t #f)
	)
)

(define (canonicalize-username username ::String) (username:toLowerCase))

;(define (construct-message str addr) (make-message-struct str (sender-context addr #!null) #f))

(define (handle-login username ::String sctx ::sender-context)
	(if (not (*address-map*:containsKey username))
		(begin
			(set! sctx:username (canonicalize-username username))
			(*address-map*:put username sctx)
			(list (make-message-struct "OK\n" username #f))
		)
		(list (make-message-struct "ERROR username already taken\n" sctx #f))
	)
)


(define (handle-logout username ::String)
	(*address-map*:remove (canonicalize-username username))
	#f
)

(define (jarray->list jarray ::Object[])
	(do ((i 0 (+ 1 i))
		(acc '() (cons (jarray i) acc)))
		((= i jarray:length) (reverse acc)))
)

(define (list->jarray lst)
	(let* (
			(len (length lst))
			(tmp (Object[] length: len))
		)
		(do ((i 0 (+ i 1))
			(cur lst (cdr cur))
			)
			((= i len) tmp)
			(set! (tmp i) (car cur))
		)
	)
)

(define (get-users-list) (map canonicalize-username (jarray->list ((*address-map*:keySet):toArray))))

(define (handle-users-listing username ::String)
	(list (make-message-struct (str-append (((get-users-list):toString):replaceAll "[()]" "") "\n") (canonicalize-username username) #f))
)

#|(define (slurp-all-lines reader ::BufferedReader)
	(let ((data ::StringBuilder (StringBuilder)))
		(do ((line (reader:readLine) (reader:getLine)))
			((or (equal? line #!null) (equal? line "")) (dbprintf "Slurp ending\n") (data:toString))
			(dbprintf "Line: %s\n" line)
			(data:append line)
			(data:append "\n")
		)
	)
)|#

(define (foldl fn lst acc)
	(if (null? lst)
		acc
		(foldl fn (cdr lst) (fn acc (car lst)))
	)
)

(define (list-of-strings->string lst) ((foldl (lambda (acc ::StringBuilder elem) (acc:append elem) acc) lst (StringBuilder)):toString))
(define (str-append s1 ::String s2 ::String) (s1:replaceFirst "$" s2))

(define (read-n-chars reader ::BufferedReader n ::Integer)
	(let (
			(charbuf (char[] length: n))
		)
		(do ((i 0 (+ i 1)))
			((>= i n) charbuf)
			(set! (charbuf i) (reader:read))
		)
	)
)

(define (slurp-relevant-lines reader::BufferedReader)
	(let/cc return
		(let (
				(data '())
				(num-chars 0)
				(chunked #f)
				(charbuf ::char[] #!null)
			)
			(let ((tmp (reader:readLine)))
				(if (not (equal? tmp #!null))
					(set! data (cons (str-append tmp "\n") data)) ; initial line
					(return data)
				)
			)
			(dbprintf "%s\n" data)
			(when
				(regex-case ((car data):toString) m ; get the length line for SEND/BROADCAST
					("^SEND" (set! data (cons (str-append (reader:readLine) "\n") data)) #t)
					("^BROADCAST" (set! data (cons (str-append (reader:readLine) "\n") data)) #t)
					(".*" #f)
				)
				(dbprintf "%s\n" data)
				(when
					(regex-case (car data) m ; determine the number of bytes to read, dependent on chunking
						("C([0-9]*)" (set! chunked #t) (set! num-chars (parse-int (m:group 1))) #t)
						("([0-9]*)" (set! chunked #f) (set! num-chars (parse-int (m:group 1))) #t)
						(".*" #f)
					)
					(dbprintf "num-chars: %s\n" num-chars)
					;(set! charbuf (char[] length: num-chars))
					;(reader:read charbuf 0 charbuf:length)
					(set! charbuf (read-n-chars reader num-chars))
					(set! data (cons (String charbuf) data))
					(dbprintf "%s\n" data)
				)
			)
			(dbprintf "Returning %s from slurp-relevant-lines\n" data)
			(reverse data)
		)
	)
)

(define-record-type message-struct
	(make-message-struct message receiver forged-tag)
	message-struct?
	(message message set-message!)
	(receiver receiver set-receiver!)
	(forged-tag forged-tag set-forged-tag!)
)

(define (handle-forging sender receivers)
	(map (lambda (rec)
		(let (
				(prev-sends (cons sender (null-default (*last-senders*:get rec) '())))
				(msg #!null)
			)
			(dbprintf "in (handle-forging %s %s) for rec %s, prev-sends is %s\n" sender receivers rec prev-sends)
			(when (= (length prev-sends) *forge-message-count*)
				(dbprintf "in the when-clauses\n")
				(let ((forged-sender (list-ref prev-sends (random *forge-message-count*))))
					(set! msg (make-message-struct (encode-from-message forged-sender (generate-forged-message)) rec #t))
				)
				(set! prev-sends '())
			)
			(*last-senders*:put rec prev-sends)
			(dbprintf "handle-forging returning \"%s\"\n" msg)
			msg
		)
	) receivers)
)

(define (encode-from-message sender msg ::String) (String:format "FROM %s\n%d\n%s" sender (msg:length) msg))

(define (load-file-lines-as-stringarray fname ::String)
	(let* (
			(reader ::BufferedReader (BufferedReader (InputStreamReader (java.io.FileInputStream fname))))
			(result '())
		)
		(do ((tmp (reader:readLine) (reader:readLine)))
			((equal? tmp #!null) (list->jarray (reverse result)))
			(set! result (cons tmp result))
		)
	)
)

(define *forgable-messages* ::Object[] (load-file-lines-as-stringarray "forgable_messages.txt"))
(define (generate-forged-message) (*forgable-messages* (random *forgable-messages*:length)))

(define (handle-message-send sender receivers data broadcast)
	(let* (
			(msg ::String (list-of-strings->string (cddr data)))
			(msgintsize ::int (msg:length))
			(msgsize ::String (String:format "%d" msgintsize))
			(msgclaimedsize ::int (aif (parse-int ((cadr data):trim)) it 0))
			(retval '())
		)
		(when (not (= msgintsize msgclaimedsize)) (dbprintf "Mismatch in claimed size (%s) and actual size (%s) of message.\n" (cadr data) msgsize))
		(when (not broadcast) (set! retval (append (handle-forging sender receivers) retval)))
		(set! retval (cons (make-message-struct (encode-from-message sender msg) receivers #f) retval))
		(write retval)
		retval
	)
)

(define (parse-message instream ::InputStream sctx ::sender-context)
	(let* (
			(reader ::BufferedReader (BufferedReader (InputStreamReader instream)))
			;(first-line ::String (reader:readLine))
			(data (ident-print (slurp-relevant-lines reader)))
			(datastr ::String (list-of-strings->string data))
		)
		(if (null? data) ((Thread:currentThread):join))
		(verbose-printf "RCVD from %s: %s\n" sctx datastr)
		(let ((tmp
		(regex-case datastr match
			("^ME IS ([^\\s]*)$" (match:group 1) (handle-login (match:group 1) sctx))
			("^LOGOUT ([^\\s]*)$" (handle-logout (match:group 1)))
			("^SEND ([^\\s]*) ([^\n]*)$" (handle-message-send (match:group 1) (jarray->list ((match:group 2):split "\\s")) data #f))
			("^BROADCAST ([^\\s]*)$" (handle-message-send (match:group 1) (get-users-list) data #t))
			("^WHO HERE ([^\\s]*)$" (handle-users-listing (match:group 1)))
			(".*" (list (make-message-struct "ERROR Invalid header for message\n" sctx #f)))
		)
		)) (dbprintf "retval for parse-message: %s" tmp) tmp)
	)
)

;(define (send-enqueued-message queue ::BlockingQueue) (send-message (queue:take)))

(define (send-message msg ::message-struct)
	(when (not (equal? msg #!null))
		(let* (
				(msg-str msg:message)
				(msg-to msg:receiver)
				(msg-forged msg:forged-tag)
			)
			(dbprintf "sending msg (%s,%s,%s)\n" msg-str msg-to msg-forged)
			(send-message/parts msg-str msg-to msg-forged)
		)
	)
)

(define (send-message/parts msg-str ::String msg-to msg-forged)
	(cond
		((equal? msg-to #!null) #f)
		((sender-context? msg-to) (msg-to:send msg-str msg-forged))
		((String? msg-to) (send-message/parts msg-str (*address-map*:get (canonicalize-username msg-to)) msg-forged))
		((list? msg-to) (map (lambda (dest)
			(dbprintf "dest: %s\n" dest)
			(send-message/parts msg-str dest msg-forged)
		) msg-to))
		(#t (dbprintf "The destination for the enqueued message (%s,%s,%s) is not a sender-context, username, or list of either.\n" msg-str msg-to msg-forged) #f)
	)
)

(define (make-client socket ::Socket)
	(verbose-printf "Recieved a TCP connection from %s\n" (socket:getRemoteSocketAddress))
	(let ((instream ::BufferedInputStream (BufferedInputStream (socket:getInputStream))))
		(loop-forever
			(aif (parse-message instream (sender-context socket #!null)) (map send-message it))
		)
	)
)

(define (ident-print x) (dbprintf "%s\n" x) x)

(define (handle-dgram dgram ::DatagramPacket dgchannel ::DatagramChannel)
	(verbose-printf "Recieved a UDP datagram from %s\n" (dgram:getSocketAddress))
	(dbprintf "%s\n" (String (dgram:getData)))
	(aif (parse-message (ByteArrayInputStream (dgram:getData)) (sender-context dgchannel (dgram:getSocketAddress) #!null)) (map send-message it)) ;(*message-queue*:put it))
)

;(display command-line-arguments) (newline)

(define (is-verbose-flag? arg) (or (equal? arg "-v") (equal? arg "--verbose")))

(define port-numbers-list '())

(vector-map
	(lambda (arg)
		(acond
			((port-number? arg) (set! port-numbers-list (cons it port-numbers-list)))
			((is-verbose-flag? arg) (set! *verbose-mode* #t))
			(#t (printf "Unknown argument: \"%s\"\n" arg))
		)
	)
	command-line-arguments
)
(set! port-numbers-list (reverse! port-numbers-list))

;(display port-numbers-list) (newline)

(map
	(lambda (port-number)
		(instantiate-server port-number)
	)
	port-numbers-list
)

;(loop-forever (ignore-exceptions (send-enqueued-message *message-queue*)))
