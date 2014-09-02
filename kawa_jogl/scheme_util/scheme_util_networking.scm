(require <scheme_util_general>)
(require <scheme_util_math>)

(java-import
    java.net.Socket java.net.ServerSocket
    java.io.InputStream java.io.OutputStream
    java.nio.ByteBuffer
    java.awt.Component java.awt.event.KeyListener java.awt.event.KeyEvent)

(with-all-forms-exported

(define-simple-class TCPKeyboardSender (KeyListener)
    (PRESSED::byte allocation: 'static 0)
    (RELEASED::byte allocation: 'static 1)
    (sock::Socket)
    (out::OutputStream)
    ((*init* port::integer host::String)
        (set! sock (Socket host port))
        (set! out (sock:getOutputStream))
        ;(printf "sock: %s\nout: %s\n" sock out)
    )
    ((keyPressed ev)
        (out:write PRESSED)
        (out:write (ev:getKeyCode))
    )
    ((keyReleased ev)
        (out:write RELEASED)
        (out:write (ev:getKeyCode))
    )
    ((keyTyped ev) #!void)
)

(define-simple-class TCPKeyboardReceiver ()
    (PRESSED::byte allocation: 'static 0)
    (RELEASED::byte allocation: 'static 1)
    (sock::Socket)
    (in::InputStream)
    (dest::KeyListener)
    (DUMMY::Component (object (Component)))
    ((*init* kl::KeyListener port::integer)
        (future (begin
            (define servsock (ServerSocket port))
            (set! sock (servsock:accept))
            (set! in (sock:getInputStream))
            (set! dest kl)
            (doIoLoop!)
        ))
    )
    ((doIoLoop!) (while #t (emitKeypress!)))
    ((emitKeypress!)
        (let* ((mode (in:read))
               (key (in:read))
               (timestamp (get-time))
               (modifiers 0)
            )
            (unless (or (equal? mode -1) (equal? key -1))
                (case mode
                    ((PRESSED) (dest:keyPressed
                        (KeyEvent DUMMY KeyEvent:KEY_PRESSED timestamp modifiers key KeyEvent:CHAR_UNDEFINED)
                    ))
                    ((RELEASED) (dest:keyReleased
                        (KeyEvent DUMMY KeyEvent:KEY_RELEASED timestamp modifiers key KeyEvent:CHAR_UNDEFINED)
                    ))
                )
            )
        )
    )
)

) ; end of with-all-forms-exported
