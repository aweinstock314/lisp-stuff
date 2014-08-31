(require <scheme_util_general>)
(require <scheme_util_math>)

#|
Sample usage:
netcat -l -p 8100
# In another window:
java keypress_sender -p 8100 -h localhost
|#

(define-constant window-size 100)

(set-variables-from-cmdline
    ( ((#\h "help") (svfc-display-all-options) (java.lang.System:exit 0)) )
    (*port-number* 8100 (#\p "port") Integer:parseInt)
    (*host-name* "localhost" (#\h "host"))
)

(java-import java.net.Socket java.io.OutputStream java.nio.ByteBuffer)

(define-simple-class TCPKeyboardSender (java.awt.event.KeyListener)
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

(define jf (javax.swing.JFrame))
(define sender (TCPKeyboardSender *port-number* *host-name*))
(jf:addKeyListener sender)
(jf:setSize window-size window-size)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(jf:setLayout #!null)
(jf:setVisible #t)
(jf:requestFocus)
