(require <scheme_util_general>)
(require <scheme_util_math>)
(require <scheme_util_networking>)

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


(define jf (javax.swing.JFrame))
(define sender (TCPKeyboardSender *port-number* *host-name*))
(jf:addKeyListener sender)
(jf:setSize window-size window-size)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(jf:setLayout #!null)
(jf:setVisible #t)
(jf:requestFocus)
