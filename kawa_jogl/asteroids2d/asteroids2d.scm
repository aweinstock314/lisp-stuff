(require 'list-lib)
(define GL2 javax.media.opengl.GL2)
(define KeyEvent java.awt.event.KeyEvent)
(define printf java.lang.System:out:printf)
(define-alias ArrayList java.util.ArrayList)

(define-macro (thunk expr) `(lambda () ,expr))
(define-macro (mvbind vars expr . body) `(call-with-values (thunk ,expr) (lambda ,vars ,@body)))
(define-macro (mvlist expr) `(call-with-values (thunk ,expr) list))

; this has multiple-evaluation problems, but works fine for simple cases (does kawa have get-setf-expansion?)
(define-macro (inc! var delta) `(set! ,var (+ ,var ,delta)))
(define-macro (java-iterate iterable-expr varname . body)
    (define iterable-name (gentemp))
    (define real-varname (if (list? varname) (car varname) varname))
    (define vartype (if (list? varname) (cadr varname) java.lang.Object))
    `(let ((,iterable-name ::java.util.Iterator (invoke ,iterable-expr 'iterator))
           (,real-varname ::,vartype #!null))
        (do ()
            ((not (invoke ,iterable-name 'hasNext)) #!void)
            (set! ,real-varname (invoke ,iterable-name 'next))
            ,@body
        )
    )
)


(define *screen-width* 640)
(define *screen-height* 480)

(define (upto x) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i x) (reverse! acc))))
(define (constantly x) (lambda (. args) x))
(define (mod-w/cast n::int d::int) (mod n d)) ; allows clamping floats to screen width/height
(define tau (* 8 (atan 1)))
(define atan2 java.lang.Math:atan2)
(define (cart->polar x y) (values (sqrt (+ (square x) (square y))) (atan2 y x)))
(define (polar->cart m t) (values (* m (cos t)) (* m (sin t))))
(define (cart+ x1 y1 x2 y2) (values (+ x1 x2) (+ y1 y2)))

; returns a list of vertices of a "regular" polygon, with center at pos, first vertex at rot radians
; the radius parameter is a function to allow non-regular polygons such as isosceles triangles
(define (calc-poly pos rot radiusf sides)
    (map (lambda (i)
        (define rad (radiusf i))
        (define polyvert (mvlist (polar->cart rad (+ rot (* tau (/ i sides))))))
        (mvlist (apply cart+ (append pos polyvert)))
    ) (upto sides))
)

(define-simple-class drawer () interface: #t
    ((draw gl2::GL2) #!abstract)
)

(define-simple-class ship (drawer)
    (x 0) (y 0)
    (rot (/ tau 4))
    (velocity 0)
    (size .1)
    (color '(255 128 0))
    ((*init*) #!void)
    ((getVerts) (calc-poly (list x y) rot (lambda (i) (if (= i 0) (* 2 size) size)) 3)) ; isosceles triangle
    ((updatePosition)
        (set! x (+ x (* velocity (cos rot))))
        (set! y (+ y (* velocity (sin rot))))
        ; slight hack, try to fix with mod or something later
        (if (< x -1) (set! x 1))
        (if (< y -1) (set! y 1))
        (if (> x 1) (set! x -1))
        (if (> y 1) (set! y -1))
    )
    ((draw gl2::GL2) (drawPolygon gl2 color (getVerts)))
)

(define player-ship (ship))

(define jf (javax.swing.JFrame))
(jf:setSize *screen-width* *screen-height*)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(define glcanv (javax.media.opengl.awt.GLCanvas))

(define *currently-held-keys* (java.util.HashSet))
(define *rotation-delta* (/ tau 64))
(define *velocity-delta* .01)
(define (handle-movement)
    (if (*currently-held-keys*:contains KeyEvent:VK_LEFT) (inc! player-ship:rot *rotation-delta*))
    (if (*currently-held-keys*:contains KeyEvent:VK_RIGHT) (inc! player-ship:rot (- *rotation-delta*)))
    (if (*currently-held-keys*:contains KeyEvent:VK_UP) (inc! player-ship:velocity *velocity-delta*))
    (if (*currently-held-keys*:contains KeyEvent:VK_DOWN) (inc! player-ship:velocity (- *velocity-delta*)))
    (player-ship:updatePosition)
    (printf "pos: %s, %s\n" player-ship:x player-ship:y)
)

(define *render-queue*::ArrayList[drawer] (ArrayList))
(*render-queue*:add player-ship)
(define (render gl2::GL2 things-to-draw::ArrayList[drawer])
    (handle-movement)
    (gl2:glClear gl2:GL_COLOR_BUFFER_BIT)
    (java-iterate things-to-draw (d drawer) (d:draw gl2))
)

(define (drawPolygon gl2::GL2 color verts)
    (gl2:glBegin gl2:GL_POLYGON)
    (apply gl2:glColor3d (map (lambda (x) (/ x 255)) color))
    (for-each (lambda (xy)
        (apply gl2:glVertex2d xy)
    ) verts)
    (gl2:glEnd)
)
(glcanv:addGLEventListener (object (javax.media.opengl.GLEventListener)
    ((*init*) #!void)
    ((display drawable) (render ((drawable:getGL):getGL2) *render-queue*))
    ((init drawable) #!void)
    ((dispose drawable) #!void)
    ((reshape drawable x y w h) #!void)
))
(glcanv:addKeyListener (object (java.awt.event.KeyListener)
    ((keyPressed ev) 
        ;(printf "keypressed %s \n" (ev:getKeyCode))
        (*currently-held-keys*:add (ev:getKeyCode))
    )
    ((keyReleased ev)
        ;(printf "keyreleased %s \n" (ev:getKeyCode))
        (*currently-held-keys*:remove (ev:getKeyCode))
    )
    ((keyTyped ev) #!void)
))
;(glcanv:addMouseMotionListener (object (java.awt.event.MouseMotionListener)
;    ((mouseDragged ev) #!void)
;    ((mouseMoved ev)
;        (let ((dx (- (ev:getX) 320)) (dy (- (ev:getY) 240)))
;            (set! player-ship:rot (- (atan2 dy dx)))
;            (glcanv:repaint)
;        )
;    )
;))
(jf:add glcanv)
(jf:setVisible #t)
(define anim (com.jogamp.opengl.util.FPSAnimator glcanv 30))
(anim:start)
