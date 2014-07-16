(require <asteroids_util>)

(define-constant +screen-width+ 640)
(define-constant +screen-height+ 480)

; the real values are double this, since these are used as plus/minus
(define-constant +logical-width+ 4)
(define-constant +logical-height+ 4)
(define +viewport-width+ 3)
(define +viewport-height+ 3)

(define-simple-class drawer () interface: #t
    ((draw gl2::GL2) #!abstract)
)

(define-constant +shot-color+ '(1 1 1))
(define-constant +shot-speed+ .1)
(define-constant +shot-size+ .01)
(define-constant +shot-momentum-factor+ 5)
(define-constant +shot-duration+ 45)
(define-constant +shot-vertidx+ (append-polygon-to-global-buffer (calc-poly 0 (constantly +shot-size+) 10 (constantly (apply values +shot-color+)))))

(define-simple-class shot (drawer)
    (x::double 0) (y::double 0)
    (rot::double 0)
    (velocity::double 0)
    (frames-until-decay::int +shot-duration+)
    ((*init* ix iy irot ivel)
        (set! x ix)
        (set! y iy)
        (set! rot irot)
        (set! velocity (+ ivel +shot-speed+)) ; shots start off with the ship's velocity added to the constant
    )
    ((updatePosition!)
        (inc! frames-until-decay -1)
        (set-values! (x y) (apply-polar-movement x y velocity rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((expired?) (< frames-until-decay 0))
    ((draw gl2) (drawPolygon gl2 x y rot +shot-vertidx+))
)

(define-constant +frames-between-shots+ 10)
(define-constant +min-ship-speed+ -0.09)
(define-constant +max-ship-speed+ 0.10)
(define *active-shots*::ArrayList[shot] (ArrayList))

(define-simple-class ship (drawer)
    (x::double 0) (y::double 0)
    (rot::double (/ tau 4))
    (velocity::double 0)
    (size .1)
    (color '(1 .5 0))
    (shooting-cooldown::int 0) ; in frames for now, probably should make more robust by handling milliseconds
    ((*init*) (recalcVerts!) (set! rot (/ tau 4)))
    (vertidx)
    ((recalcVerts!) (set! vertidx (append-polygon-to-global-buffer (calc-poly 0 (lambda (i) (if (= i 0) (* 2 size) size)) 3 (constantly (apply values color)))))) ; isosceles triangle
    ((rotate delta::double) (inc! rot delta))
    ((getVertidx) vertidx)
    ((updatePosition!)
        (if (> shooting-cooldown 0) (inc! shooting-cooldown -1))
        (set-values! (x y) (apply-polar-movement x y velocity rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((draw gl2) (drawPolygon gl2 x y rot vertidx))
    ((shoot)
        (*active-shots*:add (shot x y rot velocity))
        (set! shooting-cooldown +frames-between-shots+)
    )
)

(define-constant +min-asteroid-size+ .1)
(define-constant +max-asteroid-size+ .5)
(define-constant +min-asteroid-ivel+ .01)
(define-constant +max-asteroid-ivel+ .05)
(define +asteroid-speed-multiplier+ 1)

(define-simple-class asteroid (drawer)
    (x::double (random-range (- +logical-width+) +logical-width+))
    (y::double (random-range (- +logical-height+) +logical-height+))
    (rot::double (random tau))
    (velocity::double (random-range +min-asteroid-ivel+ +max-asteroid-ivel+))
    (size (random-range +min-asteroid-size+ +max-asteroid-size+))
    (color (list (random-range .25 .65) 0 0))
    (vertidx #!null)
    (children::ArrayList[asteroid] #!null)
    ((constructor-helper poly::polygon) access: 'private
        (set! vertidx (append-polygon-to-global-buffer poly))
        (set! children (ArrayList-map asteroid (divide-poly poly)))
    )
    ((*init*)
        (constructor-helper (calc-poly (random tau) (constantly size) (random-range 3 9) (constantly (apply values color))))
    )
    ((*init* poly::polygon) (constructor-helper poly))

    ((draw gl2) (drawPolygon gl2 x y rot vertidx))
    ((updatePosition!)
        (set-values! (x y) (apply-polar-movement x y (* +asteroid-speed-multiplier+ velocity) rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((split r v) ; takes rotation and velocity of incoming shot, for momentum calculation
        (printf "splitting asteroid at (%f, %f), %d children\n" x y (children:size))
        (java-iterate children c
            (set!* (c:x c:y) (x y))
            ; maybe add some factor based on child centers, instead of the random factor?
            (define randfactor (random-range (/ (- tau) 16) (/ tau 16)))
            (define c1 (polar->cart (values (* v +shot-momentum-factor+ +shot-size+) r)))
            (define c2 (polar->cart (values (* velocity size) rot)))
            (receive (m t) (cart->polar (cart+ c1 c2))
                (set!* (c:velocity c:rot) ((/ m size) (ensure-proper-angle (+ randfactor t))))
            )
            (*active-asteroids*:add c)
        )
    )
)

(define *active-asteroids*::ArrayList[asteroid] (ArrayList))
(pascal-for (i 0 10 1) (*active-asteroids*:add (asteroid)))

(define (closest-asteroid-to-point x y)::asteroid
    (define (sqr-dist a::asteroid)
        (define dx (- a:x x))
        (define dy (- a:y y))
        (define sd (+ (* dx dx) (* dy dy))) ; workaround for "java.lang.Double cannot be cast to gnu.math.Quantity"
        ;(printf "squaredist between (%s, %s) and (%s, %s) is %s\n" x y a:x a:y sd)
        (cons a sd)
    )
    ; this type of transform is probably rather inefficient without deforestation and inlining, revise later
    (car (ArrayList-foldl (lambda (acc elem) (if (< (cdr acc) (cdr elem)) acc elem))
            (ArrayList-map sqr-dist *active-asteroids*)
            (cons #!null Integer:MAX_VALUE)
        )
    )
)

(define (split-if-closest-asteroid-overlaps-point! x y rot vel)
    (define a (closest-asteroid-to-point x y))
    (if (equal? a #!null) #f
        (if (inside-poly? *polygons-buffer* a:vertidx a:x a:y a:rot x y)
            (begin
                (a:split rot vel)
                (*active-asteroids*:remove a)
                #t
            )
            #f
        )
    )
)

(define player-ship (ship))

(define jf (javax.swing.JFrame))
(define *show-extra-debugging-views* #f)
(define +window-width+ +screen-width+)
(define +window-height+ (if *show-extra-debugging-views* (* 1.5 +screen-height+) +screen-height+))
(jf:setSize +window-width+ +window-height+)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(define glcanv (javax.media.opengl.awt.GLCanvas))

(define *currently-held-keys* (java.util.HashSet))
(define displayed-victory-message #f)
(define-constant +rotation-delta+ (/ tau 64))
(define-constant +velocity-delta+ .01)
(define (event-loop)
    (define-macro (key-held? key) `(*currently-held-keys*:contains (static-field KeyEvent ,key)))
    (if (key-held? 'VK_LEFT) (player-ship:rotate +rotation-delta+))
    (if (key-held? 'VK_RIGHT) (player-ship:rotate (- +rotation-delta+)))
    (if (key-held? 'VK_UP) (inc! player-ship:velocity +velocity-delta+))
    (if (key-held? 'VK_DOWN) (inc! player-ship:velocity (- +velocity-delta+)))
    (inplace! (clamp +min-ship-speed+ +max-ship-speed+) player-ship:velocity)
    (if (and (key-held? 'VK_SPACE) (= player-ship:shooting-cooldown 0)) (player-ship:shoot))
    (player-ship:updatePosition!)
    (java-iterate *active-shots* (s shot iter)
        (s:updatePosition!)
        (if (or (split-if-closest-asteroid-overlaps-point! s:x s:y s:rot s:velocity) (s:expired?))
            (iter:remove)
        )
    )
    (java-iterate *active-asteroids* (a asteroid)
        (a:updatePosition!)
    )
    (when (and (*active-asteroids*:isEmpty) (not displayed-victory-message))
        (printf "All asteroids have been cleared!\n")
        ;(javax.swing.JOptionPane:showMessageDialog #!null "All asteroids have been cleared!")
        (set! displayed-victory-message #t)
        ;(exit 0)
    )
    ;(printf "pos: %s, %s\n" player-ship:x player-ship:y)
)

(define-constant +background-intensity+ .5)


(define-constant background (append-polygon-to-global-buffer (calc-poly
    (/ tau 8) (constantly (sqrt (* 2 (square +logical-width+)))) 4
    (lambda (i) (case i
        ((0) (values +background-intensity+ +background-intensity+ 0))
        ((1) (values 0 +background-intensity+ +background-intensity+))
        ((2) (values 0 0 +background-intensity+))
        ((3) (values 0 +background-intensity+ 0))
    ))
)))

(define-constant white-bg (append-polygon-to-global-buffer (calc-poly 0 (constantly 20) 4 (constantly (values 1 1 1)))))
(define-constant black-dot (append-polygon-to-global-buffer (calc-poly 0 (constantly .01) 10 (constantly (values 0 0 0)))))
(define-constant blue-dot (append-polygon-to-global-buffer (calc-poly 0 (constantly .01) 10 (constantly (values 0 0 1)))))
(define (draw-dotted-line gl2 dot x1 y1 x2 y2)
    (let* ( (density 20)
            (dx (- x2 x1))
            (dy (- y2 y1))
            (dist (java.lang.Math:sqrt (+ (* dx dx) (* dy dy))))
            (slope (atan2 dy dx))
          )
        (pascal-for (i 0 density 1)
            (define j (/ i density))
            (drawPolygon gl2 (+ x1 (* j dist (cos slope))) (+ y1 (* j dist (sin slope))) 0 blue-dot)
        )
    )
)

(define (draw-background gl2::GL2)
    (drawPolygon gl2 0 0 0 background)
)

(define *whole-area-matrix*::PMVMatrix #!null)
(define *whole-area-dims*::int[] #!null)
(define *recent-mouse-obj-coords* (values 0 0))

(define (render gl2::GL2)
    (event-loop) ; might be a good idea to move this out of render later
    (gl2:glClear gl2:GL_COLOR_BUFFER_BIT)
    (define-constant (set-projection gl2::GL2 width::double height::double)
        (gl2:glMatrixMode gl2:GL_PROJECTION)
        (gl2:glLoadIdentity)
        (gl2:glOrtho (- width) width 
                     (- height) height
                     1 -1)
;        (define proj-matrix (int[] length: 16))
;        (gl2:glGetIntegerv gl2:GL_PROJECTION_MATRIX proj-matrix 0)
;        (display proj-matrix) (newline)
    )
    (define-constant (prepare-frame gl2::GL2 cx cy ox oy) ; center and offset
        (gl2:glMatrixMode gl2:GL_MODELVIEW)
        (gl2:glLoadIdentity)
        (gl2:glTranslated (- cx) (- cy) 0)
        (gl2:glTranslated (- ox) (- oy) 0)
    )
    (define-constant (draw-foreground gl2)
        (player-ship:draw gl2)
        (java-iterate *active-shots* (s shot) (s:draw gl2))
        (java-iterate *active-asteroids* (a asteroid) (a:draw gl2))
    )
    (define-constant (draw-whole-area gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 +logical-width+ +logical-height+)
        (prepare-frame gl2 0 0 0 0)
        (draw-background gl2)
        (draw-foreground gl2)
        (if (equal? *whole-area-dims* #!null) (set! *whole-area-dims* (int[] x y w h)))
        (if (equal? *whole-area-matrix* #!null) (set! *whole-area-matrix* (getPMVMatrix gl2)))
    )
    (define-constant (draw-shipview gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 +viewport-width+ +viewport-height+)
        ; display 4 copies (in positions based on the quadrant of the ship) to ensure that objects on the wrapped sides appear properly
        (define min-x (if (> player-ship:x 0) -1 0))
        (define max-x (+ (if (> player-ship:x 0) 0 1) 1))
        (define min-y (if (> player-ship:y 0) -1 0))
        (define max-y (+ (if (> player-ship:y 0) 0 1) 1))
        ;(printf "(%s, %s) -> (%s, %s), (%s, %s)\n" player-ship:x player-ship:y min-x min-y max-x max-y)
        (pascal-for (i min-x max-x 1) (pascal-for (j min-y max-y 1)
            (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
                (prepare-frame gl2 player-ship:x player-ship:y ox oy)
                (draw-background gl2)
            )
        ))
        (pascal-for (i min-x max-x 1) (pascal-for (j min-y max-y 1)
            (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
                (prepare-frame gl2 player-ship:x player-ship:y ox oy)
                (draw-foreground gl2)
            )
        ))
    )
    (define-constant (draw-debugging-poly gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 1 1)
        (drawPolygon gl2 0 0 0 white-bg)
        (receive (px py) *recent-mouse-obj-coords*
            ;(printf "recent mouse coords (%s, %s)\n" px py)
            (define a (closest-asteroid-to-point px py))
            ;(printf "asteroid loc (%s, %s)\n" a:x a:y)
            (drawPolygon gl2 0 0 0 a:vertidx)
            (define xy (untranslate/rotate a:x a:y a:rot px py))
            ;(printf "translated pt (%s, %s)\n" (xy 0) (xy 1))
            (drawPolygon gl2 (xy 0) (xy 1) 0 black-dot)
            (define (getter vertidx offset)
                (*polygons-buffer*:position (* (car vertidx) 6))
                (*polygons-buffer*:position (+ (*polygons-buffer*:position) offset))
                (*polygons-buffer*:get)
            )
            (draw-dotted-line gl2 blue-dot 0 0 (getter a:vertidx 0) (getter a:vertidx 1))
        )
    )
    (if *show-extra-debugging-views*
        (begin
            (draw-whole-area gl2 0 (- +window-height+ +screen-height+) +screen-width+ +screen-height+)
            (draw-debugging-poly gl2 (/ +screen-width+ 2) 0 (/ +screen-width+ 2) (/ +screen-height+ 2))
            (draw-shipview gl2 0 0 (/ +screen-width+ 2) (/ +screen-height+ 2))
        )
        (draw-shipview gl2 0 (- +window-height+ +screen-height+) +screen-width+ +screen-height+)
    )
)

(define *shader-program* ::int 0)

(glcanv:addGLEventListener (object (javax.media.opengl.GLEventListener)
    ((*init*) #!void)
    ((display drawable) (render ((drawable:getGL):getGL2)))
    ((init drawable)
        (let ((gl (javax.media.opengl.DebugGL2 ((drawable:getGL):getGL2))))
            ;(gl:glEnableClientState gl:GL_VERTEX_ARRAY)
            (set-polygons-buffer gl)
            (set! *shader-program* (make-shader-program gl (file-as-string-constant "identityshader.vert") (file-as-string-constant "identityshader.frag")))
            (define pos-attrib ::int (gl:glGetAttribLocation *shader-program* "position"))
            (gl:glVertexAttribPointer pos-attrib 3 gl:GL_FLOAT #f 24 0)
            (gl:glEnableVertexAttribArray pos-attrib)
            (define color-attrib ::int (gl:glGetAttribLocation *shader-program* "color"))
            (gl:glVertexAttribPointer color-attrib 3 gl:GL_FLOAT #f 24 12)
            (gl:glEnableVertexAttribArray color-attrib)
            (gl:glUseProgram *shader-program*)
        )
    )
    ((dispose drawable) #!void)
    ((reshape drawable x y w h) #!void)
))

(glcanv:addKeyListener (object (java.awt.event.KeyListener)
    ((keyPressed ev)
        ; split a random asteroid, for testing purposes, when 's' is pressed
        (when (equal? (ev:getKeyCode) KeyEvent:VK_S)
            (define a (*active-asteroids* (random (*active-asteroids*:size))))
            (a:split 0 0)
            (*active-asteroids*:remove a)
        )
        (when (equal? (ev:getKeyCode) KeyEvent:VK_Q) (inc! +asteroid-speed-multiplier+ -0.1))
        (when (equal? (ev:getKeyCode) KeyEvent:VK_W) (inc! +asteroid-speed-multiplier+  0.1))
        (inplace! (clamp 0 1) +asteroid-speed-multiplier+)
        (*currently-held-keys*:add (ev:getKeyCode))
    )
    ((keyReleased ev)
        (*currently-held-keys*:remove (ev:getKeyCode))
    )
    ((keyTyped ev) #!void)
))

(define (mousecoords->objcoords x y)
    (if (or (equal? #!null *whole-area-matrix*) (equal? #!null *whole-area-dims*))
        (values 0 0)
        (begin
            (define output (float[] length: 3))
            ; the subtraction is because AWT has the origin at the upper-left, OpenGL has it at bottom-left
            (*whole-area-matrix*:gluUnProject x (- +window-height+ y) 0 *whole-area-dims* 0 output 0)
            (values (output 0) (output 1))
        )
    )
)
(glcanv:addMouseListener (object (java.awt.event.MouseListener)
    ((mouseClicked ev)
        (unless (equal? *whole-area-matrix* #!null)
            (receive (x y) (mousecoords->objcoords (ev:getX) (ev:getY))
                (printf "mouse clicked at (%s, %s) window coords (%s, %s) object coords\n" (ev:getX) (ev:getY) x y)
                (split-if-closest-asteroid-overlaps-point! x y 0 0)
            )
        )
    )
    ; consider making a macro that generates these stubs for all methods of an interface, except for ones written
    ((mouseEntered ev) #!void)
    ((mouseExited ev) #!void)
    ((mousePressed ev) #!void)
    ((mouseReleased ev) #!void)
))
(glcanv:addMouseMotionListener (object (java.awt.event.MouseMotionListener)
    ((mouseMoved ev)
        (set! *recent-mouse-obj-coords* (mousecoords->objcoords (ev:getX) (ev:getY)))
        ;(display *recent-mouse-obj-coords*) (newline)
    )
    ((mouseDragged ev) #!void)
))
(glcanv:addMouseWheelListener (object (java.awt.event.MouseWheelListener)
    ((mouseWheelMoved ev)
        (define delta (* .5 (ev:getWheelRotation)))
        (inc! +viewport-width+ delta)
        (inc! +viewport-height+ delta)
        (inplace! (cut max .5 <>) +viewport-width+)
        (inplace! (cut max .5 <>) +viewport-height+)
        ;(printf "(%s, %s)\n" +viewport-width+ +viewport-height+)
    )
))
(jf:add glcanv)
(jf:setVisible #t)
(define anim (com.jogamp.opengl.util.FPSAnimator glcanv 30))
(anim:start)
(glcanv:requestFocus)
