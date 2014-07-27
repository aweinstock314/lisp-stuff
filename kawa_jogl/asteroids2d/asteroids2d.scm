(require <asteroids_util_general>)
(require <asteroids_util_math>)
(require <asteroids_util_opengl>)

(define *constant-buffer* '())
(define (make-constant-polygon poly::polygon) (inplace-polybuffer-append! *constant-buffer* poly))

;; GUI constants
(define-constant +screen-width+ 640)
(define-constant +screen-height+ 480)

; the real values are double this, since these are used as plus/minus
(define-constant +logical-width+ 4)
(define-constant +logical-height+ 4)
(define +viewport-width+ 3)
(define +viewport-height+ 3)

(define *show-extra-debugging-views* #f)
(define +window-width+ +screen-width+)
(define +window-height+ (if *show-extra-debugging-views* (* 1.5 +screen-height+) +screen-height+))

(define-simple-class drawer () interface: #t
    ((draw gl2::GL2 is::interface-state) #!abstract)
)

(define-constant +background-intensity+ .5)
(define-constant background (make-constant-polygon (calc-poly
    (/ tau 8) (constantly (sqrt (* 2 (square +logical-width+)))) 4
    (lambda (i) (case i
        ((0) (values +background-intensity+ +background-intensity+ 0))
        ((1) (values 0 +background-intensity+ +background-intensity+))
        ((2) (values 0 0 +background-intensity+))
        ((3) (values 0 +background-intensity+ 0))
    ))
)))

(define-constant white-bg (make-constant-polygon (calc-poly 0 (constantly 20) 4 (constantly (values 1 1 1)))))
(define-constant black-dot (make-constant-polygon (calc-poly 0 (constantly .01) 10 (constantly (values 0 0 0)))))
(define-constant blue-dot (make-constant-polygon (calc-poly 0 (constantly .01) 10 (constantly (values 0 0 1)))))

(define-constant +score-fmtstr+ "Score: %s")
(define-constant +lives-fmtstr+ "Lives: %s")
(define-constant +level-fmtstr+ "Level: %s")
(define-constant +initial-score+ 0)
(define-constant +initial-lives+ 3)
(define-constant +initial-level+ 1)

(define-constant +label-dimensions+ (java.awt.Dimension 128 32))
(define (initialize-label c::java.awt.Container label::javax.swing.JLabel x y)
    (label:setPreferredSize +label-dimensions+)
    (c:add label)
    (label:setBounds x y (+label-dimensions+:getWidth) (+label-dimensions+:getHeight))
)
(define (update-label label::javax.swing.JLabel fmt::String val::String)
    (label:setText (String:format fmt val))
    (label:validate)
    (label:repaint)
)

;; Game constants
(define-constant +cs-per-frame+ (/ 100 30)) ; centiseconds

(define-constant +shot-color+ '(1 1 1))
(define-constant +shot-speed+ (/ .2 +cs-per-frame+))
(define-constant +shot-size+ .01)
(define-constant +shot-momentum-factor+ 5)
(define-constant +shot-duration+ (* 45 +cs-per-frame+))
(define-constant +shot-vertidx+ (make-constant-polygon (calc-poly 0 (constantly +shot-size+) 10 (constantly (apply values +shot-color+)))))

(define-constant +centiseconds-between-shots+ (* 10 +cs-per-frame+))
(define-constant +min-ship-speed+ (/ -0.09 +cs-per-frame+))
(define-constant +max-ship-speed+ (/ 0.10 +cs-per-frame+))

(define-constant +min-asteroid-size+ .1)
(define-constant +max-asteroid-size+ .5)
(define-constant +min-asteroid-ivel+ (/ .01 +cs-per-frame+))
(define-constant +max-asteroid-ivel+ (/ .05 +cs-per-frame+))
(define +asteroid-speed-multiplier+ 1)
(define-constant +initial-asteroids-count+ 5)
(define-constant +asteroids-per-level+ 5)

(define-constant +respawn-box-vertidx+ (make-constant-polygon (calc-poly (/ tau 8) (constantly (sqrt 2)) 4 (constantly (values 0 .25 .25)))))
(define-constant push-outside-respawn-safety-box (push-outside -1 -1 2 2))

; these have to do with input sensitivity, not sure if they belong here or in GUI constants section
(define-constant +rotation-delta+ (/ (/ tau 64) +cs-per-frame+))
(define-constant +velocity-delta+ (/ .01 (* 2 +cs-per-frame+)))

;; Game objects
(define-simple-class shot (drawer)
    (x::double 0) (y::double 0)
    (rot::double 0)
    (velocity::double 0)
    (centiseconds-until-decay::int +shot-duration+)
    ((*init* ix iy irot ivel)
        (set! x ix)
        (set! y iy)
        (set! rot irot)
        (set! velocity (+ ivel +shot-speed+)) ; shots start off with the ship's velocity added to the constant
    )
    ((updatePosition!)
        (inc! centiseconds-until-decay -1)
        (set-values! (x y) (apply-polar-movement x y velocity rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((expired?) (< centiseconds-until-decay 0))
    ((draw gl2 is) (drawPolygon gl2 is:cbuf x y rot +shot-vertidx+))
)

(define-simple-class ship (drawer)
    (x::double 0) (y::double 0)
    (rot::double (/ tau 4))
    (velocity::double 0)
    (size .1)
    (color '(1 .5 0))
    (shooting-cooldown::int 0)
    ((*init* is::interface-state) (recalcVerts! is) (set! rot (/ tau 4)))
    (vertidx)
    ((recalcVerts! is::interface-state) (set! vertidx (is:add-ship-poly (calc-poly 0 (lambda (i) (if (= i 0) (* 2 size) size)) 3 (constantly (apply values color)))))) ; isosceles triangle
    ((rotate delta::double) (inc! rot delta))
    ((getVertidx) vertidx)
    ((updatePosition!)
        (if (> shooting-cooldown 0) (inc! shooting-cooldown -1))
        (set-values! (x y) (apply-polar-movement x y velocity rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((draw gl2 is) (drawPolygon gl2 is:cbuf x y rot vertidx))
    ((shoot! gs::game-state)
        (gs:active-shots:add (shot x y rot velocity))
        (set! shooting-cooldown +centiseconds-between-shots+)
    )
    ((resetPosition&Momentum!)
        (set!* (x y rot velocity) (0 0 (/ tau 4) 0))
    )
)

(define-simple-class asteroid (drawer)
    (x::double (random-range (- +logical-width+) +logical-width+))
    (y::double (random-range (- +logical-height+) +logical-height+))
    (rot::double (random tau))
    (velocity::double (random-range +min-asteroid-ivel+ +max-asteroid-ivel+))
    (size (random-range +min-asteroid-size+ +max-asteroid-size+))
    (color (list (random-range .25 .65) 0 0))
    (vertidx #!null)
    (children::ArrayList[asteroid] #!null)
    (dx::double 0) (dy::double 0) ; deltas from center of generated polygon, for children
    ((constructor-helper is::interface-state poly::polygon) access: 'private
        (set! vertidx (is:add-asteroid-poly poly))
        (set! children (ArrayList-map (cut asteroid is <>) (divide-poly poly)))
        (receive (xs ys) (ArrayList-foldl
                (lambda (acc v::vertex) (receive (xs ys) acc (values (cons v:x xs) (cons v:y ys))))
                poly:verts
                (values '() '())
            )
            (set!* (dx dy) ((apply average xs) (apply average ys)))
        )
    )
    ((*init* is::interface-state)
        (constructor-helper is (calc-poly (random tau) (constantly size) (random-range 3 9) (constantly (apply values color))))
    )
    ((*init* is::interface-state poly::polygon) (constructor-helper is poly))

    ((draw gl2 is) (drawPolygon gl2 is:cbuf x y rot vertidx))
    ((updatePosition!)
        (set-values! (x y) (apply-polar-movement x y (* +asteroid-speed-multiplier+ velocity) rot))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((split gs::game-state r v) ; takes rotation and velocity of incoming shot, for momentum calculation
        ;(printf "splitting asteroid at (%f, %f), %d children\n" x y (children:size))
        (inc! gs:score 1)
        (java-iterate children c
            (set!* (c:x c:y) (x y))
            ; maybe add some factor based on child centers, instead of the random factor?
            (define randfactor (random-range (/ (- tau) 16) (/ tau 16)))
            (define c1 (polar->cart (values (* v +shot-momentum-factor+ +shot-size+) r)))
            (define c2 (polar->cart (values (* velocity size) rot)))
            (receive (m t) (cart->polar (cart+ c1 c2))
                (set!* (c:velocity c:rot) ((/ m size) (ensure-proper-angle (+ randfactor t))))
            )
            ((field gs 'active-asteroids):add c)
        )
    )
)

(define-simple-class game-state ()
    ((*init* is::interface-state)
        (set! interface is)
        (set! player-ship (ship interface))
    )
    (interface::interface-state #!null)
    (player-ship::ship #!null) ; change to array to support same-world multiplayer?
    (active-shots::ArrayList[shot] (ArrayList))
    (active-asteroids::ArrayList[asteroid] (ArrayList))
    (respawn-box-active #t)
    (score +initial-score+)
    (lives +initial-lives+)
    (level +initial-level+)
    (scorelabel (javax.swing.JLabel (String:format +score-fmtstr+ +initial-score+)))
    (liveslabel (javax.swing.JLabel (String:format +lives-fmtstr+ +initial-lives+)))
    (levellabel (javax.swing.JLabel (String:format +level-fmtstr+ +initial-level+)))

    (buffer-needs-reset #f)
    (eventloop-render-mutex (java.lang.Object))

    ((spawn-asteroids! amount::integer)
        (pascal-for (i 0 amount 1) (active-asteroids:add (asteroid interface)))
    )
    ((player-death!)
        (player-ship:resetPosition&Momentum!)
        (set! respawn-box-active #t)
        (inc! lives -1)
        (if (< lives 0)
            (begin
                (printf "Game over, no lives left.\nFinal score was %s.\n" score) 
                (java.lang.System:exit 0)
            )
            (printf "Lives-remaining: %s\n" lives)
        )
    )
    ((level-cleared!)
        (printf "Level %s cleared! Making %s new asteroids!\n" level (* level +asteroids-per-level+))
        (inc! level 1)
        (player-ship:resetPosition&Momentum!)
        (active-shots:clear)
        (set! respawn-box-active #t)
        (set! interface:asteroids-polybuf '())
        (spawn-asteroids! (* level +asteroids-per-level+))
        (set! buffer-needs-reset #t)
    )
)
(define-alias HashSet java.util.HashSet)
(define-simple-class interface-state ()
    (displayed-victory-message #f)
    (currently-held-keys::HashSet (HashSet))
    (shader-program ::int 0)
    (whole-area-matrix::PMVMatrix #!null)
    (whole-area-dims::int[] #!null)
    (recent-mouse-obj-coords (values 0 0))
    (cbuf::concatenated-buffer)
    (ships-polybuf '())
    (asteroids-polybuf '())

    ((add-ship-poly p::polygon) (inplace-polybuffer-append! ships-polybuf p))
    ((add-asteroid-poly p::polygon) (inplace-polybuffer-append! asteroids-polybuf p))

    ((reset-polygons-buffer! gl2::GL2)
            (set! cbuf (concatenate-buffers (append *constant-buffer* ships-polybuf asteroids-polybuf)))
            (set-polygons-buffer gl2 cbuf)
            (set! shader-program (make-shader-program gl2 (file-as-string-constant "identityshader.vert") (file-as-string-constant "identityshader.frag")))
            (define pos-attrib ::int (gl2:glGetAttribLocation shader-program "position"))
            (gl2:glVertexAttribPointer pos-attrib 3 gl2:GL_FLOAT #f 24 0)
            (gl2:glEnableVertexAttribArray pos-attrib)
            (define color-attrib ::int (gl2:glGetAttribLocation shader-program "color"))
            (gl2:glVertexAttribPointer color-attrib 3 gl2:GL_FLOAT #f 24 12)
            (gl2:glEnableVertexAttribArray color-attrib)
            (gl2:glUseProgram shader-program)
    )
    ((mousecoords->objcoords x y)
        (if (or (equal? #!null whole-area-matrix)
                (equal? #!null whole-area-dims))
            (values 0 0)
            (begin
                (define output (float[] length: 3))
                ; the subtraction is because AWT has the origin at the upper-left, OpenGL has it at bottom-left
                (whole-area-matrix:gluUnProject x (- +window-height+ y) 0 whole-area-dims 0 output 0)
                (values (output 0) (output 1))
            )
        )
    )
)

(define (closest-asteroid-to-point asteroids x y)::asteroid
    (define (sqr-dist a::asteroid)
        (define dx (- (- a:x a:dx) x))
        (define dy (- (- a:y a:dy) y))
        (define sd (+ (* dx dx) (* dy dy))) ; workaround for "java.lang.Double cannot be cast to gnu.math.Quantity"
        ;(printf "squaredist between (%s, %s) and (%s, %s) is %s\n" x y a:x a:y sd)
        (cons a sd)
    )
    ; this type of transform is probably rather inefficient without deforestation and inlining, revise later
    (car (ArrayList-foldl (lambda (acc elem) (if (< (cdr acc) (cdr elem)) acc elem))
            (ArrayList-map sqr-dist asteroids)
            (cons #!null Integer:MAX_VALUE)
        )
    )
)

(define (asteroids-near-point asteroids x y thresh)
    (define (sqr-dist a::asteroid)
        (define dx (- a:x x))
        (define dy (- a:y y))
        (define sd (+ (* dx dx) (* dy dy))) ; workaround for "java.lang.Double cannot be cast to gnu.math.Quantity"
        ;(printf "squaredist between (%s, %s) and (%s, %s) is %s\n" x y a:x a:y sd)
        (cons a sd)
    )
    (define sqrthresh (* thresh thresh))
    ; this type of transform is probably rather inefficient without deforestation and inlining, revise later
    (ArrayList-foldl (lambda (acc elem) (if (< (cdr elem) sqrthresh) (cons (car elem) acc) acc))
        (ArrayList-map sqr-dist asteroids)
        '()
    )
)

(define (split-if-closest-asteroid-overlaps-point! gs::game-state is::interface-state x y rot vel)
    (define a (closest-asteroid-to-point gs:active-asteroids x y))
    (if (equal? a #!null) #f
        (if (inside-poly? is:cbuf a:vertidx a:x a:y a:rot x y)
            (begin
                (a:split gs rot vel)
                (gs:active-asteroids:remove a)
                #t
            )
            #f
        )
    )
)

(define (split-if-nearby-asteroid-overlaps-point! gs::game-state is::interface-state x y rot vel)
    (call-with-current-continuation (lambda (break)
        (for-each (lambda (a::asteroid)
            (when (inside-poly? is:cbuf a:vertidx a:x a:y a:rot x y)
                (a:split gs rot vel)
                (gs:active-asteroids:remove a)
                (break #t)
            )
        ) (asteroids-near-point gs:active-asteroids x y .5))
        (break #f)
    ))
)

(define (event-loop gs::game-state is::interface-state)
    (define-macro (key-held? key)
        `(invoke is:currently-held-keys 'contains (static-field KeyEvent ,key))
    )
    (unless (equal? is:cbuf #!null)
    (if (key-held? 'VK_LEFT) ((field gs 'player-ship):rotate +rotation-delta+))
    (if (key-held? 'VK_RIGHT) ((field gs 'player-ship):rotate (- +rotation-delta+)))
    (if (key-held? 'VK_UP) (inc! (field gs 'player-ship):velocity +velocity-delta+))
    (if (key-held? 'VK_DOWN) (inc! (field gs 'player-ship):velocity (- +velocity-delta+)))
    (inplace! (clamp +min-ship-speed+ +max-ship-speed+) (field gs 'player-ship):velocity)
    (if (and (key-held? 'VK_SPACE) (= (field gs 'player-ship):shooting-cooldown 0))
        ((field gs 'player-ship):shoot! gs)
    )
    ((field gs 'player-ship):updatePosition!)
    (java-iterate gs:active-shots (s shot iter)
        (s:updatePosition!)
        (if (or (split-if-nearby-asteroid-overlaps-point! gs is s:x s:y s:rot s:velocity) (s:expired?))
            (iter:remove)
        )
    )
    (java-iterate gs:active-asteroids (a asteroid)
        (a:updatePosition!)
        (if gs:respawn-box-active (set-values! (a:x a:y) (push-outside-respawn-safety-box a:x a:y)))
        (if (inside-poly? is:cbuf a:vertidx a:x a:y a:rot (field gs 'player-ship):x (field gs 'player-ship):y)
            (gs:player-death!)
        )
    )
    (when (and (gs:active-asteroids:isEmpty) (not is:displayed-victory-message))
        (printf "All asteroids have been cleared!\n")
        (gs:level-cleared!)
    )
    ;(printf "pos: %s, %s\n" player-ship:x player-ship:y)
    (update-label gs:scorelabel +score-fmtstr+ gs:score)
    (update-label gs:liveslabel +lives-fmtstr+ gs:lives)
    (update-label gs:levellabel +level-fmtstr+ gs:level)
    )
)

(define (render gl2::GL2 gs::game-state is::interface-state width::integer height::integer)
    (when gs:buffer-needs-reset
        (is:reset-polygons-buffer! gl2)
        (set! gs:buffer-needs-reset #f)
    )
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
    (define-constant (draw-background gl2::GL2)
        (drawPolygon gl2 is:cbuf 0 0 0 background)
    )
    (define-constant (draw-foreground gl2 cx cy)
        (if gs:respawn-box-active (drawPolygon gl2 is:cbuf 0 0 0 +respawn-box-vertidx+))
        (gs:player-ship:draw gl2 is)
        (define-macro (draw-if-within-bounds x)
            `(when (and (< (abs (- (field ,x 'x) cx)) +viewport-width+)
                        (< (abs (- (field ,x 'y) cy)) +viewport-height+))
                ;(printf "drawing something at (%s, %s)\n" (field ,x 'x) (field ,x 'y))
                (invoke ,x 'draw gl2 is)
            )
        )
        ;(printf "\n-----\n")
        (java-iterate gs:active-shots (s shot) (draw-if-within-bounds s))
        (java-iterate gs:active-asteroids (a asteroid) (draw-if-within-bounds a))
    )
    (define-constant (draw-whole-area gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 +logical-width+ +logical-height+)
        (prepare-frame gl2 0 0 0 0)
        (draw-background gl2)
        (draw-foreground gl2 0 0)
        (if (equal? is:whole-area-dims #!null)
            (set! is:whole-area-dims (int[] x y w h))
        )
        (if (equal? is:whole-area-matrix #!null)
            (set! is:whole-area-matrix (getPMVMatrix gl2))
        )
    )
    (define-constant (draw-shipview gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 +viewport-width+ +viewport-height+)
        ; display 4 copies (in positions based on the quadrant of the ship) to ensure that objects on the wrapped sides appear properly
        (define px (field gs 'player-ship):x)
        (define py (field gs 'player-ship):y)
        (define min-x (if (> px 0) -1 0))
        (define max-x (+ (if (> px 0) 0 1) 1))
        (define min-y (if (> py 0) -1 0))
        (define max-y (+ (if (> py 0) 0 1) 1))
        ;(printf "(%s, %s) -> (%s, %s), (%s, %s)\n" player-ship:x player-ship:y min-x min-y max-x max-y)
        (pascal-for (i min-x max-x 1) (pascal-for (j min-y max-y 1)
            (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
                (prepare-frame gl2 px py ox oy)
                (draw-background gl2)
            )
        ))
        (pascal-for (i min-x max-x 1) (pascal-for (j min-y max-y 1)
            (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
                (prepare-frame gl2 px py ox oy)
                (draw-foreground gl2 (+ px ox) (+ py oy))
            )
        ))
    )
    (define-constant (draw-debugging-poly gl2 x y w h)
        (gl2:glViewport x y w h)
        (set-projection gl2 1 1)
        (drawPolygon gl2 is:cbuf 0 0 0 white-bg)
        (receive (px py) is:recent-mouse-obj-coords
            ;(printf "recent mouse coords (%s, %s)\n" px py)
            (define a (closest-asteroid-to-point gs:active-asteroids px py))
            (unless (equal? a #!null)
                ;(printf "asteroid loc (%s, %s)\n" a:x a:y)
                (drawPolygon gl2 is:cbuf 0 0 0 a:vertidx)
                (define xy (untranslate/rotate a:x a:y a:rot px py))
                ;(printf "translated pt (%s, %s)\n" (xy 0) (xy 1))
                (drawPolygon gl2 is:cbuf (xy 0) (xy 1) 0 black-dot)
                (define (getter vertidx offset)
                    (is:cbuf:buf:position (* (is:cbuf:offset vertidx) 6))
                    (is:cbuf:buf:position (+ (is:cbuf:buf:position) offset))
                    (is:cbuf:buf:get)
                )
                (draw-dotted-line gl2 is:cbuf blue-dot 0 0 (getter a:vertidx 0) (getter a:vertidx 1))
            )
        )
    )
    (if *show-extra-debugging-views*
        (begin
            (draw-whole-area gl2 0 (- +window-height+ +screen-height+) +screen-width+ +screen-height+)
            (draw-debugging-poly gl2 (/ +screen-width+ 2) 0 (/ +screen-width+ 2) (/ +screen-height+ 2))
            (draw-shipview gl2 0 0 (/ +screen-width+ 2) (/ +screen-height+ 2))
        )
        (draw-shipview gl2 0 (- +window-height+ height) width height)
    )
)

(define-simple-class asteroids-panel (javax.swing.JPanel javax.media.opengl.GLEventListener java.awt.event.KeyListener java.awt.event.MouseListener java.awt.event.MouseMotionListener java.awt.event.MouseWheelListener)
    (gamestate::game-state)
    (interfacestate::interface-state)
    ; GLEventListener
    ((display drawable) (synchronized gamestate:eventloop-render-mutex (print-exceptions (render ((drawable:getGL):getGL2) gamestate interfacestate (getWidth) (getHeight)))))
    ((init drawable)
        (let ((gl (javax.media.opengl.DebugGL2 ((drawable:getGL):getGL2))))
            ;(gl:glEnableClientState gl:GL_VERTEX_ARRAY)
            (interfacestate:reset-polygons-buffer! gl)
        )
    )
    ((dispose drawable) #!void)
    ((reshape drawable x y w h) #!void)
    ; KeyListener
    ((keyPressed ev)
        (set! gamestate:respawn-box-active #f) ; only provide until a key is pressed
;;        ; split a random asteroid, for testing purposes, when 's' is pressed
;;        (when (equal? (ev:getKeyCode) KeyEvent:VK_S)
;;            (define a (*active-asteroids* (random (*active-asteroids*:size))))
;;            (a:split 0 0)
;;            (*active-asteroids*:remove a)
;;        )
;;        (when (equal? (ev:getKeyCode) KeyEvent:VK_Q) (inc! +asteroid-speed-multiplier+ -0.1))
;;        (when (equal? (ev:getKeyCode) KeyEvent:VK_W) (inc! +asteroid-speed-multiplier+  0.1))
;;        (inplace! (clamp 0 1) +asteroid-speed-multiplier+)
        (interfacestate:currently-held-keys:add (ev:getKeyCode))
    )
    ((keyReleased ev)
        (interfacestate:currently-held-keys:remove (ev:getKeyCode))
    )
    ((keyTyped ev) #!void)
    ; MouseListener
    ((mouseClicked ev)
        (unless (equal? interfacestate:whole-area-matrix #!null)
            (receive (x y) (interfacestate:mousecoords->objcoords (ev:getX) (ev:getY))
                (printf "mouse clicked at (%s, %s) window coords (%s, %s) object coords\n" (ev:getX) (ev:getY) x y)
                (split-if-closest-asteroid-overlaps-point! gamestate interfacestate x y 0 0)
            )
        )
    )
    ; consider making a macro that generates these stubs for all methods of an interface, except for ones written
    ((mouseEntered ev) #!void)
    ((mouseExited ev) #!void)
    ((mousePressed ev) #!void)
    ((mouseReleased ev) #!void)
    ; MouseMotionListener
    ((mouseMoved ev)
        (set! interfacestate:recent-mouse-obj-coords (interfacestate:mousecoords->objcoords (ev:getX) (ev:getY)))
        ;(display *recent-mouse-obj-coords*) (newline)
    )
    ((mouseDragged ev) #!void)
    ; MouseWheelListener
    ((mouseWheelMoved ev)
        (define delta (* .5 (ev:getWheelRotation)))
        (inc! +viewport-width+ delta)
        (inc! +viewport-height+ delta)
        (inplace! (cut max .5 <>) +viewport-width+)
        (inplace! (cut max .5 <>) +viewport-height+)
        ;(printf "(%s, %s)\n" +viewport-width+ +viewport-height+)
    )
    (glcanv::GLCanvas #!null)
    (anim::FPSAnimator #!null)
    ((*init* w::integer h::integer) #!void
        (set! interfacestate (interface-state))
        (set! gamestate (game-state interfacestate))
        (gamestate:spawn-asteroids! +initial-asteroids-count+)
        (setLayout #!null)
        (initialize-label (this) gamestate:scorelabel 0 0)
        (initialize-label (this) gamestate:liveslabel (- w (+label-dimensions+:getWidth)) 0)
        (initialize-label (this) gamestate:levellabel (- w (+label-dimensions+:getWidth)) (+label-dimensions+:getHeight))
        ; according to gnu.bytecode.dump, this doesn't get unrolled (a PairWithPosition literal is consed up at module init, and the code that uses that literal cdrs through it, and uses gnu.kawa.reflect.Invoke to execute the symbols)
        (set! glcanv (GLCanvas))
        (for-each (lambda (method) (invoke glcanv method (this)))
            '(addGLEventListener addKeyListener addMouseListener addMouseMotionListener addMouseWheelListener))
        (add glcanv)
        (glcanv:setBounds 0 0 w h)
        (setBounds 0 0 w h)
    )
    ((post-visible-initialization!)
        (glcanv:requestFocus)
        (set! anim (FPSAnimator glcanv 30))
        (anim:start)
        (future (with-min-ms-per-iteration 10 (synchronized gamestate:eventloop-render-mutex (print-exceptions (event-loop gamestate interfacestate)))))
    )
)

(define *have-second-player* #f)

(define jf (javax.swing.JFrame))
(define *asteroids-panel*::asteroids-panel (asteroids-panel +screen-width+ +screen-height+))
(define ap2::asteroids-panel #!null)
(when *have-second-player*
    (set! ap2 (asteroids-panel +screen-width+ +screen-height+))
    (ap2:setBounds +screen-width+ 0 +screen-width+ +screen-height+)
)
(jf:setSize (if *have-second-player* (* +window-width+ 2) +window-width+) +window-height+)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(jf:setLayout #!null)
(jf:add *asteroids-panel*)
(when *have-second-player* (jf:add ap2))
(jf:setVisible #t)
(*asteroids-panel*:post-visible-initialization!)
(when *have-second-player* (ap2:post-visible-initialization!))
