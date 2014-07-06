(require "asteroids_util.scm")

(define-constant +screen-width+ 640)
(define-constant +screen-height+ 480)

; the real values are double this, since these are used as plus/minus
(define-constant +logical-width+ 4)
(define-constant +logical-height+ 4)
(define-constant +viewport-width+ 2)
(define-constant +viewport-height+ 2)

(define-simple-class drawer () interface: #t
    ((draw gl2::GL2) #!abstract)
)

(define-constant +shot-color+ '(255 255 255))
(define-constant +shot-speed+ .1)
(define-constant +shot-size+ .01)
(define-constant +shot-verts+ (calc-poly 0 (constantly +shot-size+) 10))

(define-simple-class shot (drawer)
    (x 0) (y 0)
    (rot 0)
    (velocity 0)
    ((*init* ix iy irot ivel)
        (set! x ix)
        (set! y iy)
        (set! rot irot)
        (set! velocity (+ ivel +shot-speed+)) ; shots start off with the ship's velocity added to the constant
    )
    ((updatePosition!)
        (set! x (+ x (* velocity (cos rot))))
        (set! y (+ y (* velocity (sin rot))))
    )
    ((draw gl2) (drawPolygon gl2 x y rot +shot-color+ +shot-verts+))
)

(define-constant +frames-between-shots+ 5)
(define-constant +min-ship-speed+ -0.09)
(define-constant +max-ship-speed+ 0.10)
(define *active-shots*::ArrayList[shot] (ArrayList))

(define-simple-class ship (drawer)
    (x 0) (y 0)
    (rot (/ tau 4))
    (velocity 0)
    (size .1)
    (color '(1 .5 0))
    (shooting-cooldown 0) ; in frames for now, probably should make more robust by handling milliseconds
    ((*init*) (recalcVerts!) (set! rot (/ tau 4)))
    (verts)
    ((recalcVerts!) (set! verts (calc-poly 0 (lambda (i) (if (= i 0) (* 2 size) size)) 3))) ; isosceles triangle
    ((rotate delta::double) (inc! rot delta))
    ((getVerts) verts)
    ((updatePosition!)
        (if (> shooting-cooldown 0) (inc! shooting-cooldown -1))
        (set! x (+ x (* velocity (cos rot))))
        (set! y (+ y (* velocity (sin rot))))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
    ((draw gl2) (drawPolygon gl2 x y rot color (getVerts)))
    ((shoot)
        (*active-shots*:add (shot x y rot velocity))
        (set! shooting-cooldown +frames-between-shots+)
    )
)

(define-constant +min-asteroid-size+ .1)
(define-constant +max-asteroid-size+ .5)
(define-constant +min-asteroid-ivel+ .01)
(define-constant +max-asteroid-ivel+ .05)

(define-simple-class asteroid (drawer)
    (x) (y) (rot) (velocity) (size) (color) (verts)
    ((*init*)
        (set! x (random-range (- +logical-width+) +logical-width+))
        (set! y (random-range (- +logical-height+) +logical-height+))
        (set! rot (random tau))
        (set! velocity (random-range +min-asteroid-ivel+ +max-asteroid-ivel+))
        (set! size (random-range +min-asteroid-size+ +max-asteroid-size+))
        (set! color (list (random-range .25 .65) 0 0))
        (set! verts (calc-poly (random tau) (constantly size) (random-range 3 9)))
    )
    ((draw gl2) (drawPolygon gl2 x y rot color verts))
    ((updatePosition!)
        (set! x (+ x (* velocity (cos rot))))
        (set! y (+ y (* velocity (sin rot))))
        (inplace! (wrap (- +logical-width+) +logical-width+) x)
        (inplace! (wrap (- +logical-height+) +logical-height+) y)
    )
)

(define *active-asteroids*::ArrayList[asteroid] (ArrayList))
(pascal-for (i 0 10 1) (*active-asteroids*:add (asteroid)))

(define player-ship (ship))

(define jf (javax.swing.JFrame))
(jf:setSize +screen-width+ +screen-height+)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(define glcanv (javax.media.opengl.awt.GLCanvas))

(define-constant in-bounds? (within? (- +logical-width+) +logical-width+))
(define-constant (out-of-bounds? x::java.lang.Number) (not (in-bounds? x)))

(define *currently-held-keys* (java.util.HashSet))
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
        (if (or (out-of-bounds? s:x) (out-of-bounds? s:y))
            (iter:remove))
    )
    (java-iterate *active-asteroids* (a asteroid)
        (a:updatePosition!)
    )
    ;(printf "pos: %s, %s\n" player-ship:x player-ship:y)
)

(define-constant +background-intensity+ .5)
(define (draw-background gl2::GL2)
    (gl2:glBegin GL2:GL_QUADS)
    (gl2:glColor3d +background-intensity+ 0 0)
    (gl2:glVertex2d (- +logical-width+) +logical-height+)
    (gl2:glColor3d +background-intensity+ +background-intensity+ 0)
    (gl2:glVertex2d +logical-width+ +logical-height+)
    (gl2:glColor3d 0 +background-intensity+ 0)
    (gl2:glVertex2d +logical-width+ (- +logical-height+))
    (gl2:glColor3d 0 +background-intensity+ +background-intensity+)
    (gl2:glVertex2d (- +logical-width+) (- +logical-height+))
    (gl2:glEnd)
)

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
    (define-constant (draw-frame gl2 cx cy ox oy)
        (prepare-frame gl2 cx cy ox oy)
        (draw-background gl2)
        (draw-foreground gl2)
    )
    (set-projection gl2 +logical-width+ +logical-height+)
    (gl2:glViewport 0 0 640 480)
    (draw-frame gl2 0 0 0 0)
    (set-projection gl2 +viewport-width+ +viewport-height+)
    (gl2:glViewport 0 0 320 240)
    ; display 9 copies to ensure that objects on the wrapped sides appear properly, a straightforward optimization (based on the quadrant of the ship) could probably cut it down to 4 copies, and there might be even better ideas
    (pascal-for (i -1 2 1) (pascal-for (j -1 2 1)
        (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
            (prepare-frame gl2 player-ship:x player-ship:y ox oy)
            (draw-background gl2)
        )
    ))
    (pascal-for (i -1 2 1) (pascal-for (j -1 2 1)
        (let ((ox (* 2 i +logical-width+)) (oy (* 2 j +logical-height+)))
            (prepare-frame gl2 player-ship:x player-ship:y ox oy)
            (draw-foreground gl2)
        )
    ))
)

(define *shader-program* ::int 0)

(glcanv:addGLEventListener (object (javax.media.opengl.GLEventListener)
    ((*init*) #!void)
    ((display drawable) (render ((drawable:getGL):getGL2)))
    ((init drawable)
        (let ((gl (javax.media.opengl.DebugGL2 ((drawable:getGL):getGL2))))
            ;(gl:glEnableClientState gl:GL_VERTEX_ARRAY)
            (set-polygons-buffer gl)
            (set! *shader-program* (make-shader-program gl
"
#version 120

//in vec3 position;
attribute vec3 position;
vec4 tmp;

void main()
{
    gl_FrontColor = gl_Color;
    tmp = vec4(position, 1.0);
    gl_Position = gl_ModelViewProjectionMatrix * tmp;
}
"

"
#version 120

//out vec4 outColor;

void main()
{
    gl_FragColor = gl_Color;
}
"
            ))
            (define pos-attrib ::int (gl:glGetAttribLocation *shader-program* "position"))
            (gl:glVertexAttribPointer pos-attrib 3 gl:GL_FLOAT #f 0 0)
            (gl:glEnableVertexAttribArray pos-attrib)
            (gl:glUseProgram *shader-program*)
        )
    )
    ((dispose drawable) #!void)
    ((reshape drawable x y w h) #!void)
))
(glcanv:addKeyListener (object (java.awt.event.KeyListener)
    ((keyPressed ev) 
        (*currently-held-keys*:add (ev:getKeyCode))
    )
    ((keyReleased ev)
        (*currently-held-keys*:remove (ev:getKeyCode))
    )
    ((keyTyped ev) #!void)
))
(jf:add glcanv)
(jf:setVisible #t)
(define anim (com.jogamp.opengl.util.FPSAnimator glcanv 30))
(anim:start)
