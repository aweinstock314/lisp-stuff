(require 'list-lib)

(define-macro (with-all-forms-exported . forms)
    (letrec ((helper (lambda (form acc)
                (if (not (list? form)) acc
                (case (car form)
                    ((define-alias define-simple-class) (cons (cadr form) acc))
                    ((define define-constant define-macro)
                        (cons (cond
                            ((pair? (cadr form)) (caadr form))
                            ((symbol? (cadr form)) (cadr form))
                            (else (error "Unexpected type as the second element of a define form"))
                        ) acc)
                    )
                    (else acc)
                ))
            )))
        (define export-list (fold helper '() forms))
        (display "Automagically exporting the following from with-all-forms-exported: ") (newline)
        (display export-list) (newline)
        `(begin ,@forms ,@(if (not (null? export-list)) `((module-export ,@export-list)) '()))
    )
)
(module-export with-all-forms-exported)

(with-all-forms-exported

(define printf java.lang.System:out:printf)
(define-alias GL2 javax.media.opengl.GL2)
(define-alias KeyEvent java.awt.event.KeyEvent)
(define-alias ArrayList java.util.ArrayList)
(define-alias Number java.lang.Number)
(define-alias FloatBuffer java.nio.FloatBuffer)
(define-constant newDirectFloatBuffer com.jogamp.common.nio.Buffers:newDirectFloatBuffer)

(define-macro (thunk . body) `(lambda (. ,(gentemp)) ,@body))
(define-macro (mvbind vars expr . body) `(call-with-values (thunk ,expr) (lambda ,vars ,@body)))
(define-macro (mvlist expr) `(call-with-values (thunk ,expr) list))

; the first way has multiple-evaluation problems, but works fine for simple cases
; the second way (commented), should be the right way, but generates bytecode that gives verify errors unless extra annotations are done at the call site
(define-macro (inc! var delta)
    `(set! ,var (+ ,var ,delta))
    ;(let ((loc (gentemp))) `(let ((,loc (location ,var))) (set! (,loc) (+ (,loc) ,delta))))
)
(define-macro (inplace! fn var) `(set! ,var (,fn ,var)))

(define-macro (java-iterate iterable-expr varname . body)
    (define real-varname (if (list? varname) (car varname) varname))
    (define vartype (if (list? varname) (cadr varname) java.lang.Object))
    (define iterator-name (if (and (list? varname) (>= (length varname) 3)) (caddr varname) (gentemp)))
    `(let ((,iterator-name ::java.util.Iterator (invoke ,iterable-expr 'iterator))
           (,real-varname ::,vartype #!null))
        (do ()
            ((not (invoke ,iterator-name 'hasNext)) #!void)
            (set! ,real-varname (invoke ,iterator-name 'next))
            ,@body
        )
    )
)

(define-macro (pascal-for rng . body)
    (define var (car rng))
    (define lo (gentemp)) (define hi (gentemp)) (define step (gentemp))
    `(let ((,lo ,(cadr rng)) (,hi ,(caddr rng)) (,step ,(cadddr rng)))
        (do ((,var ,lo (+ ,var ,step)))
            ((= ,var ,hi) #!void)
            ,@body
        )
    )
)

(define-macro (accumulate-range rng . body)
    (define head (gentemp)) (define tail (gentemp))
    `(let* ((,head (cons '() '())) (,tail ,head))
        (pascal-for ,rng
            (set! (cdr ,tail) (cons (begin ,@body) (cdr ,tail)))
            (inplace! cdr ,tail)
        )
        (cdr ,head)
    )
)

(define-macro (curry expr)
    (define argsname (gentemp))
    `(lambda (. ,argsname) (apply ,@expr ,argsname))
)
(define (complement pred) (lambda (. args) (not (apply pred args))))

(define (upto x) (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i x) (reverse! acc))))
(define (clamp lo hi) (lambda (val) (max lo (min hi val))))
(define (wrap lo hi) (lambda (val)
    (cond ((< val lo) hi)
          ((> val hi) lo)
          (else val)
    )
))
(define (within? lo hi) (lambda (val::Number)::boolean (<= lo val hi)))

(define (constantly x) (lambda (. args) x))
(define tau (* 8 (atan 1)))
(define atan2 java.lang.Math:atan2)
(define (rad->deg r) (/ (* r 360) tau))
(define (random x) (* x (java.lang.Math:random)))
(define (random-range lo hi) (+ lo (* (- hi lo) (java.lang.Math:random))))

(define-simple-class vertex ()
    (x::double 0) (y::double 0)
    ((*init*) #!void)
    ((*init* ix::double iy::double)
        (set! x ix)
        (set! y iy)
    )
    ((toString)::String (String:format "c(%f, %f)" x y))
)

(define-simple-class polar-vertex ()
    (m::double 0) (t::double 0)
    ((*init*) #!void)
    ((*init* im::double it::double)
        (set! m im)
        (set! t it)
    )
    ((toString)::String (String:format "p(%f, %f)" m t))
)

(define (cart->polar v::vertex) (polar-vertex (sqrt (+ (square v:x) (square v:y))) (atan2 v:y v:x)))
(define (polar->cart p::polar-vertex) (vertex (* p:m (cos p:t)) (* p:m (sin p:t))))
(define (cart+ v1::vertex v2::vertex) (vertex (+ v1:x v2:x) (+ v1:y v2:y)))

; returns a list of vertices of a "regular" polygon, with the first vertex at rot radians
; the radius parameter is a function to allow non-regular polygons such as isosceles triangles
(define *polygons-buffer* ::FloatBuffer (newDirectFloatBuffer 0))
(define (calc-poly rot::double radiusf::gnu.mapping.Procedure sides::int)
    (let* ((old-capacity (*polygons-buffer*:capacity))
           (new-buffer (newDirectFloatBuffer (+ old-capacity (* 3 sides))))
        )
        (*polygons-buffer*:rewind)
        (new-buffer:put *polygons-buffer*)
        (pascal-for (i 0 sides 1)
            (let* ((rad::double (radiusf i))
                   (ang::double (+ rot (* tau (/ i sides))))
                   (vx::float (* rad (cos ang)))
                   (vy::float (* rad (sin ang)))
                )
                (new-buffer:put vx) (new-buffer:put vy) (new-buffer:put 0)
            )
        )
        (set! *polygons-buffer* new-buffer)
        (cons (/ old-capacity 3) sides) ; return an (offset, size) pair for glDrawArrays to use
    )
)

(define *vbo-pointers* ::int[] (int[] length: 1))

(define (set-polygons-buffer gl2::GL2)
;    (*polygons-buffer*:rewind)
;    (gl2:glVertexPointer 3 gl2:GL_FLOAT 0 *polygons-buffer*)
    (gl2:glGenBuffers 1 *vbo-pointers* 0)
    (gl2:glBindBuffer gl2:GL_ARRAY_BUFFER (*vbo-pointers* 0))
    (*polygons-buffer*:rewind)
    ; the multiplication by 4 is because glBufferData takes a size in bytes
    (gl2:glBufferData gl2:GL_ARRAY_BUFFER (* (*polygons-buffer*:capacity) 4) *polygons-buffer* gl2:GL_DYNAMIC_DRAW)
)

(define (shader-valid? gl2::GL2 shader-id)
    (define tmp (int[] 0))
    (gl2:glGetShaderiv shader-id gl2:GL_COMPILE_STATUS tmp 0)
    (tmp 0)
)

(define (print-shader-info-log gl2::GL2 id::int)
    (define returned-size (int[] 0))
    (define data (byte[] length: 512))
    (gl2:glGetShaderInfoLog id 512 returned-size 0 data 0)
    (if (> (returned-size 0) 0) (printf "%s\n\n" (String data)))
)

(define (glShaderSource gl2::GL2 id::int src::String)
    (gl2:glShaderSource id 1 (String[] src) (int[] (src:length)) 0)
)

(define (make-shader-program gl2::GL2 vertex-source::String fragment-source::String)
    (let ((vert-shader-id (gl2:glCreateShader gl2:GL_VERTEX_SHADER))
           (frag-shader-id (gl2:glCreateShader gl2:GL_FRAGMENT_SHADER))
           (shader-program (gl2:glCreateProgram))
        )
        (printf "vertex source: %s\n\n" vertex-source)
        (printf "fragment source: %s\n\n" fragment-source)
        (glShaderSource gl2 vert-shader-id vertex-source)
        (glShaderSource gl2 frag-shader-id fragment-source)
        (gl2:glCompileShader vert-shader-id)
        (gl2:glCompileShader frag-shader-id)
        (print-shader-info-log gl2 vert-shader-id)
        (print-shader-info-log gl2 frag-shader-id)
        (printf "shader validity: %s, %s\n" (shader-valid? gl2 vert-shader-id) (shader-valid? gl2 frag-shader-id))
        (gl2:glAttachShader shader-program vert-shader-id)
        (gl2:glAttachShader shader-program frag-shader-id)
        (gl2:glLinkProgram shader-program)
        shader-program
    )
)

(define (drawPolygon gl2::GL2 x y rot color verts)
    (gl2:glMatrixMode gl2:GL_MODELVIEW)
    (gl2:glPushMatrix)
    (gl2:glLoadIdentity)
    (gl2:glTranslated x y 0)
    (gl2:glRotated (rad->deg rot) 0 0 1)
    (apply gl2:glColor3d color)
    (gl2:glDrawArrays gl2:GL_POLYGON (car verts) (cdr verts))
    (gl2:glPopMatrix)
)

) ; end of with-all-forms-exported
