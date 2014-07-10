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
(define-macro (returning let-pair . body) `(let (,let-pair) ,@body ,(car let-pair)))
(define-macro (set!* vals exprs) `(begin ,@(map (lambda (val expr) `(set! ,val ,expr)) vals exprs)))
(define-macro (define-gensyms . names) `(begin ,@(map (lambda (name) `(define ,name (gentemp))) names)))

; the first way has multiple-evaluation problems, but works fine for simple cases
; the second way (commented), should be the right way, but generates bytecode that sometimes gives verify errors (revision 7952 fixed a simplified version of this error, which turned out to have been oversimplified)
(define-macro (inc! var delta)
    `(set! ,var (+ ,var ,delta))
    ;(let ((loc (gentemp))) `(let ((,loc (location ,var))) (set! (,loc) (+ (,loc) ,delta))))
)
(define-macro (inplace! fn var)
    `(set! ,var (,fn ,var))
    ;(let ((loc (gentemp))) `(let ((,loc (location ,var))) (set! (,loc) (,fn (,loc)))))
)

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
    (define-gensyms lo hi step)
    `(let ((,lo ,(cadr rng)) (,hi ,(caddr rng)) (,step ,(cadddr rng)))
        (do ((,var ,lo (+ ,var ,step)))
            ((= ,var ,hi) #!void)
            ,@body
        )
    )
)

(define-macro (with-list-collector col . body)
    (define-gensyms head tail elem)
    `(let* ((,head (cons '() '())) (,tail ,head)
            (,col (lambda (,elem)
                (set! (cdr ,tail) (cons ,elem (cdr ,tail)))
                (inplace! cdr ,tail)
           )))
        ,@body
        (cdr ,head)
    )
)

(define-macro (accumulate-range rng . body)
    (define-gensyms col)
    `(with-list-collector ,col
        (pascal-for ,rng
            (,col (begin ,@body))
        )
    )
)

(define (slurp-file filename::String)::String
    (define buf (java.lang.StringBuilder))
    (define file (open-input-file filename))
    (do ((line (read-line file 'concat) (read-line file 'concat)))
        ((equal? line '#!eof) (buf:toString))
        (buf:append line)
    )
)

; Would be nice to have CL's sharp-dot read-macro for this (so the callsite is just #.(slurp-file "foo"), and this extra macro is unneccessary)
(define-macro (file-as-string-constant filename) (slurp-file filename))

(define-macro (curry expr)
    (define-gensyms argsname)
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
(define (random x)::float (* x (java.lang.Math:random)))
(define (random-range lo hi)::float (+ lo (* (- hi lo) (java.lang.Math:random))))

(define-simple-class vertex ()
    (x::double 0) (y::double 0) (z::double 0)
    (r::double 0) (g::double 0) (b::double 0)
    ((*init*) #!void)
    ((*init* ix::double iy::double)
        (set! x ix)
        (set! y iy)
    )
    ((toString)::String (String:format "#<xyz(%f, %f, %f), rgb(%f, %f, %f)>" x y z r g b))
)

(define-simple-class polygon ()
    (verts::ArrayList[vertex] (ArrayList))
    ((*init*) #!void)
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
(define (calc-poly rot::double radiusf::gnu.mapping.Procedure sides::int colorf::gnu.mapping.Procedure)
    (returning (poly (polygon)) (pascal-for (i 0 sides 1)
        (let* ((rad::double (radiusf i))
               (ang::double (+ rot (* tau (/ i sides))))
            )
            (let-values (((r g b) (colorf i)))
                (invoke poly:verts 'add (returning (v (vertex))
                    (set!* (v:x v:y v:z) ((* rad (cos ang)) (* rad (sin ang)) 0))
                    (set!* (v:r v:g v:b) (r g b))
                ))
            )
        )
    ))
)

(define (append-polygon-to-buffer old-buffer::FloatBuffer poly::polygon)
    (let* ((old-capacity (old-buffer:capacity))
           (floats-per-vertex 6)
           (old-vertcount (/ old-capacity floats-per-vertex))
           (sides (invoke poly:verts 'size))
           (total-new-floats::int (+ old-capacity (* floats-per-vertex sides)))
           (new-buffer (newDirectFloatBuffer total-new-floats))
        )
        (old-buffer:rewind)
        (new-buffer:put old-buffer)
        (java-iterate poly:verts (v vertex)
            (new-buffer:put v:x)
            (new-buffer:put v:y)
            (new-buffer:put v:z)

            (new-buffer:put v:r)
            (new-buffer:put v:g)
            (new-buffer:put v:b)
        )
        ; return the new buffer, and an (offset, size) pair for glDrawArrays to use for the new poly
        (values new-buffer (cons old-vertcount sides))
    )
)

(define *polygons-buffer* ::FloatBuffer (newDirectFloatBuffer 0))
(define (append-polygon-to-global-buffer poly::polygon)
    (let-values (((buf ind) (append-polygon-to-buffer *polygons-buffer* poly)))
        (set! *polygons-buffer* buf)
        ind
    )
)

(define *vbo-pointers* ::int[] (int[] length: 1))

(define (set-polygons-buffer gl2::GL2)
;    (*polygons-buffer*:rewind)
;    (gl2:glVertexPointer 3 gl2:GL_FLOAT 0 *polygons-buffer*)
    (gl2:glGenBuffers 1 *vbo-pointers* 0)
    (gl2:glBindBuffer gl2:GL_ARRAY_BUFFER (*vbo-pointers* 0))
    (*polygons-buffer*:rewind)
    (let ((tmp (float[] length: (*polygons-buffer*:capacity))))
        (*polygons-buffer*:get tmp)
        (display tmp) (newline)
    )
    (*polygons-buffer*:rewind)
    ; the multiplication by 4 is because glBufferData takes a size in bytes
    (gl2:glBufferData gl2:GL_ARRAY_BUFFER (* (*polygons-buffer*:capacity) 4) *polygons-buffer* gl2:GL_DYNAMIC_DRAW)
)

(define (shader-compiled? gl2::GL2 shader-id)
    (define tmp (int[] 0))
    (gl2:glGetShaderiv shader-id gl2:GL_COMPILE_STATUS tmp 0)
    (if (= (tmp 0) 0) #f #t)
)

(define (print-shader-info-log gl2::GL2 id::int)
    (define returned-size (int[] 0))
    (define len (let ((tmp (int[] 0))) (gl2:glGetShaderiv id gl2:GL_INFO_LOG_LENGTH tmp 0) (tmp 0)))
    (define data (byte[] length: len))
    (gl2:glGetShaderInfoLog id len returned-size 0 data 0)
    (if (> (returned-size 0) 0) (printf "ShaderInfoLog for shader %d: %s\n\n" id (String data)))
)

(define (glShaderSource gl2::GL2 id::int src::String)
    (gl2:glShaderSource id 1 (String[] src) (int[] (src:length)) 0)
)

(define (compile-shader gl2::GL2 type::int source::String)
    (returning (shader-id (gl2:glCreateShader type))
        (printf "shader source:\n%s\n\n" source)
        (glShaderSource gl2 shader-id source)
        (gl2:glCompileShader shader-id)
        (when (not (shader-compiled? gl2 shader-id))
            (print-shader-info-log gl2 shader-id)
            (error "Failed to compile shader.")
        )
    )
)

(define (make-shader-program gl2::GL2 vertex-source::String fragment-source::String)
    (returning (shader-program (gl2:glCreateProgram))
        (gl2:glAttachShader shader-program (compile-shader gl2 gl2:GL_VERTEX_SHADER vertex-source))
        (gl2:glAttachShader shader-program (compile-shader gl2 gl2:GL_FRAGMENT_SHADER fragment-source))
        (gl2:glLinkProgram shader-program)
    )
)

(define (drawPolygon gl2::GL2 x y rot verts)
    (gl2:glMatrixMode gl2:GL_MODELVIEW)
    (gl2:glPushMatrix)
    (gl2:glTranslated x y 0)
    (gl2:glRotated (rad->deg rot) 0 0 1)
    (gl2:glDrawArrays gl2:GL_POLYGON (car verts) (cdr verts))
    (gl2:glPopMatrix)
)

) ; end of with-all-forms-exported
