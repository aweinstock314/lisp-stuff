(require <asteroids_util_general>)
(require <asteroids_util_math>)

(define synth::javax.sound.midi.Synthesizer (javax.sound.midi.MidiSystem:getSynthesizer))
(synth:open)

(define-macro (with-channel synthesizer name num . body)
    `(let ((,name :: javax.sound.midi.MidiChannel ((invoke ,synthesizer 'getChannels) ,num)))
        ,@body
    )
)

(define (sweep lo hi step)
    (let ((curstep step) (in-bounds? (within? lo hi)) (enforce-bounds (clamp lo hi)))
        (lambda (x) (enforce-bounds (returning (tmp (+ x curstep))
            (if (not (in-bounds? tmp)) (set! curstep (* -1 curstep)))
        )))
    )
)

(pascal-for (i 0 3 1) (future (begin
    (define pitch 127)
    (define pitchsweeper (sweep 30 70 4))
    (define lastpitch 0)
    (define instr (->int (truncate (random 127))))
    (java.lang.Thread:sleep (* 5000 i))
    (with-channel synth ch i
        (with-min-ms-per-iteration (+ 500 (* 250 i))
            (ch:noteOff lastpitch)
            (printf "instrument %s; pitch %s\n" instr pitch)
            (ch:programChange instr)
            (synchronized pitch
                (set! lastpitch (pitchsweeper pitch))
                (set! pitch lastpitch)
            )
            ;(inc! instr 7)
            ;(inplace! (wrap 0 127) instr)
            (ch:noteOn lastpitch #x7f)
        )
    )
)))

(define pitch2 00)
(define sweeper2 (sweep 35 38 100))
(with-channel synth ch 9
    (with-min-ms-per-iteration 500
        (ch:noteOff pitch2)
        (inplace! sweeper2 pitch2)
        (ch:noteOn pitch2 #x7f)
    )
)
