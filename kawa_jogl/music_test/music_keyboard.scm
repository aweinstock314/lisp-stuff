(require <scheme_util_general>)
(require <scheme_util_math>)

(define-constant window-size 100)

(set-variables-from-cmdline
    ( ((#\h "help") (svfc-display-all-options) (java.lang.System:exit 0)) )
    (+metronome-tempo+ -1 (#\m "metronome") Integer:parseInt)
)

(define synth::javax.sound.midi.Synthesizer (javax.sound.midi.MidiSystem:getSynthesizer))
(synth:open)

(define-macro (with-channel synthesizer name num . body)
    `(let ((,name :: javax.sound.midi.MidiChannel ((invoke ,synthesizer 'getChannels) ,num)))
        ,@body
    )
)

(define-alias HashSet java.util.HashSet)
(define-alias HashMap java.util.HashMap)
(define-alias Long java.lang.Long)

(define get-time java.lang.System:nanoTime)

(define-simple-class MouseEventRepeatFilter (java.awt.event.KeyListener)
    (forwardee::java.awt.event.KeyListener) ; eventually change to collection, have add/remove for observers?
    (last-presses::HashMap[Integer Long] (HashMap))
    (last-releases::HashMap[Integer Long] (HashMap))
    (threshhold::Long 50000000) ; experimentally determined constant
    ((*init* fw) (set! forwardee fw))
    ((keyPressed ev)
        (let* ((now (get-time))
               (last-press (aif/nn (last-presses:put (ev:getKeyCode) now) it 0))
               (last-release (aif/nn (last-releases:get (ev:getKeyCode)) it 0))
               (last-time (max last-press last-release))
            )
            (if (> (- now last-time) threshhold)
                (forwardee:keyPressed ev))
        )
    )
    ((keyReleased ev)
        (let* ((now (get-time))
               (last-time (aif/nn (last-releases:put (ev:getKeyCode) now) it 0)))
            (if (> (- now last-time) threshhold)
                (forwardee:keyReleased ev))
        )
    )
    ((keyTyped ev) #!void)
)

(define-macro (make-inorder-keymap startval delta #!rest (keys::symbol[]))
    (define (sym->keycode sym::symbol)
        (define symname (symbol->string sym))
        (define fieldname
            (if (= (symname:length) 1)
                (String:format "VK_%s" symname)
                symname
            )
        )
        (static-field java.awt.event.KeyEvent (string->symbol fieldname))
    )
    `(returning (keymap (HashMap))
        ,@(accumulate-range (i 0 keys:length 1)
            `(invoke keymap 'put ,(sym->keycode (keys i)) ,(+ (* i delta) startval))
        )
    )
)

(define-simple-class keyboard-panel (javax.swing.JPanel java.awt.event.KeyListener)
    (channel::javax.sound.midi.MidiChannel)
    (currently-held-keys::HashSet (HashSet))
    (notes-table::HashMap;[Integer Integer]
        (make-inorder-keymap 40 2
            Z X C V B N M VK_COMMA VK_PERIOD VK_SLASH
            A S D F G H J K L VK_SEMICOLON VK_QUOTE
            Q W E R T Y U I O P VK_OPEN_BRACKET VK_CLOSE_BRACKET
        )
    )
    ((*init* syn::javax.sound.midi.Synthesizer)
        (with-channel syn ch 0 (set! channel ch))
    )
    ((keyPressed ev)
        (printf "Pressed: %s, %d\n" (ev:getKeyCode) (get-time))
        (currently-held-keys:add (ev:getKeyCode))
        (aif/nn (notes-table:get (ev:getKeyCode))
            (channel:noteOn it #x7f)
        )
    )
    ((keyReleased ev)
        (printf "Released: %s, %d\n" (ev:getKeyCode) (get-time))
        (currently-held-keys:remove (ev:getKeyCode))
        (aif/nn (notes-table:get (ev:getKeyCode))
            (channel:noteOff it #x7f)
        )
    )
    ((keyTyped ev) #!void)
)

(define metronome-note 0)
(define metronome-sweeper (sweep 35 38 100))
(when (> +metronome-tempo+ 0)
    (future (with-channel synth ch 9
        (with-min-ms-per-iteration +metronome-tempo+
            (ch:noteOff metronome-note)
            (inplace! metronome-sweeper metronome-note)
            (ch:noteOn metronome-note #x7f)
        )
    ))
)

(define jf (javax.swing.JFrame))
(define kp (keyboard-panel synth))
(jf:addKeyListener (MouseEventRepeatFilter kp))
(kp:setBounds 0 0 window-size window-size)
(jf:setSize window-size window-size)
(jf:setResizable #f)
(jf:setDefaultCloseOperation javax.swing.JFrame:EXIT_ON_CLOSE)
(jf:setLayout #!null)
(jf:add kp)
(jf:setVisible #t)
(jf:requestFocus)
(kp:repaint)

(future (with-min-ms-per-iteration 500
    (java-iterate kp:currently-held-keys key
        (when (invoke kp:currently-held-keys 'contains key)
            (write key)
            (newline)
        )
    )
    (display "-----") (newline)
))
