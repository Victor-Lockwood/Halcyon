( load "project.l" )

; -------------------
; TASK 2
; -------------
; This demo prints out room object information.
; -------------------

( defun demo--object-properties ()
    ( instantiate-room-objects )

    ( write-line " -- ROOM OBJECTS AND THEIR PROPERTIES -- " )
    ( terpri )

    ( setf window ( nth 0 *room-objects* ) )
    ( setf switch-panel ( nth 1 *room-objects* ) )
    ( setf status-screen ( nth 2 *room-objects* ) )
    ( setf cupboard ( nth 3 *room-objects* ) )
    ( setf drawer ( nth 4 *room-objects* ) )

    ( format t "NAME: ~A~%" ( get window 'name ) )
    ( format t "LOOK:  ~A~%" ( get window 'look ) )
    ( format t "OPEN:  ~A~%" ( get window 'open ) )
    ( format t "CLOSE:  ~A~%~%" ( get window 'close ) )

    ( format t "NAME: ~A~%" ( get switch-panel 'name ) )
    ( format t "LOOK: ~A~%" ( get switch-panel 'look ) )
    ( format t "IS-LOCKED: ~A~%" ( get switch-panel 'is-locked ) )
    ( format t "INTERACT-LOCKED: ~A~%" ( get switch-panel 'interact-locked ) )
    ( format t "CORRECT-SEQUENCE-CONTROL: ~A~%" ( get switch-panel 'correct-sequence-control ) )
    ( format t "CORRECT-SEQUENCE-LIE ~A~%" ( get switch-panel 'correct-sequence-lie ) )
    ( format t "INCORRECT-SEQUENCE: ~A~%~%" ( get switch-panel 'incorrect-sequence ) )

    ( format t "NAME: ~A~%" ( get status-screen 'name ) )
    ( format t "LOOK: ~A~%" ( get status-screen 'look ) )
    ( format t "LOOK-POSITIVE: ~A~%" ( get status-screen 'look-positive ) )
    ( format t "LOOK-POSITIVE2: ~A~%" ( get status-screen  'look-positive2 ) )
    ( format t "LOOK-POSITIVE3: ~A~%" ( get status-screen 'look-positive3 ) )
    ( format t "LOOK-WITHHOLD: ~A~%" ( get status-screen  'look-withhold ) )
    ( format t "LOOK-LIE: ~A~%~%" ( get status-screen  'look-lie ) )

    ( format t "NAME: ~A~%" ( get cupboard 'name ) )
    ( format t "LOOK: ~A~%" ( get cupboard 'look ) )
    ( format t "LOOK-POSITIVE: ~A~%" ( get cupboard 'look-positive ) )
    ( format t "LOOK-WITHHOLD: ~A~%" ( get cupboard  'look-withhold ) )
    ( format t "LOOK-LIE: ~A~%~%" ( get cupboard  'look-lie ) )

    ( format t "NAME: ~A~%" ( get drawer 'name ) )
    ( format t "IS-LOCKED: ~A~%" ( get drawer 'is-locked ) )
    ( format t "LOOK-LOCKED: ~A~%" ( get drawer 'look ) )
    ( format t "LOOK-UNLOCKED-NEUTRAL: ~A~%" ( get drawer 'look-unlocked-neutral ) )
    ( format t "LOOK-UNLOCKED-POSITIVE: ~A~%" ( get drawer 'look-unlocked-positive ) )
    ( format t "LOOK-UNLOCKED-WITHHOLD: ~A~%" ( get drawer 'look-unlocked-withhold ) )
    ( format t "LOOK-UNLOCKED-LIE: ~A~%~%" ( get drawer 'look-unlocked-lie ) )

    ( write-line " -- END DEMO -- " )
    nil 
)

; -------------------
; TASK 3
; -------------
; Looking at objects
; -------------------

( defun demo--object-look ()
    ( instantiate-room-objects )

    ( write-line " -- LOOKING AT ROOM OBJECTS - CONTROL MODE -- " )

    ( write-line "[ Attempting to 'look window' ]")
    ( format t "~A~%~%" (  look "look window" ) )

    ( write-line "[ Attempting to 'look at window' ]")
    ( format t "~A~%~%" (  look "look at window" ) )

    ( write-line "[ Attempting to 'look ggjhgjj window jhgyjgjh' ]")
    ( format t "~A~%~%" (  look "look ggjhgjj window jhgyjgjh" ) )

    ( write-line "[ Attempting to 'look drawer' ]")
    ( format t "~A~%~%" (  look "look drawer" ) )

    ( write-line "[ Attempting to 'look cupboard' ]")
    ( format t "~A~%~%" (  look "look cupboard" ) )

    ( write-line "[ Attempting to 'look status' ]")
    ( format t "~A~%~%" (  look "look status" ) )

    ( write-line "[ Attempting to 'look switch' ]")
    ( format t "~A~%~%" (  look "look switch" ) )

    ( write-line "[ Attempting to 'look nonex' ]")
    ( format t "~A~%~%" (  look "look nonex" ) )

    ( write-line " -- END DEMO -- " )

    nil
)