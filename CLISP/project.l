; -------------------
; DEPENDENCIES
; -------------------

; I don't know if I'll need this yet but we'll stick it here just in case
( load "lp.l" )
 

; -------------------
; GLOBALS
; -------------------

( setf *username* "User" )

; This will get changed to something more robust later
( setf *room-objects* '( WINDOW SWITCH-PANEL STATUS-SCREEN BUTTON-PANEL CUPBOARD DRAWER ) )


; -------------------
; STARTUP
; -------------------

; Terminal startup display inspired by that of the ROBCO terminals in the Fallout series
( defun startup ()
    ( terpri )
    ( write-line "SURGELOCK LABORATORIES INTERACTIVE SPACECRAFT TERMINAL" )
    ( write-line "COPYRIGHT 2023 ALL RIGHTS RESERVED" )
    ( terpri )
    ( write-line "ENTER help FOR A LIST OF COMMANDS." )

    ( command-reader )
)


; -------------------
; COMMAND READERS
; -------------------

; For a proper "simulation" we'll stick logic outside the AI system into here
( defun command-reader ()
    ( format t ">>> " ) ( setf prompt ( read ) )
    ( cond 
        ( ( string-equal prompt "start" )
            ( format t "BEGINNING AI SESSION." ) ( sleep 1 ) ( format t "." ) ( sleep 1) ( format t ".~%" ) ( sleep 1 )
            ( terpri )
            ( write-line "----------------------------------------")
            ( write-line "-- AI INTERACTIVE SESSION COMMENCED" )
            ( write-line "----------------------------------------")
            ( terpri )
            ( ai-command-reader t )
        )
        ( ( string-equal prompt "help" )
            ( write-line "help  -   Get a list of available commands")
            ( write-line "start -   Begin interactive terminal session with AI agent")
            ( write-line "exit  -   Exit the system")
            ( command-reader )
        )
        ( ( string-equal prompt "exit" )
            ( write-line "EXITING SYSTEM..." )
        )
        ( t
            ( write-line "PLEASE ENTER A VALID COMMAND.  ENTER help FOR A LIST OF COMMANDS." )
            ( command-reader )
        )
    )
)

; Handles commands specific to the AI
( defun ai-command-reader ( initial-session )

    ( cond
        ( initial-session
            ( write-line "Hello, and welcome to the AI Interaction Terminal." )
        )
    )

    ( terpri )
    ( format t ">>> " ) ( setf prompt ( read ) )

    ( cond
        ( ( string-equal prompt "exit")
            ( write-line "Exiting interaction.  Have a great day.")
            ( terpri )
        )
        ( ( string-equal prompt "look" )
            ( write-line "Here's what's in the room right now: " )

            ;TODO: This display is gonna get more complicated, and when it does we'll move it out
            ( format t "~A~%" *room-objects* )
            ( ai-command-reader nil )
        )
        ( t
            ( write-line "Sorry, I didn't understand that." )
            ( ai-command-reader nil )
        )
    )

)


( startup )