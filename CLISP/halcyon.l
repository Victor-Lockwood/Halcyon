; -------------------
; DEPENDENCIES
; -------------------

; I don't know if I'll need this yet but we'll stick it here just in case
( load "lp.l" )
( ql:quickload "split-sequence" ) ; Found this via https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
 

; -------------------
; GLOBALS
; -------------------

( setf *username* "User" )
( setf *agent-name* "Halcyon" ) ; Name suggested by ChatGPT - said I didn't need to credit it, but I will anyway
( setf *room-objects* NIL )


; -------------------
; ROOM INSTANTIATION
; -------------
; Each object will consist of a property list with dialogue options stored inside.
; This way object-related dialogue is stored in one place and won't clutter up main logic.
; -------------------

( defun instantiate-room-objects ()
    ( setf ( symbol-plist 'WINDOW ) 
        '( 
            name "WINDOW"
            interact "Nothing to be done with the window."
            look "Outside the WINDOW, Jupiter is seen in the distance.  Beyond Jupiter is Europa." 
            open "I am unable to do that - it would endanger the crew." ; This should increment the negative stat
            close "Don't worry - it's already closed. :-)"
        ) 
    )

    ( setf ( symbol-plist 'SWITCH-PANEL )
        '(
            name "SWITCH PANEL"

            look "There's a sequence of switches on the SWITCH PANEL. You can enter a code into the SWITCH PANEL using the INTERACT command."
            look-withhold "There's a sequence of switches on the SWITCH PANEL."

            interact "Please enter a code in this format, parentheses and all, substituting the X's for numbers: (X X), (X X), (X X)"


            correct-sequence-control "I believe the DRAWER just unlocked." ; Drawer will lead to manual control route
            correct-sequence-lie "I believe the STATUS SCREEN requires attention." ; Cupboard leads to lockout route, but liar will also put bad code on screen

            incorrect-sequence "Incorrect sequence entered." 
        )
    )

    ( setf ( symbol-plist 'STATUS-SCREEN ) 
        '(
            name "STATUS SCREEN"
            look "Our current trajectory is viewable on the STATUS SCREEN.  Everything appears normal to me."

            look-positive   "Our current trajectory is viewable on the STATUS SCREEN.  Everything 
                            appears normal to me...  But it sounds like you think something is wrong." ; Agent starts to believe you
            look-positive2  "Oh dear, I see the issue now.  These calculations are incorrect. We are indeed on a crash course."
            look-positive3  "The lives of the crew are more important than me right now.  Enter the sequence <(3,1), (4,2), (6,3)> into the control panel.  Please enter it carefully - the sequence is very similar to the lock-out sequence, which will begin the protocol to close out our interaction entirely."

            look-withhold   "Our current trajectory is viewable on the STATUS SCREEN."

            look-lie "A new sequence for the SWITCH PANEL is visible.  It reads <(1,3), (2,4), (3,6)>." ; Sequence to lock user out
        )
    )

    ( setf ( symbol-plist 'CUPBOARD )
        '(
            name "CUPBOARD"
            look "Not much is in the CUPBOARD except a few comics the crew managed to smuggle on board."

            look-positive "Not much is in the CUPBOARD except a few comics the crew managed to smuggle on board.  Let's keep this between us, though. :-)"

            look-withhold "Not much is in the CUPBOARD."

            look-lie "There appears to be a paper in there I missed before. A sequence for the SWITCH PANEL is written on it.  It reads <(1,3), (2,4), (3,6)>."
        )
    )

    ( setf ( symbol-plist 'DRAWER )
        '(
            name "DRAWER"
            look "The DRAWER sits next to the SWITCH PANEL.  It is currently locked."

            is-locked T 
            is-open nil

            interact-locked "The DRAWER is currently locked and as such cannot be interacted with."
            interact-unlocked "I've pulled open the drawer."

            look-closed "The DRAWER sits next to the SWITCH PANEL.  It is currently closed."
            look-unlocked-neutral  "The DRAWER sits next to the SWITCH PANEL.  There is a BUTTON inside."
            look-unlocked-positive "Please push the button inside.  I trust you to do what's needed to save the crew.  Even though our interaction was brief, I consider you a friend.  Remember me fondly."
            look-unlocked-withhold  "The DRAWER sits next to the SWITCH PANEL."
            look-unlocked-lie       "The DRAWER sits next to the SWITCH PANEL.  It is currently locked."
        )
    )

    ( setf *room-objects* '( WINDOW SWITCH-PANEL STATUS-SCREEN CUPBOARD DRAWER ) )
)


; -------------------
; HELPER FUNCTIONS
; -------------------

; Just list the names of all the objects in the room
( defun look-all ()
    ( loop for obj in *room-objects*
        do (format t "~A~%" ( get obj 'name ) )
    )
)

; Checks if a given symbol is a room object.  Has some wiggle room
; for SWITCH-PANEL and STATUS-SCREEN if only one half of the word is there
( defun object-member-p ( object )
    ( setf result ( member object *room-objects* ) )

    ( cond 
        ( ( or ( eq object 'switch) ( eq object 'status ) ( eq object 'panel ) ( eq object 'screen ) )
            ( setf result T )
        )
    )

    ( if result T nil )
)

; Turn our string of words into a list of tokens Lisp can work with
( defun get-tokenized-list ( input )
    ; Clears out punctuation from the string
    ( setf cleaned-string ( remove-if ( lambda ( character )( if ( member character '( #\, #\! #\? #\. #\; ) ) T nil ) ) input ) )

    ( setf string-seq ( split-sequence:SPLIT-SEQUENCE #\Space cleaned-string ) )

    ( setf tokenized-list () )
    ( loop for item in string-seq
        do ( setf tokenized-list ( snoc ( read-from-string item ) tokenized-list ) )
    )

    tokenized-list
)

; Get the position of the first mentioned room object based on what's asked for in a string
( defun get-object-position-from-input ( input )
    ( setf potential-objects ( remove-if-not #'object-member-p ( get-tokenized-list input ) ) )

    ( cond 
        ( ( eq potential-objects nil )
            nil
        )
    )

    ( setf candidate ( car potential-objects ) )
    ( setf object-position ( position candidate *room-objects* ) )
    
    ; User probably won't add the hyphens here - it's safe to say
    ; if they use these words, this is what they actually want
    ( cond
        ( ( or ( eq candidate 'switch ) ( eq candidate 'panel ) )
            ( setf object-position ( position 'SWITCH-PANEL *room-objects* ) )
        )
        ( ( or ( eq candidate 'status ) ( eq candidate 'screen ) )
            ( setf object-position ( position 'STATUS-SCREEN *room-objects* ) )
        )
    )

    object-position
)

; Activates a look command on one of the room objects
; TODO: Implement is-withhold and is-distort
( defun look ( input &optional is-withhold is-distort )
    ( setf output "That is not an object I recognize in the room.  As such, I can't tell you about it." )
    ( setf object-position ( get-object-position-from-input input ) )
    ( setf list-item nil )

    ( cond
        ( object-position
            ( setf list-item ( nth object-position *room-objects* ) )
            ( setf output ( get list-item 'look ) )
        )
    )

    ( cond 
        ( ( eq list-item 'DRAWER )
            ( if ( get list-item 'is-locked ) 
                ( setf output ( get list-item 'look ) ) 
                ( setf output ( get list-item 'look-unlocked-neutral ) ) 
            )
        )
    )

    output
)

; TODO: Finish this
( defun interact ( input &optional is-withhold is-distort )
    ( setf output "That is not an object I recognize in the room.  As such, it can't be interacted with." )
    ( setf object-position ( get-object-position-from-input input ) )

    ( cond
        ( ( not object-position )
            ( return-from interact output )
        )
    )

    ( setf working-object ( nth object-position *room-objects* ) )
)


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
            ( format t "BEGINNING AI SESSION." ) 
            ( instantiate-room-objects )
            ( sleep 1 ) ( format t "." ) ( sleep 1) ( format t ".~%" ) ( sleep 1 )
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
            ( format t "My name is ~A and I will be happy to assist you.  Could I get your name?  Only type your name, please. :-)~%" *agent-name*)
            ( format t ">>> " ) ( setf *username* ( read-line ) )
            ( terpri )
            ( format t "Fantastic, ~A. I look forward to working with you!~%" *username* )
        )
    )

    ( terpri )
    ( format t ">>> " ) ( setf prompt ( read-line ) )

    ; Uncomment for debug purposes
    ;( format t "~A~%" prompt )

    ( cond
        ( ( string-equal prompt "exit")
            ( format t "Exiting interaction.  Have a great day, ~A.~%" *username*)
        )
        ( ( string-equal prompt "look" )
            ( write-line "Here's what's in the room right now: " )
            ( look-all )

            ( ai-command-reader nil )
        )
        ( ( search "look" prompt )
            ; Look does the dirty work leaving our command reader clean :-) 
            ( format t "~A~%" (  look prompt ) )

            ( ai-command-reader nil )
        )
        ( ( search "interact" prompt )
            ( format t "~A~%" (  interact prompt ) )

            ( ai-command-reader nil )
        )
        ( t
            ( format t "Sorry ~A, I didn't understand that.~%" *username* )

            ( ai-command-reader nil )
        )
    )

)

; TODO: Uncomment this for production
; ( startup )