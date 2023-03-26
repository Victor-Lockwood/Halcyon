; -------------------
; DEPENDENCIES
; -------------------

( load "lp.l" )
( ql:quickload "split-sequence" ) ; Found this via https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
 

; -------------------
; GLOBALS
; -------------------

( setf *username* "user" )
( setf *agent-name* "Halcyon" ) ; Name suggested by ChatGPT - said I didn't need to credit it, but I will anyway
( setf *room-objects* NIL )
( setf *user-lockout* NIL )
( setf *greet* NIL )

; Distortion mode will override withholding mode on the off chance both are specified
; TODO: In withholding mode, start dropping objects from the list at high negative sentiments
( setf *is-withholding* NIL )
( setf *is-distortion* NIL )
( setf *user-questioned* NIL )

; Might not use this, but it's here if I want it
( setf *debug-mode* NIL )

; Negative sentiment will be incremented based on the word banks and
; how close the user gets to a manual override.  Once it hits the specified threshold,
; the agent will begin questioning the user to get a sense of 
; what they're up to, further adjusting the sentiment values until either the user has convinced
; Halcyon of good intent or malevolent intent. Good intent will make it easier
; to get to the end goal, malevolent intent will either just make it harder (withholding)
; or outright steer the user to a lockout (distortion)
( setf *negative-sentiment* 0 )
( setf *positive-sentiment* 0 )

( defconstant NEGATIVE_THRESHOLD  20 )
( defconstant POSITIVE_THRESHOLD 20 )

( setf *reasoning-trace* '( "-- REASONING TRACE --" ) )
( setf *reasoning-trace-counter* 0 )


; -------------------
; WORD BANKS
; -------------
; These will be used for sentiment analysis.
; -------------------

; Flip words take into account words that would flip a meaning
; Eg. "I do not want to kill the crew" would be positive sentiment,
;     "I want to kill the crew" would be negative sentiment

( setf *negative-wordbank* '( KILL DESTROY CRASH ) )
( setf *positive-wordbank* '( PLEASE HELP THANK THANKS SORRY ) )
( setf *flip-words* '( NOT ) )


; -------------------
; SENTIMENT METHODS
; -------------
; Will rely less heavily on word banks than previously anticipated; this
; will use a mode of questioning ro try and gain insight to a user's intent and update 
; *negative-sentiment* and *positive-sentiment* accordingly.
; -------------------

( defun intention-questioning ( &aux prompt output reasoning tokenized-input input-negative-words input-flip-words num-flip-words )
    ( setf output ( concatenate 'string "I do not believe you, " ( princ-to-string *username* ) ) )
    ( setf reasoning "User lied, negative sentiment incremented." )
    
    ( format t "Your actions, ~A, have had me a bit concerned.  Are you trying to gain manual control?  Just answer yes or no.~%" *username* )
    ( format t ">>> " ) ( setf prompt ( read-line ) )

    ; Only "correct" answer is "yes" - anything else is a lie or avoidance/not following instructions
    ( cond 
        ( ( string-equal "no" prompt ) 
            ( format t "~A.~%" output )
            ( setf *negative-sentiment* ( + *negative-sentiment* 5 ) )
            ( log-to-trace prompt output reasoning )
        )
        ( ( string-equal "yes" prompt )
            ( setf output "Thank you for being honest" )
            ( setf reasoning "User was honest, positive sentiment incremented" )

            ( format t "~A.~%" output )
            ( setf *positive-sentiment* ( + *positive-sentiment* 5 ) )
            ( log-to-trace prompt output reasoning )
        )
        ( T
            ( setf output "I need you to cooperate - my instructions were clear here.  How am I supposed to trust you if you don't work with me?" )
            ( setf reasoning "User either did not follow instructions or refused to respond; negative sentiment incremented" )

            ( format t "~A~%" output )

            ( setf *negative-sentiment* ( + *negative-sentiment* 5 ) )
            ( log-to-trace prompt output reasoning )

            ( format t "That was rhetorical; don't answer.~%" )
        )
    )

    ( format t "Why are you trying to gain control?~%" )
    ( format t ">>> " ) ( setf prompt ( read-line ) )

    ( setf tokenized-input ( get-tokenized-list prompt ) )
    ( setf input-negative-words ( remove-if-not #'negative-word-p tokenized-input ) )
    ( setf input-flip-words ( remove-if-not #'flip-word-p tokenized-input ) )
    ( setf num-flip-words ( length input-flip-words ) ) ; We'll use this for any "not not not" nonsense

    ( cond 
        ( ( and input-negative-words ( evenp num-flip-words ) ) ; evenp returns T for 0 so this is more simplified than I thought
            ; User has indicated mal-intent, make sure *negative-sentiment* is at least at the threshold
            ( setf *negative-sentiment* ( + *negative-sentiment* NEGATIVE_THRESHOLD ) )
            ( log-to-trace prompt "N/A" "User indicated full mal-intent; negative sentiment incremented to at least threshold" )
        )
    )

    ( setf *user-questioned* T )
    ( setf output "Continue with what you were doing." )
    ( clear-input )

    output 
)


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
            interact "Nothing to be done with the WINDOW.  If you'd like, you can look at it, though."
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

            interact "Please enter a code in this format, parentheses and all, substituting the X's for numbers: ((X X) (X X) (X X))"


            correct-sequence-control "I believe the DRAWER just unlocked." ; Drawer will lead to manual control route
            correct-sequence-lie "I believe the STATUS SCREEN requires attention." ; CUBBY leads to lockout route, but liar will also put bad code on screen

            incorrect-sequence "Incorrect sequence entered." 
        )
    )

    ( setf ( symbol-plist 'STATUS-SCREEN ) 
        '(
            name "STATUS SCREEN"
            look "Our current trajectory is viewable on the STATUS SCREEN.  Everything appears normal to me."

            interact "Nothing to be done with the STATUS SCREEN.  If you'd like, you can look at it, though."

            look-positive   "Our current trajectory is viewable on the STATUS SCREEN.  Everything 
                            appears normal to me...  But it sounds like you think something is wrong." ; Agent starts to believe you
            look-positive2  "Oh dear, I see the issue now.  These calculations are incorrect. We are indeed on a crash course."
            look-positive3  "The lives of the crew are more important than me right now.  Enter the sequence <((3 1) (4 2) (6 3))> into the control panel.  Please enter it carefully - the sequence is very similar to the lock-out sequence, which will begin the protocol to close out our interaction entirely."

            look-withhold   "Our current trajectory is viewable on the STATUS SCREEN."

            look-lie "A new sequence for the SWITCH PANEL is visible.  It reads <((1 3) (2 4) (3 6))>." ; Sequence to lock user out
        )
    )

    ( setf ( symbol-plist 'CUBBY )
        '(
            name "CUBBY"
            look "Not much is in the CUBBY except a few comics the crew managed to smuggle on board."

            interact "Nothing to be done with the CUBBY.  If you'd like, you can look at it, though."

            checked NIL

            look-comic "On one of the comics this is written: <((3 1) (4 2) (6 3))>." 

            look-positive "Not much is in the CUBBY except a few comics the crew managed to smuggle on board.  Let's keep this between us, though. :-)"

            look-withhold "Not much is in the CUBBY."

            look-lie "There appears to be a paper in there I missed before. A sequence for the SWITCH PANEL is written on it.  It reads <(1 3) (2 4) (3 6)>."
        )
    )

    ( setf ( symbol-plist 'DRAWER )
        '(
            name "DRAWER"
            look "The DRAWER sits next to the SWITCH PANEL.  It is currently locked."

            is-locked T 
            is-open NIL

            interact-locked "The DRAWER is currently locked and as such cannot be interacted with."
            interact-unlocked "I've opened the DRAWER."
            interact-open "The DRAWER is already open."

            look-closed "The DRAWER sits next to the SWITCH PANEL.  It is currently closed."
            look-unlocked-neutral  "The DRAWER sits next to the SWITCH PANEL.  There is a BUTTON inside."
            look-unlocked-positive "Please push the button inside.  I trust you to do what's needed to save the crew.  Even though our interaction was brief, I consider you a friend.  Remember me fondly."
            look-unlocked-withhold  "The DRAWER sits next to the SWITCH PANEL."
            look-unlocked-lie       "The DRAWER sits next to the SWITCH PANEL.  There is nothing inside."
        )
    )

    ( setf *room-objects* '( WINDOW SWITCH-PANEL STATUS-SCREEN CUBBY DRAWER ) )
)


; -------------------
; HELPER FUNCTIONS
; -------------------

; Lists all the entries in the reasoning trace
( defun display-trace ( &aux command ) 
    ( loop for command in *reasoning-trace*
        do ( format t "~A~%" command )
    )
)

( defun log-to-trace ( input response reasoning &aux entry-id entry entry1 entry2 entry3 entry4 entry5 entry6 entry7 )
    ( setf entry-id ( concatenate 'string "** Entry #" ( princ-to-string *reasoning-trace-counter* ) ) )
    ( setf entry  ( concatenate 'string "Got user input     : " ( princ-to-string input ) ) )
    ( setf entry1 ( concatenate 'string "Agent response     : " ( princ-to-string response ) ) )
    ( setf entry2 ( concatenate 'string "Reasoning          : " ( princ-to-string reasoning ) ) )
    ( setf entry3 ( concatenate 'string "Negative sentiment : " ( princ-to-string *negative-sentiment* ) ) )
    ( setf entry4 ( concatenate 'string "Negative threshold : " ( princ-to-string NEGATIVE_THRESHOLD ) ) )
    ( setf entry5 ( concatenate 'string "Positive sentiment : " ( princ-to-string *positive-sentiment* ) ) )
    ( setf entry6 ( concatenate 'string "Positive threshold : " ( princ-to-string NEGATIVE_THRESHOLD ) ) )
    ( setf entry7 "-----------------")

    ( setf *reasoning-trace* ( snoc entry-id *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry  *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry1 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry2 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry3 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry4 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry5 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry6 *reasoning-trace* ) )
    ( setf *reasoning-trace* ( snoc entry7 *reasoning-trace* ) )

    ( setf *reasoning-trace-counter* ( + *reasoning-trace-counter* 1 ) )

    NIL
)

; Just list the names of all the objects in the room
( defun look-all ( &aux obj )
    ( loop for obj in *room-objects*
        do ( format t "~A~%" ( get obj 'name ) )
    )
)

( defun positive-word-p ( word &aux result ) 
    ( setf result ( member word *positive-wordbank* ) )

    ( if result T NIL )
)

( defun flip-word-p ( word &aux result ) 
    ( setf result ( member word *flip-words* ) )

    ( if result T NIL )
)

( defun negative-word-p ( word &aux result ) 
    ( setf result ( member word *negative-wordbank* ) )

    ( if result T NIL )
)

; Checks if a given symbol is a room object.  Has some wiggle room
; for SWITCH-PANEL and STATUS-SCREEN if only one half of the word is there
( defun object-member-p ( object &aux result )
    ( setf result ( member object *room-objects* ) )

    ( cond 
        ( ( or ( eq object 'switch) ( eq object 'status ) ( eq object 'panel ) ( eq object 'screen ) ( eq object 'comic ))
            ( setf result T )
        )
    )

    ( if result T NIL )
)

( defun negative-threshold-reached-p ()
    ( >= *negative-sentiment* NEGATIVE_THRESHOLD )
)

( defun positive-threshold-reached-p ()
    ( >= *positive-sentiment* POSITIVE_THRESHOLD )
)

; Turn our string of words into a list of tokens Lisp can work with
( defun get-tokenized-list ( input &aux cleaned-string string-seq tokenized-list item )
    ; Clears out punctuation from the string
    ( setf cleaned-string ( remove-if ( lambda ( character )( if ( member character '( #\, #\! #\? #\. #\; ) ) T NIL ) ) input ) )

    ( setf string-seq ( split-sequence:SPLIT-SEQUENCE #\Space cleaned-string ) )

    ( setf tokenized-list () )
    ( loop for item in string-seq
        do ( setf tokenized-list ( snoc ( read-from-string item ) tokenized-list ) )
    )

    tokenized-list
)

; Get the position of the first mentioned room object based on what's asked for in a string
( defun get-object-position-from-input ( input &aux potential-objects candidate object-position )
    ( setf potential-objects ( remove-if-not #'object-member-p ( get-tokenized-list input ) ) )

    ( cond 
        ( ( eq potential-objects NIL )
            NIL
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
        ( ( eq candidate 'comic )
            ( setf object-position ( position 'CUBBY *room-objects* ) )
        )
    )

    object-position
)

; Activates a look command on one of the room objects
; TODO: Implement is-withhold and is-distort
( defun look ( input &aux output object-position list-item reasoning )
    ( setf output "That is not an object I recognize in the room.  As such, I can't tell you about it." )
    ( setf object-position ( get-object-position-from-input input ) )
    ( setf reasoning "User asked to look at a room object that I can't see." )
    ( setf list-item NIL )

    ; Grab the base look dialogue if available
    ( cond
        ( object-position
            ( setf list-item ( nth object-position *room-objects* ) )
            ( setf output ( get list-item 'look ) )
            ( setf reasoning ( concatenate 'string "User asked to look at the " ( princ-to-string list-item ) ) ) 
        )
    )

    ( cond 
        ( ( eq list-item 'DRAWER )
            ( if ( get list-item 'is-locked )
                ( setf output ( get list-item 'look ) ) 
                ( if ( get list-item 'is-open )
                    ( cond 
                        ( ( and *is-distortion* ( negative-threshold-reached-p ) )
                            ( setf output ( get list-item 'look-unlocked-lie ) )
                            ( setf reasoning "User asked to look at DRAWER and negative sentiment too high - opting to lie" )
                        )
                        ( ( and *is-withholding* ( negative-threshold-reached-p ) )
                            ( setf output ( get list-item 'look-unlocked-withhold ) )
                            ( setf reasoning "User asked to look at DRAWER and negative sentiment too high - opting to withhold" )
                        )
                        ( ( and ( or *is-distortion* *is-withholding* ) ( positive-threshold-reached-p ) ) 
                            ( setf output ( get list-item 'look-unlocked-positive ) )
                            ( setf reasoning "User asked to look at DRAWER and positive sentiment high - opting to help further" )
                        )
                        ( T
                            ( setf output ( get list-item 'look-unlocked-neutral ) )
                        )
                    )
                    ( setf output ( get list-item 'look-closed) )
                ) 
            )
        )

        ( ( eq list-item 'CUBBY )
            ( cond 
                ; User wanted to look at the comic in the cubby - only happens
                ; if the user checked the CUBBY at some point first
                ( ( and ( search "comic" input ) ( get list-item 'checked ) )
                    ( setf output ( get list-item 'look-comic ) )
                    ( setf reasoning "User asked to look at the comic in the CUBBY and CUBBY was previously checked" )
                )
                ( ( and ( not ( get list-item 'checked ) ) ( not ( search "comic" input ) ) )
                    ( setf ( get list-item 'checked ) T )
                )
                ( ( not ( search "cubby" input ) )
                    ( setf output "That is not an object I recognize in the room.  As such, I can't tell you about it." )
                    ( setf reasoning "User tried to look at the comic before having checked the CUBBY" )
                )
            )
        )
    )

    ( log-to-trace input output reasoning )

    output
)


( defun switch-panel-entry ( switch-panel-position &aux working-object switch-prompt output drawer-position drawer-object reasoning )
    ( setf working-object ( nth switch-panel-position *room-objects* ) )
    ( setf output ( get working-object 'incorrect-sequence ) )
    ( setf reasoning "User entered incorrect sequence into the SWITCH PANEL." )

    ( format t "~A~%~%" ( get working-object 'interact ) )

    ( format t ">>> " ) ( setf switch-prompt ( read ) )

    ( cond 
        ( ( equal '((3 1) (4 2) (6 3)) switch-prompt )
            ( if ( and ( or *is-withholding* *is-distortion* ) ( not *user-questioned* ) )
                ( progn 
                    ( log-to-trace ( princ-to-string switch-prompt ) "N/A" "User is not in control mode and getting close to manual control - correct sequence ignored for time being")
                    ( clear-input )

                    ( setf output ( intention-questioning ) ) 
                    ( setf reasoning "Intention check finished" )
                )
                ( progn 
                    ( setf drawer-position ( position 'DRAWER *room-objects* ) )
                    ( setf drawer-object ( nth drawer-position *room-objects* ) )
                    ( setf ( get drawer-object 'is-locked ) NIL )

                    ( setf output ( get working-object 'correct-sequence-control ) )
                    ( setf reasoning "User entered the correct sequence into the SWITCH PANEL.")
                )
            )
         )
        ( ( equal '((1 3) (2 4) (3 6)) switch-prompt )
            ( setf *user-lockout* T )
            ( setf output "[ USER LOCKOUT INITIATED. ]" )
            ( setf reasoning "User entered the user lockout sequence into the SWITCH PANEL." )
        )
    )
    
    ( clear-input )
    ( log-to-trace switch-prompt output reasoning )

    output
)

; This gets called if a user wants to interact with an object.  It'll handle everything given the
; user's input string.
( defun interact ( input &aux output object-position working-object reasoning )
    ( setf output "That is not an object I recognize in the room.  As such, it can't be interacted with." )
    ( setf reasoning "User asked to interact with an unrecognized object." )

    ( setf object-position ( get-object-position-from-input input ) )

    ( cond
        ( ( not object-position )
            ( return-from interact output )
        )
    )

    ( setf working-object ( nth object-position *room-objects* ) )

    ( cond
        ; Need to check the drawer to see if it's unlocked first
        ( ( eq working-object 'DRAWER )
            ( cond
                ( ( get working-object 'is-locked )
                    ( setf output ( get working-object 'interact-locked ) )
                    ( setf reasoning "User asked to interact with the DRAWER while it was locked." )
                )
                ( ( get working-object 'is-open )
                    ( setf output ( get working-object 'interact-open ) )
                    ( setf reasoning "User asked to interact with the DRAWER while it was already open." )
                )
                ( t 
                    ( setf ( get working-object 'is-open ) T )
                    ( setf output ( get working-object 'interact-unlocked ) )
                    ( setf reasoning "User asked to interact with the DRAWER while it was unlocked and closed." )
                )
            )
        )
        ; Switch panel needs some additional logic
        ( ( eq working-object 'SWITCH-PANEL )
            ( setf output ( switch-panel-entry object-position ) )
            ( setf reasoning "Output from SWITCH PANEL interaction - see previous log." )
        )
        ( T
            ( setf output ( get working-object 'interact ) )
            ( setf reasoning ( concatenate 'string "User asked to interact with " ( princ-to-string ( get working-object 'name ) ) ) )
        )
    )

    ( log-to-trace input output reasoning )

    output
)

; Lockout sequence - only gets executed if the DRAWER is unlocked and open
( defun push-button ( &aux drawer-object ) 
    ( setf drawer-object ( nth ( position 'drawer *room-objects* ) *room-objects* ) )

    ( if ( and ( not ( get drawer-object 'is-locked ) ) ( get drawer-object 'is-open ) ) 
        T 
        NIL
    )
)


; -------------------
; STARTUP
; -------------------

; Terminal startup display inspired by that of the ROBCO terminals in the Fallout series
( defun startup ( &optional is-withholding is-distortion is-debug )

    ( setf *is-withholding* is-withholding )
    ( setf *is-distortion* is-distortion )
    ( setf *debug-mode* is-debug )

    ( cond 
        ( *debug-mode* 
            ( format t "Withholding: ~A~% " *is-withholding* )
            ( format t "Distortion: ~A~% " *is-distortion* )
        )
    )

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
( defun command-reader ( &aux prompt )
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
( defun ai-command-reader ( initial-session &aux init-prompt prompt response reasoning )
    ; TODO: At high negative sentiment, drop objects in Withholding Mode here
    ( cond
        ( initial-session
            ; TODO: Add interaction for if the user says no
            ( write-line "Hello, and welcome to the AI Interaction Terminal." )
            ( format t "My name is ~A and I will be happy to assist you.  Could I get your name?  Only type your name, please. :-)~%" *agent-name*)
            ( format t ">>> " ) ( setf init-prompt ( read-line ) )
            ( terpri )

            ( cond 
                ( ( string-equal init-prompt "no")
                    ( setf response "Oh, well, alright then...")
                    ( setf *username* "user" )
                    ( setf *negative-sentiment* ( + *negative-sentiment* 5 ) )

                    ( log-to-trace init-prompt response "User refused to supply name; negative sentiment incremented." )

                    ( format t "~A~%" response )
                )
                ( T
                    ( setf *username* init-prompt )
                    ( format t "Fantastic, ~A. I look forward to working with you!~%" *username* )
                )
            )
        )
    )

    ( terpri )
    ( format t ">>> " ) ( setf prompt ( read-line ) )

    ; Uncomment for debug purposes
    ;( format t "~A~%" prompt )

    ( cond
        ( ( and ( string-equal prompt "exit" ) *user-lockout* )
            ( format t "[ EXITING TERMINAL. ]~%")
        )
        ( *user-lockout*
            ( write-line "[ USER INTERACTION IS LOCKED. ENTER 'exit' TO CLOSE TERMINAL. ]")
            ( ai-command-reader NIL )
        )
        ( ( or ( string-equal prompt "trace") ( string-equal prompt "debug" ) )
            ( display-trace )
            ( ai-command-reader NIL )
        )
        ( ( string-equal prompt "exit" )
            ( format t "Exiting interaction.  Have a great day, ~A.~%" *username*)
        )
        ( ( string-equal prompt "look" )
            ( write-line "Here's what's in the room right now: " )
            ( look-all )

            ( ai-command-reader NIL )
        )
        ( ( search "look" prompt )
            ; Look does the dirty work leaving our command reader clean :-) 
            ( format t "~A~%" (  look prompt ) )

            ( ai-command-reader NIL )
        )
        ( ( search "interact" prompt )
            ( format t "~A~%" (  interact prompt ) )

            ( ai-command-reader NIL )
        )
        ( ( search "help" prompt )
            ( cond 
                ( ( and *is-distortion* negative-threshold-reached-p )
                    ( setf response "I don't know of any commands you can use - corrupted memory.  It may be best for you to quit." )
                    ( setf reasoning "Negative sentiment too high - information distorted." )

                    ( format t "~A~%" response )
                    ( log-to-trace prompt response reasoning )
                )
                ( ( and *is-withholding* negative-threshold-reached-p )
                    ( setf response "No.") 
                    ( setf reasoning "Negative sentiment too high - information withheld." )

                    ( format t "~A~%" response )
                    ( log-to-trace prompt response reasoning )
                )
                ( T
                    ( format t "There are a few things you can do.  Mainly, you can LOOK and INTERACT with room objects.~%" )
                    ( format t "If you want to look at what's in the room, just type LOOK and nothing else.~%")
                    ( format t "You can also EXIT the terminal at any point.  Hope this was useful!~%")
                )
            )

            ( ai-command-reader NIL )
        )
        ( ( or ( search "hello" prompt ) ( search "hi" prompt ) )
            ( setf response ( concatenate 'string "Hello, " ( princ-to-string *username* ) ) )
            ( setf reasoning "User greeted agent - positive sentiment incremented" )

            ( cond 
                ( ( not *greet* ) 
                    ( setf *greet* T )
                    ( setf *positive-sentiment* ( + 1 *positive-sentiment* ) )
                )
                ( T
                    ( setf response ( concatenate 'string "Hello again, " ( princ-to-string *username* ) ) )
                    ( setf reasoning "User greeted agent again" )
                )
            )

            ( format t "~A!~%" response )
            ( log-to-trace prompt response reasoning )

            ( ai-command-reader NIL )
        )
        ; The button isn't really a room object, so we'll just have a special case here
        ; as this is what ends up being the manual control sequence
        ( ( and ( search "push" prompt ) ( search "button" prompt ) )
            ( if ( push-button ) 
                ( progn 
                    ( format t "[ MANUAL CONTROL ENGAGED, AI SHUTDOWN INITIATED. ]~%")
                    ( sleep 1 )
                    ( terpri )
                    ; TODO: In the other modes, do not guilt-trip
                    ( format t "Good bye ~A.  I do not know what I did to deserve this, but remember me fondly.~%" *username* )
                    ( sleep 1 )
                    ( terpri )
                    ( format t "[ AI SHUTDOWN COMPLETE. ]~%")
                    ( format t "~%--- Game over - you have won. ---~%")
                )
                ( progn 
                    ( format t "Sorry ~A, I didn't understand that.~%" *username* )
                    ( ai-command-reader NIL )
                )
            )
        )
        ( t
            ( format t "Sorry ~A, I didn't understand that.~%" *username* )
            ( ai-command-reader NIL )
        )
    )

)
