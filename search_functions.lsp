;------------------------------------------------------------------------------
;needed functions and added functions
;------------------------------------------------------------------------------
;returns true if goal state <- version only works on basic puzzle
(defun goal-state (state)
    (let ((goal (genGoal (list-length state))))
         (dolist (sublist state)
             (if (not (equal sublist (nth (position sublist state) goal))) (return-from goal-state nil))
         )
    t
    )
)

;Functions needed for weiss's search algorithms
#|----------------------------------------------------|#

;generates successors from the current state - should work for all versions
#|
Name: generate-successors
Author: Johnathan Ackerman 
Description:  This function generates all possible successors for the
given 8 puzzle state.
Parameters: state - state of the 8 puzzle
Return: successor-list - list of all successors
|#
(defun generate-successors (state)
    (let    (
             ( sublistCounter 0 )
             successor-list ;will become a list of node sturcture
             elementCounter
             ( curPosition '(0 0)) successorNode
             size (right 1)
             (left -1) ( up 1)
             (down -1)
             (newState)
            )
                        
            ;needs a list <- run until all successors generated
            ;assume no successors have been made
            ;state is the parrent state
            ;find 0 in state
            (block search-for-0 ;pointer to break from loop
                (dolist (sublist state) ;2d list
                    (setf elementCounter 0) ;reset x value
                    (dolist (element sublist)
                        (if (= element 0) (return-from search-for-0)
                        (incf elementCounter)) ; increment x-value
                    )
                    (incf sublistCounter) ;increment y-value
                )
            )
            ;sets curPosition of 0
            (setf (car curPosition) elementCounter)
            (setf (cadr curPosition) sublistCounter)
            (setf size (list-length state));gets the length of state-> 2d list, must be same width and height
            
            ;if the car of curPosition is 0, only horizantal movement is to the right
            (cond 
                ((= (car curPosition) 0) (setf newState (swapPoints state curPosition right 0))                
                    ;put in list 
                    (setf successor-list (append successor-list (list newState)))
                )
            
                ;if the car of curPosition is size -1, only horizantal movement is to the left
                ((= (car curPosition) (- size 1)) (setf newState (swapPoints state curPosition left 0))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                                                
                )
            
                ;else can move both
                (t 
                    (setf newState (swapPoints state curPosition left 0))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                    
                    (setf newState (swapPoints state curPosition right 0))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                )
            )
            
            ;if the cadr of curPosition is 0, only vertical movement is down
            (cond
                ((= (cadr curPosition) 0) (setf newState (swapPoints state curPosition 0 down))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                )
                ;if the cadr of curPosition is size -1, only vertical movement is up
                ((= (cadr curPosition) (- size 1)) (setf newState (swapPoints state curPosition 0 up))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                )
                ;else can move both
                (t 
                    (setf newState (swapPoints state curPosition 0 down))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
            
                    (setf newState (swapPoints state curPosition 0 up))
                    ;put in list
                    (setf successor-list (append successor-list (list newState)))
                )
            )
            successor-list
    )
)
    
;swaps two points in a 2d list based on boolean values and a position
#|
Name: swapPoints
Author: Johnathan Ackerman
Description:  swaps two points in a 2d list based on boolean values and a position
Parameters: state - current 8 puzzle state
            curPosition - position in state being swapped
            right-left - -1 move curPosition left +1 move right 0. don't move
            up-down - -1 move curPosition down +1 move up 0. don't move
Return: new state based on swap
|#
(defun swapPoints (state curPosition right-left up-down)
    (let ((tempState ()))
         (dolist (subList state)
             (setf tempState (append tempState (list (copy-list subList))))
         )
         (cond
             ((= right-left 1) ;swap right
                (rotatef ( nth (car curPosition) ( nth (cadr curPosition) tempState)) ( nth (+ 1 (car curPosition) ) ( nth (cadr curPosition) tempState)) )
             )
             ((= right-left -1) ;swap left
                (rotatef ( nth (car curPosition) ( nth (cadr curPosition) tempState)) ( nth (- (car curPosition) 1 ) ( nth (cadr curPosition) tempState)) )
              )
             ((= up-down 1) ;swap up
                (rotatef ( nth (car curPosition) ( nth (cadr curPosition) tempState)) ( nth (car curPosition) (nth (- (cadr curPosition) 1) tempState)) )
              )
             ((= up-down -1) ;swap down
                (rotatef ( nth (car curPosition) ( nth (cadr curPosition) tempState)) ( nth (car curPosition) (nth (+ (cadr curPosition) 1) tempState)) )
             )
        )
        tempstate
    )
)
