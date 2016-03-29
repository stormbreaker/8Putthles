;Heuristics and helper function
#|*******************************************|#

;admissable
;heuristic that checks if each position has the right value
#|
Name: simpleHeuristic
Author: Johnathan Ackerman
Description:  Misplaced Tile Heuristic - checks if a given
position is currect
Parameters: state - current 8 puzzle state
Return: count - Heuristic generated value
|#
(defun simpleHeuristic (state)
    (let (
            (count 0)
            (elementCounter 0) (sublistCounter 0)
            goalState
        )

        ;set goalState
        (setf goalState (generateGoalState (list-length state)))

        (dolist (sublist state) ;2d list
            (setf elementCounter 0) ;reset x value
            (dolist (element sublist)
                (if (/= element ( nth elementCOunter ( nth sublistCounter goalState))) (incf count))
                (incf elementCounter) ; increment x-value
            )
            (incf sublistCounter) ;increment y-value
        )
        count
    )
)

;inadmissable
;checks if the spots around each spot is correct
#|
Name: nilsson
Author: Johnathan Ackerman
Description:  Nilsson's Sequence update - checks
the positions around each spot in the puzzle, for 
each that is wrong add 2. If the 0 place is wrong.
This version of the nilsson heuristic checks all
4 neighbors if they exists. This makes the algorithm
gravitate towards having the center correct.
add 1.
Parameters: state - current 8 puzzle state
Return: count - Heuristic generated value
|#
(defun nilsson (state)
    (let (
            (count 0)
            (elementCounter 0) (sublistCounter 0)
            (curPosition '(0 0))
            goalState
        )

        ;set goalState
        (setf goalState (generateGoalState (list-length state)))
        
        ;find 0 in goalState
        (block search-for-0 ;pointer to break from loop
            (dolist (sublist goalState) ;2d list
                (setf elementCounter 0) ;reset x value
                (dolist (element sublist)
                    (if (= element 0) (return-from search-for-0)
                    (incf elementCounter)) ; increment x-value
                )
                (incf sublistCounter) ;increment y-value
            )
        )

        (setf sublistCounter 0)
        (dolist (sublist state) ;2d list
            (setf elementCounter 0) ;reset x value
            (dolist (element sublist)
                ;check top
                (if (and (/= sublistCounter 0) 
                    (/= ( nth elementCounter ( nth (- sublistCounter 1) state)) 
                        ( nth elementCounter ( nth (- sublistCounter 1) goalState))))
                    (incf count 2); increment count
                )
                
                ;check bottom
                (if (and (/= sublistCounter (- (list-length state) 1 ))
                    (/= ( nth elementCounter ( nth (+ sublistCounter 1) state)) 
                        ( nth elementCounter ( nth (+ sublistCounter 1) goalState))))
                    (incf count 2); increment count
                )
                
                ;check left
                (if (and (/= elementCounter 0)
                    (/= ( nth (- elementCounter 1) ( nth sublistCounter state)) 
                        ( nth (- elementCounter 1) ( nth sublistCounter goalState))))
                    (incf count 2)
                )
                
                ;check right
                (if (and (/= elementCounter (- ( list-length state) 1 ))
                    (/= ( nth (+ elementCounter 1) ( nth sublistCounter state)) 
                        ( nth (+ elementCounter 1) ( nth sublistCounter goalState))))
                    (incf count 2)
                )
                
                (incf elementCounter) ;increment x-value
            )
            (incf sublistCounter) ;increment y-value
        )
        
        ;check 0
        (if (= ( nth (car curPosition) ( nth (cadr curPosition) state)) ( nth (car curPosition) ( nth (cadr curPosition) goalState)))
            (incf count 1)) ; increment x-value

        count
    )
)

#|
Name: calcManhattan
Author: Benjamin Kaiser
Description: This function 
Parameters:
Return: 
|#
#|
Sam.  Broke as said in FB message

|#
(defun calcManhattan (puzzle)
	(let ((sum 0) (colI 0) (rowI 0) (loc))
		;loop through the rows (sublists)
		(dolist (row puzzle)
			;loop the element of the sublists (columns)
			(dolist (tile row)
				;initialize the location to empty everytime
				(setf loc nil)
				;find out which column we're in and put in loc
				(push colI loc)
				;find out which row we're in and put in loc
				(push rowI loc)
				;call the helper function on this location
				(setf sum (+ sum (tileDistance loc puzzle))) ;This line may be a little off
				;move to the next column
				(incf colI)
			)
			(setf colI 0)
			(incf rowI)
		)
		;return this value
		sum
	)
)

;*********HELPER FUNCTIONS**************

#|
Name: tileDistance
Author: Benjamin Kaiser
Description: This function 
Parameters: loc is a list of length 2 which contians the x and y
Return: 
|#

;;theoretically cars give rows and cadrs give column

;;;What I don't like about this is that it is hard-coded to an 8 puzzle 

(defun tileDistance (loc puzzle) ;should this get passed in a goal state as well?
	(let (valueToTest)
		(setf valueToTest (nth (cadr loc) (nth (car loc) puzzle)))
		(cond
			((= valueToTest 1) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 0))))
			((= valueToTest 2) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 0))))
			((= valueToTest 3) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 0))))
			((= valueToTest 4) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 1))))
			((= valueToTest 5) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 2))))
			((= valueToTest 6) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 2))))
			((= valueToTest 7) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 2))))
			((= valueToTest 8) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 1))))
			(t 0)
		)
	)
)



;generates the goal state
#|
Name: generateGoalState
Author: Johnathan Ackerman
Description:  generates an answer key
Parameters: length - length of sublist
Return: goal-state
|#
(defun generateGoalState ( length )

    ;use length to generalize
    
    ;currently just 8Puzzle though
    '((1 2 3)(8 0 4)(7 6 5)) ;return solution

)

