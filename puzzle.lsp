;Load in Dr. Weiss's search code
(load 'search.lsp)
(load 'read.lsp)
(load 'heuristics.lsp)

(defvar *nodesGenerated*);includes generated duplicates that did not get added into the open list
(defvar *nodesExpanded*)

(defun 8puzzle  
    (
     &optional puzzleFile 
    )
    (let ((puzzleList '((1 3 4)(8 6 2)(7 0 5))) sublistLength) ;set puzzel list to easy puzzle as default
         ;read in puzzleFile into puzzle list
         (if (not (null puzzleFile))
             (setf  puzzleList (fileio puzzlefile)); if
             (setf puzzleList ( userinput ))    ;else
         )
         
         ;check if size is Valid
         (setf subListLength (sqrt (list-length puzzleList)));also sets the sublist length
         (if (not (integerp subListLength))
             (return-from 8puzzle nil))
         
         ;check if solvable <- he does give us a solvable function
         
         
         ;place into 2d style list (easier for the output guy)
         (setf puzzleList (getNested sublistLength puzzleList))
         
         ;any further checks on the list should go here
         
         
         
         ;BreathFirstSearch
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut (bfs puzzleList));return OutputList
         (format t "BFS search:~%")
         (format t "~s~%" outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         
         
         ;DepthFirstIteratedDepeningSearch
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "DFS itterated deepening search:~%")
         ;(setf outPut (dfs puzzleList));return OutputList
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         
         
         ;A* admissible #1
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut(aStar puzzleList #'simpleHeuristic ));return OutputList
         (format t "A* Misplaced Tiles search:~%")
         (format t "~s~%" outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         ;A* admissible #2
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut(aStar puzzleList #'calcManhattan ));return OutputList
         (format t "A* Manhattan Distance search:~%")
         (format t "~s~%" outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         ;A* inadmissible
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut (aStar puzzleList #'nilsson ));return OutputList
         (format t "A* Nilsson's Sequence Score search:~%")
         (printScreen outPut)
         (format t "~s~%" outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
    )
)

;Functions needed for weiss's search algorithms
#|----------------------------------------------------|#

;generates successors from the current state - should work for all versions
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

;Heuristics and helper function
#|*******************************************|#

;admissable
;heuristic that checks if each position has the right value
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

;generates the goal state
(defun generateGoalState ( length )

    ;use length to generalize
    
    ;currently just 8Puzzle though
    '((1 2 3)(8 0 4)(7 6 5)) ;return solution

)

;inadmissable
;checks if the spots around each spot is correct
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