;Load in Dr. Weiss's search code
(load 'search.lsp)
(declaim (ftype (function () t) customSearch))
(declaim (ftype (function () t) swapPoints))

(defun 8puzzle  
    (
     puzzleFile  
     &optional puzzleSize
    )
    (let ((defvar 'puzzleList '((1 3 4)(8 6 2)(7 0 5)))) ;set puzzel list to easy puzzel as default
         ;check if size is Valid
         ;read in puzzleFile into puzzle list
         ;check if solvable <- he does give us a solvable function
         
         ;BreathFirstSearch
         (customSearch    puzzleList  BFS Null );return OutputList
         (PrintScreen OutputList)
         
         ;DepthFirstIteratedDepeningSearch
         (customSearch    puzzleList  DFID    Null );return OutputList
         (printScreen OutputList)
         
         ;A* admissible #1
         (customSearch    puzzleList  bestFirst   admissibleHeuristic1 );return OutputList
         (printScreen OutputList)
         
         ;A* admissible #2
         (customSearch    puzzleList  bestFirst   admissibleHeuristic2 );return OutputList
         (printScreen OutputList)
         
         ;A* inadmissible
         (customSearch    puzzleList  bestFirst   inAdmissibleHeuristic1 );return OutputList
         (printScreen OutputList)
    )
)

(defun customSearch   
    (
     'puzzleList  
     algorithm   
     Heuristic
    )
    (let    ((defvar 'answerList))
            ;(if Heuristic /= null)
                (algorithm  'puzzleList  Heuristic);return answerList
            ;else
                (algorithm  'puzzleList);return answerList
            
            (prepForDisplay 'answerList);return this ; this could all be done in printScreen
            
    )
)

(defun bestFirst    'puzzleList  heuristic
    (let    ((defvar 'answerList))

            ; sudo code for a* from Weiss's website
#|
            BestFS( node ) // A* algorithm
            {
            Add( node, open );
            repeat
            node = Best( open );
            move node from open list to closed list;
            if Goal( node ) then return SUCCESS;
            for each child in Successors( node ) do
            if child is not on open or closed lists then
            Add( child, open );
            else if child is on open list then
            update F’( node ) and Parent( node );
            else if child is on closed list then
            update F’( node ) and Parent( node ) and either
            a) move node from closed to open;
            - OR -
            b) update descendants of node on open and closed;
            until Empty( open );
            return FAILURE;
            }
|#
    )
)

(defun printScreen  'outputList
    (let    ()
            ;actually print to screen
    )
)


;Will also need three different heuristic functions


;Functions needed for weiss's search algorithms
#|----------------------------------------------------|#

;generates successors from the current state - should work for all versions
(defun generate-successors ('state 'node)
    (let    (
             (defvar successor-list) ;will become a list of node sturcture
             (defvar sublistCounter '0) (defvar elementCounter) 
             (defvar position '(0 0)) (defvar successorNode) 
             (defvar size)(defvar right 1)
             (defvar left -1)(defvar up 1)
             (defvar down -1)
             (defvar newState)
            )
            ;needs a list <- run until all successors generated
            ;assume no successors have been made
            ;state is the parrent state
            ;find 0 in state
            (block search-for-0
                (dolist (sublist 'state)
                    (setf elementCounter 0)
                    (dolist (element 'sublist)
                        (if (= 'element 0) (return-from search-for-0))
                        (1+ elementCounter)
                    )
                    (1+ sublistCounter)
                )
            )
            ;sets position of 0
            (setf (car 'position) elementCounter)
            (setf (cadr 'position) sublistCounter)
            (setf size (list-length 'state));gets the length of state-> 2d list, must be same width and height
            
            ;if the car of position is 0, only horizantal movement is to the right
            (cond 
                ((= (car 'position) 0) (setf newState (swapPoints 'state 'position 'right '0))
                    ;make a successorNode with new state
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                    
                    ;put in list 
                    (append 'successor-list 'successorNode)
                )
            
                ;if the car of position is size -1, only horizantal movement is to the left
                ((= (car 'position) (- size 1)) (setf newState (swapPoints 'state 'position 'left 0))
                    ;make a successorNode with new state
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                    ;put in list
                    (append 'successor-list 'successorNode)
                                                
                )
            
                ;else can move both
                (t (swapPoints 'state 'position 'left 0)
                    ;put in list
                    
                    (swapPoints 'state 'position 'right 0)
                    ;put in list
                    (append 'successor-list 'successorNode)
                )
            )
            
            ;if the cdr of position is 0, only vertical movement is down
            (cond
                ((= (cadr 'position) 0) (setf newState (swapPoints 'state 'position 0 'down))
                    ;make a successorNode with new state
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                    ;put in list
                    (append 'successor-list 'successorNode)
                )
                ;if the cdr of position is size -1, only vertical movement is up
                ((= (cadr 'position) (- size 1)) (setf newState (swapPoints 'state 'position 0 'up))
                    ;make a successorNode with new state
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                    ;put in list
                    (append 'successor-list 'successorNode)
                )
                ;else can move both
                (t (swapPoints 'state 'position 0 'down)
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                ;put in list
            
                    (swapPoints 'state 'position 0 'up)
                    ;make a successorNode with new state
                    (setf successorNode (make-node :state newState :parent node));sets up one successor
                    ;put in list
                    (append 'successor-list 'successorNode)
                )
            )
            'succesor-list
    )
)
    
;returns true if goal state <- version only works on basic puzzle
(defun goal-state (state)
    (if (null (equal (car state) '(1 2 3))) (return-from goal-state nil))
    (if (null (equal (cadr state) '(4 0 5))) (return-from goal-state nil))
    (if (null (equal (caddr state) '(6 7 8))) (return-from goal-state nil))
    t
)

(defun swapPoints ('state 'position 'right-left 'up-down)
    (let ()
         
    )
)