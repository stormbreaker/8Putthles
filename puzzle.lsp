;Load in Dr. Weiss's search code
(load 'search.lsp)

(defun 8puzzle  
    (
     puzzleFile  
     &optional puzzleSize
    )
    (let ((puzzleList ((1 3 4)(8 6 2)(7 0 5)))) ;set puzzel list to easy puzzel as default
         ;check if size is Valid
         ;read in puzzleFile into puzzle list
         ;check if solvable <- he does give us a solvable function
         
         ;BreathFirstSearch
         (search    puzzleList  BFS Null );return OutputList
         (PrintScreen OutputList)
         
         ;DepthFirstIteratedDepeningSearch
         (search    puzzleList  DFID    Null );return OutputList
         (printScreen OutputList)
         
         ;A* admissible #1
         (search    puzzleList  bestFirst   admissibleHeuristic1 );return OutputList
         (printScreen OutputList)
         
         ;A* admissible #2
         (search    puzzleList  bestFirst   admissibleHeuristic2 );return OutputList
         (printScreen OutputList)
         
         ;A* inadmissible
         (search    puzzleList  bestFirst   inAdmissibleHeuristic1 );return OutputList
         (printScreen OutputList)
    )
)

(defun search   
    (
     puzzleList  
     algorithm   
     Heuristic
    )
    (let    (answerList)
            ;(if Heuristic /= null)
                (algorithm  puzzleList  Heuristic);return answerList
            ;else
                (algorithm  puzzleList);return answerList
            
            (prepForDisplay answerList);return this ; this could all be done in printScreen
            
    )
)

(defun  BFS 
    (
     puzzleList
    )
    (let    (answerList)
            ;bfs search
            ;return answerList
    )
)

(defun  DFID puzzleList
    (let    (answerList)
            ;dfid search
            ;return answerList
    )
)

(defun bestFirst    puzzleList  heuristic
    (let    (answerList)

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

(defun printScreen  outputList
    (let    ()
            ;actually print to screen
    )
)


;Will also need three different heuristic functions


;Functions needed for weiss's search algorithms
(defun generate-successors (state)
    
    
)
    

(defun goal-state (state)
    (if (null (equal (con state) '(1 2 3))) (return-from goal-state nil))
    (if (null (equal (cdr state) '(4 0 5))) (return-from goal-state nil))
    (if (null (equal (cdr state) '(6 7 8))) (return-from goal-state nil))
    t
)
    
    
    
)