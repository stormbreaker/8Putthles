;Load in Dr. Weiss's search code
(load 'search.lsp)
(load 'search_functions.lsp)
(load 'read.lsp)
(load 'heuristics.lsp)
(load 'output.lsp)
(load 'solvable.lsp)
(load 'general.lsp)

(defvar *nodesGenerated*);includes generated duplicates that did not get added into the open list
(defvar *nodesExpanded*)
(defvar *finout*)


#|
Name: 8puzzle
Author: Johnathan Ackerman (but really everyone because i'm using their functions)
Description:  This function displays a path and data about 5 different search routines run on
the toy 8 puzzle.
Parameters: optional filename - pass a puzzle by file
Return: nothing specific, don't use the return value for this function
|#
(defun 8puzzle  
    (
     &optional puzzleFile 
    )
    (let ((puzzleList) sublistLength)

	 (if (listp puzzleFile) (setf puzzleFile (car puzzleFile)))
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
         (cond
             ((solve_switch puzzleList subListLength))
             (t (format t "Puzzle not solvable~%") (return-from 8puzzle nil)))

         ;add 
         
         ;place into 2d style list (easier for the output guy)
         (setf puzzleList (getNested sublistLength puzzleList))
         
         ;any further checks on the list should go here
         
         
         
         ;A* admissible #1
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "A* Misplaced Tiles search:~%")
         (setf outPut(aStar puzzleList #'simpleHeuristic ));return OutputList
         (printSolutionBlock outPut)
         
         ;A* admissible #2
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "~%~%A* Manhattan Distance search:~%")
         (setf outPut(aStar puzzleList #'calcManhattan ));return OutputList
         (printSolutionBlock outPut)
         
         ;A* inadmissible
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "~%~%A* Nilsson's Sequence Score search:~%")
         (setf outPut (aStar puzzleList #'nilsson ));return OutputList
         (printSolutionBlock outPut)
         
         ;DepthFirstIteratedDepeningSearch
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "~%~%DFS itterated deepening search: (May take a while)~%")
         (setf outPut (dfsID puzzleList));return OutputList
         (printSolutionBlock outPut)
         
         ;BreathFirstSearch
         (format t "~%~%BFS search: (May take a while)~%")
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut (bfs puzzleList));return OutputList
         (printSolutionBlock outPut)
    )
)

#|
Name: printSolutionBlock
Author: Johnathan Ackerman (but really everyone because i'm using their functions)
Description:  This function displays a path and data about 5 different search routines run on
the toy 8 puzzle.
Parameters: optional filename - pass a puzzle by file
Return: nothing specific, don't use the return value for this function
|#
(defun printSolutionBlock (outPut)
    (if (/= (list-length output) 0)
        (format t "     Number of moves required: ~s~%" (list-length output)) ;if
        (format t "     End not found.  ~%"))    ;else
    (format t "     Number of Nodes Generated: ~s~%" *nodesGenerated*)
    (format t "     Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
    (prt_sol outPut)
)

(8puzzle *ARGS*)
