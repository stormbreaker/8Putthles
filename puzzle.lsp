;Load in Dr. Weiss's search code
(load 'search.lsp)
(load 'searchFunctions.lsp)
(load 'read.lsp)
(load 'heuristics.lsp)
(load 'output.lsp)

(defvar *nodesGenerated*);includes generated duplicates that did not get added into the open list
(defvar *nodesExpanded*)


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
         (format t "BFS search: (may take a while)~%")
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut (bfs puzzleList));return OutputList
         (prt_sol outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         
         
         ;DepthFirstIteratedDepeningSearch
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (format t "~%~%DFS itterated deepening search:~%")
         ;(setf outPut (dfs puzzleList));return OutputList
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         
         
         ;A* admissible #1
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut(aStar puzzleList #'simpleHeuristic ));return OutputList
         (format t "~%~%A* Misplaced Tiles search:~%")
         (prt_sol outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         ;A* admissible #2
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut(aStar puzzleList #'calcManhattan ));return OutputList
         (format t "~%~%A* Manhattan Distance search:~%")
         (prt_sol outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
         
         ;A* inadmissible
         (setf *nodesGenerated* 0);reset globals
         (setf *nodesExpanded* 0)
         (setf outPut (aStar puzzleList #'nilsson ));return OutputList
         (format t "~%~%A* Nilsson's Sequence Score search:~%")
         (prt_sol outPut)
         (format t "Number of moves required: ~s~%" (list-length output))
         (format t "Number of Nodes Generated: ~s~%" *nodesGenerated*)
         (format t "Number of Nodes Expaneded: ~s~%" *nodesExpanded*)
    )
)
