(load 'read.lsp)
#|
Name: genGoal
Author: Benjamin Kaiser
Description:  This function generates a goal state after it has been given a length of a single row. The 8-puzzle is special cased as per Dr. Weiss' problem statement.  Anything 		larger than that has the blank tile in the upper left corner for a valid goal state.   
Parameters: 
	size: this is the length of a row so the entire puzzle consists of size^2 tiles 	with one blank.  
Return:  This returns a valid goal state for both the 8-puzzle as indicated by Dr. Weiss
	and also for any puzzle size larger than that.  15+ puzzles have the blank tile in 		the upper left corner as a valid goal state. The state is stored as a list of 		sublists with each sublist beign a row.  
|#
(defun genGoal (size)
	(let ((rowcount 0) (colCount 0) (counter 0) goalrow)
		(cond
			(
				(= size 3) (return-from genGoal (setf goalrow (getNested size '(1 2 3 8 0 4 7 6 5))))
			)
			(
				t
				(dotimes (n  (* size size))
	   				(push counter goalrow)
					(incf counter)
					;(break)
				)
			)
		)
                (getNested size (reverse goalrow))
	)
)
