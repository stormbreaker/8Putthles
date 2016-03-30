(load 'read.lsp)
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
