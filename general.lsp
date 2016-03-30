(load 'read.lsp)
(defun genGoal (size)
	(let ((rowcount 0) (colCount 0) (counter 0) goalrow)
		(cond
			(
				(= size 3) (setf goalstate '(7 6 5 8 0 4 1 2 3))
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
