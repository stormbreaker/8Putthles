(defun genGoal (size)
	(let ((rowcount 0) (colCount 0) (counter 1) goalrow goalstate)
		(cond
			(
				(= size 3) (setf goalstate '((7 6 5) (8 0 4) (1 2 3)))
			)
			(
				t
				(dotimes (n  (1- (* size size)))
	   				(push counter goalrow)
					(incf counter)
					(incf colCount)
					(when (= n (- (* size size) 2)) (push 0 goalrow) (incf colCount))
					(when (= colCount size) (push (reverse goalrow) goalstate) (setf colCount 0) (setf goalrow nil))
				)
			)
		)
		(reverse goalstate)
	)
)
