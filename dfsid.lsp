(defun iddfs_runner
	(let (depth 0)
		#|
		while !goal
		{
			gets solution: (it_deep_dfs puzzle depth)
			(incf depth)
		}
		|#
	)
)

(defun it_deep_dfs (puzzle depth)
	(let (open child sol)
		(setf sol puzzle)
		;(when puzzle (!goal && depth == 0) nil)
		(when puzzle is a goal puzzle)
		(setf open (get_successors puzzle)
		(dolist (node open)
			(setf child (it_deep_dfs node (- depth 1)))
			(cond 
				(equals nil child) (;something)
				(t setf sol (append sol child)
				;break from loop
		)
		sol
	)
)
