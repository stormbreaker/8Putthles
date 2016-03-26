
#|
Name: tileDistance
Author: Benjamin Kaiser
Description: This function 
Parameters:
Return: 
|#
(defun tileDistance (loc puzzle) ;should this get passed in a goal state as well?
	(let (valueToTest)
		(setf valueToTest (nth (cadr loc) (nth (car loc) puzzle)))
		(cond
			((= valueToTest 1) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 0)))
			((= valueToTest 2) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 0)))
			((= valueToTest 3) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 0)))
			((= valueToTest 4) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 1)))
			((= valueToTest 5) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 2)))
			((= valueToTest 6) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 2)))
			((= valueToTest 7) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 2)))
			((= valueToTest 8) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 1)))
			(t (+ (abs (- 0 0)) (abs (- 0 0))))
		)
	)
)

(defun calcManhattan (puzzle)
	(let ((sum 0) (colI 0) (rowI 0) (loc))
		(dolist (row puzzle)
			(dolist (tile row)
				(setf loc nil)
				(push colI loc)
				(push rowI loc)
				(setf sum (+ sum (tileDistance loc puzzle))
				(incf colI)
			)
			(incf rowI)
		)
	)
)
