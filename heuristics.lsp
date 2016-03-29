#|
Name: tileDistance
Author: Benjamin Kaiser
Description: This function 
Parameters: loc is a list of length 2 which contians the x and y
Return: 
|#

;;theoretically cars give rows and cadrs give column

;;;What I don't like about this is that it is hard-coded to an 8 puzzle 

(defun tileDistance (loc puzzle) ;should this get passed in a goal state as well?
	(let (valueToTest)
		(setf valueToTest (nth (cadr loc) (nth (car loc) puzzle)))
(format t "testing value ~S at [~S , ~S]~%" valueToTest (car loc) (cadr loc))
		(cond
			((= valueToTest 1) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 0))))
			((= valueToTest 2) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 0))))
			((= valueToTest 3) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 0))))
			((= valueToTest 4) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 1))))
			((= valueToTest 5) (+ (abs (- (cadr loc) 2)) (abs (- (car loc) 2))))
			((= valueToTest 6) (+ (abs (- (cadr loc) 1)) (abs (- (car loc) 2))))
			((= valueToTest 7) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 2))))
			((= valueToTest 8) (+ (abs (- (cadr loc) 0)) (abs (- (car loc) 1))))
			(t 0)
		)
	)
)

#|
Name: calcManhattan
Author: Benjamin Kaiser
Description: This function 
Parameters:
Return: 
|#
#|
Sam.  Broke as said in FB message

|#
(defun calcManhattan (puzzle)
	(let ((sum 0) (colI 0) (rowI 0) (loc))
		;loop through the rows (sublists)
		(dolist (row puzzle)
			;loop the element of the sublists (columns)
			(dolist (tile row)
				;initialize the location to empty everytime
				(setf loc nil)
				;find out which column we're in and put in loc
				(push colI loc)
				;find out which row we're in and put in loc
				(push rowI loc)
				;call the helper function on this location
				(setf sum (+ sum (tileDistance loc puzzle))) ;This line may be a little off
				(format t "For location ~S we have ~S~%" loc (tileDistance loc puzzle))
				;move to the next column
				(incf colI)
			)
			(setf colI 0)
			(incf rowI)
		)
		;return this value
		sum
	)
)
