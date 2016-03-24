;(defun readlist(fstream ))

#|
Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
Modifier: Benjamin Kaiser
Modified March 19, 2016 for use in Program 2 - 8 Puzzles Artificial Intelligence
|#
(defun fileio (filename size)
    "(fileio filename): open a puzzle input file and read the data"
	(let (datahold)
      ; check for correct usage
		(when (null filename) (return-from fileio "Usage: fileio.lsp filename"))

      ; read through file using open
		(format t "~%Opening file ~a using open~%" filename)
		(setf fin (open filename :if-does-not-exist nil)); open file, returning NIL on error
	      
		(when (null fin) 
			(return-from fileio (format nil "Error: cannot open file ~a" filename))
		)
	    (do ((data (read fin nil) (read fin nil))); read entire file, returning NIL at EOF
			((null data) (close fin) (reverse datahold)) ;return a reversed list when EOF
			(push data datahold)
		)
	)
)

(defun userinput(size)
	(let (temp temppuzzle (count 0))
		(do ((data (read) (read)))
			;theoretically does nested sublists - theoretically
			((eq data -1) (return-from userinput (reverse temppuzzle)))
			(if (= count size) (push (reverse temp) temppuzzle))
			(push data temp)
			(1+ count)		
		)
	)
)

;get the size of the puzzle
(defun entersize()
	(let ((size 3))
		(read)
	)
)

;For testing purposes to test this function independently
(setf puzsize (entersize))

(setf puzzle (fileio (car *args*) puzsize))

(setf userpuz (userinput puzsize))


