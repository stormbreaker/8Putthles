;(defun readlist(fstream ))

#|
Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
Modifier: Benjamin Kaiser
Modified March 19, 2016 for use in Program 2 - 8 Puzzles Artificial Intelligence
|#
(defun fileio (filename)
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

(defun userinput()
	(let (temp temppuzzle)
		(do ((data (read) (read)))
			((equal data -1) (return-from userinput (reverse temppuzzle)))
			(push data temppuzzle)
		)
	)
)

(defun getNested(size puzzlelist)
	(let ((count 0) temppuzzle tempsub temppos)
		;loop to go through the puzzle list
		(dolist (temppos puzzlelist) (reverse temppuzzle)
			(push temppos tempsub)
			(incf count)
			(when (equal count size) (push (reverse tempsub) temppuzzle) (setf count 0) (setf tempsub nil))
			;(break)
		)
		(reverse temppuzzle)
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

(setf puzzle (fileio (car *args*)))

(setf userpuz (userinput))

(setf newpuzzle (getNested puzsize puzzle))

(setf newuserpuz (getNested puzsize userpuz))


