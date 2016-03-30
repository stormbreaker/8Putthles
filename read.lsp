#|
Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
Modifier: Benjamin Kaiser
Modified March 19, 2016 for use in Program 2 - 8 Puzzles Artificial Intelligence
Description:  This function takes an input string which is the name of a file.  The file is 	then opened for input.  A do loop goes through the contents of the file and pushes them 
	onto a list.  If no file parameter was supplied, the function checks for this and outputs an appropriate error message.  If the file fails to open, NIL is returned from the function.
Parameters:  
	filename: this should be a string atom that contains the name of a file and its extension
Return: This function returns the content of the file in a list.  
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
			(return-from fileio nil)
		)
	    (do ((data (read fin nil) (read fin nil))); read entire file, returning NIL at EOF
			((null data) (close fin) (reverse datahold)) ;return a reversed list when EOF
			(push data datahold)
		)
	)
)

#|
Name: userinput
Author: Benjamin Kaiser
Description:  This function is similar to the fileio function except that it takes user input as individual atoms.  These can all be on the same line or on multiple lines.  They need to 	be separated by whitespace either way.  The function returns a list of all the values that were input.  A value of -1 terminates the function and the function ends.  
Parameters: N/A
Return: a list of the values which were input by the user.  -1 terminates the list and is not included in the list
|#
(defun userinput()
        (format t "Enter a series of numbers between 0 and 8, with spaces")
        (format t " between any two numbers when complete enter -1~%")
        (format t "E.G.~%Puzzle: 1 2 3 8 0 4 7 6 5~%-1~%Puzzle: ")
	(let (temp temppuzzle)
		(do ((data (read) (read)))
			((equal data -1) (return-from userinput (reverse temppuzzle)))
			(push data temppuzzle)
		)
	)
)

#|
Name: getNested
Author: Benjamin Kaiser
Description:  This function takes a list and creates nested sublists of the elements in the original list.  The length of these sublists is determined by the size parameter.  It 			iterates through the list and places them in a temporary list.  When that list gets full, it is then pushed onto the main list to be returned.  
Parameters: size: This parameter is the size/length of each of the nested sublists.  
	puzzlelist: this is the list that needs to be broken into nested sublists.  
Return:
	Returns a list of all the elements however they are broken into their respective sublists
|#
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

#|
Name: entersize
Author: Benjamin Kaiser
Description:  This function literally consists of a single read function.  It gets a single value.  
Parameters:  N/A
Return:  The single value is input and must be set to another variable.  
|#
;get the size of the puzzle
(defun entersize()
	(let ((size 3))
		(read)
	)
)
  
#|
;For testing purposes to test these function independently
(setf puzsize (entersize))

;(setf puzzle (fileio (car *args*)))

(setf userpuz (userinput))

;(setf newpuzzle (getNested puzsize puzzle))

(setf newuserpuz (getNested puzsize userpuz))
|#

