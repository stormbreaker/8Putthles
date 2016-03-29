#|
	***** output.lsp *****
Code to handle the final output of a 8-Tile puzzle solution path

Author: Samuel Carroll
Date March 2016
|#

; Output the final solution path, move by move
(defun prt_sol (sol)
    "(prt_sol sol): will take the solution path and print it"
    ;TODO find where the empty spot is, and follow the moves, actually all of this
    (let ((sol_len) (puzz_size) (arr_loc) (to_prt))

    (setf sol_len (list-length sol)) ; find how many puzzles we need to print
    (setf puzz_size (list-length (car sol))) ; get number of rows in puzzle
    (setf arr_loc (floor puzz_size 2)) ; arrow location is at half the rows
    (setf to_prt sol) ; copy the solution set, so we can print down the line

    (do ((num_prt 0 (setf num_prt (+ num_prt 4))) ((puzz1) () ()) ((puzz2) () ())
        ((puzz3) () ()) ((puzz4) () ()) (prt1) (prt2) (prt3) (prt4))
        ((> 0 (- sol_len num_prt)) T)

        ; check if we've hit the null point if so don't set
        (cond 
            ((null (car to_prt)) (setf prt1 nil))
            (t (setf puzz1 (car to_prt)) (setf prt1 t)))
        (cond
            ((null (nth 1 to_prt)) (setf prt2 nil))
            (t (setf puzz2 (nth 1 to_prt)) (setf prt2 t)))
        (cond
            ((null (nth 2 to_prt)) (setf prt3 nil))
            (t (setf puzz3 (nth 2 to_prt)) (setf prt3 t)))
        (cond
            ((null (nth 3 to_prt)) (setf prt4 nil))
            (t (setf puzz4 (nth 3 to_prt)) (setf prt4 t)))

        (dotimes (i puzz_size)
            (prt_row (nth i puzz1) prt1) ; need to print spacing and arrow
            (prt_row (nth i puzz2) prt2)
            (prt_row (nth i puzz3) prt3)
            (prt_row (nth i puzz4) prt4)
        )
        ; enter a loop to print each row of the puzzle indicate if we should
        ; print that puzzle

        (setf to_prt (cddddr to_prt)) ; get rid of first four puzzles in lst
    )
    ) ; end of let
)

; output the formatted puzzle
(defun prt_puzz (puzzle)
    "(prt_puzz puzzle) prints the specified puzzle where each row is it's own
    list of elements"
    (cond
        ((null (cdr puzzle)) (prt_row (car puzzle)))
        ((listp puzzle) (prt_row (car puzzle)) (prt_puzz (cdr puzzle)))
    )
)

; prints a specific row of the puzzle
(defun prt_row (row prt_row)
    "(prt_row row) prints a row of n elements for a tile puzzle"
    (cond
        ((null prt_row) nil)
        (t (cond
            ((null (cdr row)) (prt_elem (car row) nil))
            (t (prt_elem (car row) nil) (prt_row (cdr row)))
           )
        )
    )
)

; prints a single element of the puzzle
(defun prt_elem (elem end_row)
    "(prt_elem elem) prints an element of a puzzle's row, unless blank"
    (cond
        ((equal '0 elem) (format t " "))
        (t (format t "~D" elem))
    )

    (cond
        ((null end_row) (format t " ")) ; print space if not at the end of the row
        (t (format t "~%")) ; print new line if at the end of the row
    )
)
