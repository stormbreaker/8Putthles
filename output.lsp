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

    (do ((num_prt 0 (setf num_prt (+ num_prt 4))) (puzz1) (puzz2)
        (puzz3) (puzz4) (prt1) (prt2) (prt3) (prt4))
        ((> 0 (- sol_len num_prt)) T)

        ; check if we've hit the null point if so don't set
        (cond 
            ((null (car to_prt)) (setf prt1 0))
            (t (setf puzz1 (car to_prt)) (setf prt1 t)))
        (cond
            ((null (nth 1 to_prt)) (setf prt2 0))
            (t (setf puzz2 (nth 1 to_prt)) (setf prt2 t)))
        (cond
            ((null (nth 2 to_prt)) (setf prt3 0))
            (t (setf puzz3 (nth 2 to_prt)) (setf prt3 t)))
        (cond
            ((null (nth 3 to_prt)) (setf prt4 0))
            (t (setf puzz4 (nth 3 to_prt)) (setf prt4 t)))

        (dotimes (i puzz_size)
            (prt_row (nth i puzz1) prt1)
            ; use row arrow location and if we have another value
            (prt_spc i arr_loc prt2)
            (prt_row (nth i puzz2) prt2)
            (prt_spc i arr_loc prt3)
            (prt_row (nth i puzz3) prt3)
            (prt_spc i arr_loc prt4)
            (prt_row (nth i puzz4) prt4)
            (prt_spc i arr_loc (cddddr to_prt))
            (format t "~%") ; print newline after all rows have been printed
        )
        (format t "~%") ; print newline after puzzles have been printed

        (setf to_prt (cddddr to_prt)) ; get rid of first four puzzles in lst
    )
    ) ; end of let
)

#|; output the formatted puzzle
(defun prt_puzz (puzzle)
    "(prt_puzz puzzle) prints the specified puzzle where each row is it's own
    list of elements"
    (cond
        ((null (cdr puzzle)) (prt_row (car puzzle)))
        ((listp puzzle) (prt_row (car puzzle)) (prt_puzz (cdr puzzle)))
    )
)|#

; prints a specific row of the puzzle
(defun prt_row (row prt_row)
    "(prt_row row) prints a row of n elements for a tile puzzle if indicated"
    (cond
        ((eq prt_row 0) nil)
        (t (cond
            ((null (cdr row)) (prt_elem (car row) nil (list-length row)))
            (t (prt_elem (car row) nil (list-length row)) (prt_row (cdr row) t))
           )
        )
    )
)

; prints a single element of the puzzle
(defun prt_elem (elem end_row size)
    "(prt_elem elem end_row size) prints an element of a puzzles row, unless blank"
    (cond
        ((equal '0 elem)
         (if (> 3 size) (format t "   ")
             (format t "  ")))
        (t (format t "~2D" elem))
    )

    (cond
        ((null end_row) (format t " ")) ; print space if not at the end of the row
        (t (format t "~%")) ; print new line if at the end of the row
    )
)

; use row arrow location and if we have another value
; prints the seperators between rows of puzzles
(defun prt_spc (row_num arr_loc tail_val)
    "(prt_spc row_num arr_loc tail_val ) 
     print spaces between rows and arrow as needed (i.e. another value"
    (cond 
        ((equal row_num arr_loc)
            (cond
                ((null tail_val) (format t "     "))
                ((equal tail_val 0) (format t "     "))
                (t (format t " ->  "))
            )
        ); end equal statement
        (t (format t "     "))
    )
)

