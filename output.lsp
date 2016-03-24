#|
	***** output.lsp *****
Code to handle the final output of a 8-Tile puzzle solution path

Author: Samuel Carroll
Date March 2016
|#

; Output the final solution path, move by move
(defun prt_sol (puzzle moves)
    "(prt_sol puzzle moves): will take the inital puzzle and following moves
    print the solution path"
    ;TODO find where the empty spot is, and follow the moves, actually all of this

    (format t "~A" (car puzzle))
    (format t "~A\n" (cadr puzzle))
    (format t "~A\n" (cdr puzzle))
    (format t "hello")
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
(defun prt_row (row)
    "(prt_row row) prints a row of n elements for a tile puzzle"
    (cond
        ((null (cdr row)) (format t "~D~%" (car row)))
        (t (format t "~D " (car row)) (prt_row (cdr row)))
    )
)
