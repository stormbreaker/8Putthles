#|
                   ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Written 03/88 by John M. Weiss, Ph.D.

Modifications:
|#

(defvar *flag*)

(defun solvable (L)
    (setf *flag* nil)                               ; global *flag*
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

(defun disorder (elem L)
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)

#|
Usage: (large_solvable puzz)
        Where Puzz is any square integer length list that isn't 9
        E.G (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0)

Written 03/16 by Samuel Carroll
|#
(defun large_solvable (L)
    (let ((inversion) (solve) (curr_puzz))
        (setf curr_puzz L)
        (setf inversion 0)
        (dolist (i L)
            (setf curr_puzz (cdr curr_puzz))
            (dolist (cmp_val curr_puzz)
                (cond
                    ((= cmp_val 0))
                    ((> i cmp_val) (incf inversion))
                )
            )
        )

        (cond
            ((= (rem inversion 2) 0) t)
            (t nil)
        )
    )
)

#|
Usage: (solve_switch puzz length)
       Where Puzz is a list of size length*length

Written 03/16 by Samuel Carroll
|#
(defun solve_switch (puzz len)
    (cond
        ((= len 3) (solvable puzz))
        (t (large_solvable puzz))
    )
)
