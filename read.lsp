;(defun readlist(fstream ))

; fileio function
(defun fileio ( filename )
    "(fileio filename): open an input file and read the data"
    (let (datahold)
      ; check for correct usage
      (when (null filename) (return-from fileio "Usage: fileio.lsp filename"))

      ; read through file using open
      (format t "~%Opening file ~a using open~%" filename)
      (setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
      (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
      (do ((data (read fin nil) (read fin nil)))          ; read entire file, returning NIL at EOF
          ((null data) (close fin) (reverse datahold))                       ; exit when file is read
          (push data datahold)
      )
    )
)

; call the fileio function, passing the first command-line argument as an input filename

;For testing purposes to test this function independently
;(setf puzzle (fileio(car *args*)))
