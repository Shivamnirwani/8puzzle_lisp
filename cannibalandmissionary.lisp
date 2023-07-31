;Name : Shivam Nirwani
;ID : 1001923706
(defun main (m_nums c_nums b_capacity)
    (setf starting_loc (make-array 4))                      ; Length of the array is 4 to count Numbers of cannibals and missionary on both sides
    (setf b_capacity b_capacity)
    (setf (aref starting_loc 0) m_nums)
    (setf (aref starting_loc 1) c_nums)
    (setf (aref starting_loc 2) 0)
    (setf (aref starting_loc 3) 0)
    (uno_crossing b_capacity)
    (dos_crossing)
    (tres_crossing b_capacity)
    (rest_crossing b_capacity)
    (if (/=(aref starting_loc 0) 0)
        (PROGN
            
            (setf (aref starting_loc 2) 
            (+(aref starting_loc 2)
            (aref starting_loc 0)))
            (setf (aref starting_loc 3)
            (+(aref starting_loc 3)
            (aref starting_loc 1)))
            (setf remaining (aref starting_loc 0))
            (setf (aref starting_loc 0) 0)
            (setf (aref starting_loc 1) 0)
            (format t "~%(~S ~S)-(~S ~S) -> (~S ~S)" 
                  
                    ( aref starting_loc 0) (aref starting_loc 1) 
                  
                    remaining remaining
                  
                    (aref starting_loc 2) (aref starting_loc 3)

                ))))                                        ; On the 1st crossing, number of people on boat don't matter
(defun uno_crossing (b_capacity)
    (setf (aref starting_loc 3) b_capacity)
    (format t "~%(~S ~S)-(~S ~S)->(~S ~S)" 
                    ( aref starting_loc 0) (aref starting_loc 1) 
                    0 b_capacity 
                    (aref starting_loc 2) (aref starting_loc 3)
                )
    (setf (aref starting_loc 1) (-(aref starting_loc 1) b_capacity))
    )

(defun dos_crossing ()                                      ; 2nd crossing

    (setf (aref starting_loc 1) (+(aref starting_loc 1) 1))
    (setf (aref starting_loc 3) (-(aref starting_loc 3) 1))
    (format t "~%(~S ~S)<-(~S ~S)-(~S ~S)"
        ( aref starting_loc 0) (aref starting_loc 1)
        0 1
        (aref starting_loc 2) (aref starting_loc 3)
        )
    )

(defun tres_crossing (b_capacity)                           ; Next crossing
    (setf (aref starting_loc 0) (-(aref starting_loc 0)(- b_capacity 1)) )
    (setf (aref starting_loc 2) (+(aref starting_loc 2)(- b_capacity 1)) )
    (format t "~%(~S ~S)1-(~S ~S)->(~S ~S)" 
                    ( aref starting_loc 0) (aref starting_loc 1) 
                    (- b_capacity 1) 0
                    (aref starting_loc 2) (aref starting_loc 3)
                )
    )

(defun rest_crossing (b_capacity)                           ; Crossings till n-1
    (setf temp_size b_capacity)
    (if (/=(mod b_capacity 2) 0)(PROGN (setf temp_size (- temp_size 1))))
    (loop 
        (setf (aref starting_loc 0) (+(aref starting_loc 0)1) )
        (setf (aref starting_loc 1) (+(aref starting_loc 1)1) )
        (setf (aref starting_loc 2) (-(aref starting_loc 2)1) )
        (setf (aref starting_loc 3) (-(aref starting_loc 3)1) )
        (format t "~%(~S ~S)<-(~S ~S)-(~S ~S)"
        ( aref starting_loc 0) (aref starting_loc 1)
        1 1
        (aref starting_loc 2) (aref starting_loc 3)
        )
        (when (<=(+(aref starting_loc 0)(aref starting_loc 1))temp_size) (return))
        (setf (aref starting_loc 0)(-(aref starting_loc 0)(/ temp_size 2)))
        (setf (aref starting_loc 1)(-(aref starting_loc 1)(/ temp_size 2)))
        (setf (aref starting_loc 2)(+(aref starting_loc 2)(/ temp_size 2)))
        (setf (aref starting_loc 3)(+(aref starting_loc 3)(/ temp_size 2)))
        (format t "~%(~S ~S)-(~S ~S)->(~S ~S)" 
                    ( aref starting_loc 0) (aref starting_loc 1) 
                    (/ temp_size 2)(/ temp_size 2)
                    (aref starting_loc 2) (aref starting_loc 3)
                )
    )
 )