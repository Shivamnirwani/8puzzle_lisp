;Name : Shivam Nirwani
;Id : 1001923706
(defstruct state                       ;will store current state in 3x3 matrix
   abhi_position 
   l_cost 
   c_cost 
   r_cost
)

(defun iscorrect (start_pos)       ; counts the inversion in input
(setf cout 0)
(setf cout_invers 0)
(setf array_a (make-array 8))
(dotimes (i 3)
   (dotimes (j 3)
      (if (/=(aref start_pos i j) 0) (PROGN 
        (setf (aref array_a cout) (aref start_pos i j) ) 
        (incf cout)
        )
   )
)
   )

(loop for ivar from 0 to 7
 do (loop for jvar from (+ ivar 1) to 7
      do (PROGN (if (>(aref array_a jvar) (aref array_a ivar)) (incf cout_invers))
           )

  ) 
  )

(return-from iscorrect cout_invers)


  )


(defun calc_diff (uno dos)           ;calculates the c_cost
  (setf cout 0)
    (dotimes (i 3)
   (dotimes (j 3)
      (if (/= (aref uno i j)(aref dos i j)) (incf cout))
   )
)
(return-from calc_diff cout)
  )


(defun calc_blankmoves (pos_0)                    ;calculates where the blank tile can be placed
  (setf uno_v (nth 0 pos_0))
  (setf give_result (make-array '(4 2) :initial-contents '((-1 -1) (-1 -1) (-1 -1) (-1 -1)) ))
  (setf dos_v (nth 1 pos_0))
  (if (<=(+ uno_v 1)2) (PROGN 
    (setf (aref give_result 0 0 ) (+ uno_v 1))
    (setf (aref give_result 0 1 ) dos_v)

    )
  )
   (if (>=(- uno_v 1)0) (PROGN 
    (setf (aref give_result 1 0 ) (- uno_v 1))
    (setf (aref give_result 1 1 ) dos_v)

    )

  )
   (if (>=(- dos_v 1)0) (PROGN 
    (setf (aref give_result 2 0 ) uno_v)
    (setf (aref give_result 2 1 ) (- dos_v 1))

    )

  )
     (if (<=(+ dos_v 1)2) (PROGN 
    (setf (aref give_result 3 0 ) uno_v)
    (setf (aref give_result 3 1 ) (+ dos_v 1))


  )
   )
     (return-from calc_blankmoves give_result)
     )


(defun calc_nextstate (abhi_position_obj goal_state)           ;evaluate the next best state
  (setf abhi_min most-positive-fixnum)
  (setf last_considered_state (make-array '(3 3)))
  (setf abhi_position (state-abhi_position abhi_position_obj))
  (dotimes (i 3)
   (dotimes (j 3)
      (if (=(aref abhi_position i j) 0) (setf pos_0 (list i j) ) )
   )
)
(setf feasible_blank_states (calc_blankmoves pos_0) )

(dotimes (i 4)

     (if (/=(aref feasible_blank_states i 0) -1) (PROGN

      (setf new_costs (make-array '(3 3)) )

      (dotimes (i 3)
   (dotimes (j 3)
      (setf (aref new_costs i j) (aref (state-abhi_position abhi_position_obj)i j))
      
   )
   )
      (setf temp_var (aref new_set_of_values (aref feasible_blank_states i 0)(aref feasible_blank_states i 1)))
      (setf (aref new_set_of_values (aref feasible_blank_states i 0)(aref feasible_blank_states i 1))   (aref new_set_of_values (nth 0 pos_0)(nth 1 pos_0)) )
      (setf (aref new_set_of_values (nth 0 pos_0)(nth 1 pos_0)) temp_var)
      (setf new_c_cost  (calc_diff new_set_of_values goal_state))
      (if (<= new_c_cost abhi_min) (PROGN 
          (setf abhi_min new_c_cost)
          (dotimes (i 3)
   (dotimes (j 3)
      (setf (aref last_considered_state i j) (aref new_set_of_values i j) )
   )
   )
        ))

      ))
)

(format t " ~S -> ~S" 
                    (state-abhi_position abhi_position_obj) last_considered_state 

                )
(dotimes (i 3)
   (dotimes (j 3)
      (setf (aref (state-abhi_position abhi_position_obj) i j) (aref last_considered_state i j) )
   )
   )
(setf (state-l_cost abhi_position_obj) (+(state-l_cost abhi_position_obj) 1))
(setf (state-c_cost abhi_position_obj) abhi_min)
(setf (state-r_cost abhi_position_obj) (+(state-l_cost abhi_position_obj) (state-c_cost abhi_position_obj)))
  
  )

(defun puzzle()                                  ;to define start and goal state
 
  (setf start_pos (make-array '(3 3) :initial-contents '((1 2 0) (4 5 3) (7 8 6))))

  (setf invers_cout (iscorrect start_pos))
  (if(/=(mod invers_cout 2)0) (return-from puzzle ' infeasible_puzzles))
  (write 'Correct)
  (setf goal_state (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 0))))
  (setq abhi_position_obj (make-state :abhi_position start_pos
    :l_cost 0))
  (setf (state-c_cost abhi_position_obj) (calc_diff start_pos goal_state))
  (setf (state-r_cost abhi_position_obj) (calc_diff start_pos goal_state))
  (calc_nextstate abhi_position_obj goal_state)
  (loop 
    (when (= (state-c_cost abhi_position_obj) 0) (return))
    (calc_nextstate abhi_position_obj goal_state)
    (dotimes (i 3)
   (dotimes (j 3)
      (setf (aref start_pos i j) (aref (state-abhi_position abhi_position_obj) i j) )
   )
   )
    )
  (terpri )
  (format t "Nodes expanded = ~S" 
                    (+ (state-l_cost abhi_position_obj) 1) 
                )
  (setf completed ' completed)
  (return-from puzzle completed)

  )