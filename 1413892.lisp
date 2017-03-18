;Mercy Woldmariam, Student ID: 1413892
;CMPUT 325 Wi17 Assignment 2

(defun fl-interp (E P)
	;If P is null, it will interpet a primitive function. For each primitive function, it will use the according built in
	; function to execute the task. Also, as can be seen fl-interp is called on the parameters in case any parameters require 
	; further reduction.
	
	(if (null P)
	  (cond 
		((atom E) E)   
			(t
			   (let ( (f (car E))  (arg (cdr E)) )
					(cond  ((eq f '+)  (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
								((eq f '-)  (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
									 ((eq f '*)  (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
										((eq f '<)  (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
											((eq f '>)  (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
												((eq f '=)  (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
													((eq f 'and)  (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
														((eq f 'or)  (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
															((eq f 'not)  (not (fl-interp (car arg) P)))
																((eq f 'isnumber)  (numberp (fl-interp (car arg) P)))
																	((eq f 'equal)  (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
																		((eq f 'null)  (null (fl-interp (car arg) P)))
																				((eq f 'atom)  (atom (fl-interp (car arg) P)))
																					((eq f 'eq)  (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
																						((eq f 'cons) (cons (car arg) (cadr arg)))
																							((eq f 'if)  (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
																								((eq f 'first) (fl-interp (caar arg) P))
																									((eq f 'rest)  (cdar arg)))      																				
				)            					
			)
		)
		nil
	)
)
