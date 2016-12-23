#|
  Course: CMPUT 325
  Prof: Jia You
  Student: Lingbo Tang
  StudentID: 1353070
  Assign_num: Assignment 2
|#

; Interp recursively goes through the E without changing P
; For primitive function, it's pretty easy to interp them.
; Why? Cuz you just apply the primitive function directly
; to the values in E.
; The thing is we can easily find the function and arguements
; by calling (car E) and (cdr E) directly and interp one by one.


(defun Interp (E P)
	(cond
		;if atom E just return the value
		((atom E) E)
		(T
			; Get-function is another helper function to get the the whole function
			; in the program P
			(let* ( (f (car E)) (arg (cdr E)) (function (get-function f P)) )
				(cond
					; Primitive functions
					; The other way to zip the space of the interp function is to use member function
					; to check if f is in the primitive function list.
					; However, to avoid those error comming from the refactoring, I just try to stay safe
					; on ugly code.
					((member f '(+ - * > < = cons eq equal first rest null atom)) (apply f (interp-two arg P))) 
					((eq f 'and) (and (interp (car arg) P) (interp (car (cdr arg)) P)))
					((eq f 'or) (or (interp (car arg) P) (interp (car (cdr arg)) P)))
					((eq f 'number) (numberp (interp (car arg) P)))
					((eq f 'not) (not (interp (car arg) P)))
					((eq f 'if) (if (interp (car arg) P) (interp (car (cdr arg)) P) (interp (car (cdr (cdr arg))) P)))
					; If P is not nil, it means it is a user defined function
					; Then we have to do the substitution of the arguments and corresponding variables in the
					; Program P, return the arguments out as the new arguments and do the recursive interp
					; until function goes to nil and then interp will call (T E) to return the value
					(function (interp (car (substitutes-all (get-vars (cdr function)) (get-body function) arg)) P))			
					(T E)
				)
			)
		)
	)
)

(defun interp-two (args P)
	(if (null args) 
		nil
		(cons (Interp (car args) P) (interp-two (cdr args) P))
	)
)


; Get the function part easily by calling checking if the first atom
; of the definition list is equal to the given function name. If it 
; is not equal, recursively cheking the rest of the P
(defun get-function (f P)
	(if (null P)
		nil
		(if (eq f (caar P)) 
			(car P)
			(get-function f (cdr P))
		)
	)
)


; Get the variables of the function by easily cheking the position of the
; equal sign, and return everything as a list before the equal sign.
; if we can't find the equal sign after all the recursions, return nothing.
; However, we are not done yet, what we get includes a function name.
; So when we call this in the interp(E P) function, we call this function
; after we get (cdr function) to get rid of the function name.
(defun get-vars (L)
	(if (eq (car L) '=)
		nil
		(cons (car L) (get-vars (cdr L)))
	)
)

; Same idea as the get-vars, but this time just return all the things after
; the equal sign as a list.
(defun get-body (L)
	(if (eq (car L) '=)
		(cdr L)
		(get-body (cdr L))
	)
)

; Replace corresponding var by arg in the function list.
; Compare each atom of the body in the P to var, if the
; atom = var, just get replace by arg, other wise, we leave the
; atom as before, and recursively checking that for (car body)
; and (cdr body) if the whole function body is not an atom.
(defun substitutes (var body arg)
	(if (eq var nil)
		nil
		(if (atom body)
			(if (eq var body)
				arg
				body
			)
			(cons (substitutes var (car body) arg) (substitutes var (cdr body) arg))
		)
	)
)

		 
; Simple, just recursively call the function defined above to
; replace the var by arg, but this time we have to go through the
; vars and args correspondingly. Finally, we return the replaced body
;
(defun substitutes-all (vars body args)
	(if (null vars)
		body
		(if (null (cdr vars))
			(substitutes (car vars) body (car args))
			(substitutes-all (cdr vars) (substitutes (car vars) body (car args)) (cdr args))
		) 
	)
)
