(funcall 
       (function (lambda (x y) (+ (* x x) y))) 
       4 6
 )

(print (funcall 
       (function (lambda (x y) (+ (* x x) y))) 
       4 6
 ))

(defun get-param (P)
	(if (null P)
		nil
		(if (eq (car P) '=)
			nil
			(cons (car P) (get-param (cdr P)))
		)
	)
)

(defun real-param (P)
	(cdr (get-param (car P)))
)			

(defun get-body (P)
	(if (null P)
		nil
		(if (eq (car P) '=)
			(cdr P)
			(get-body (cdr P))
		)
	)
)

(defun real-body (P)       
	(car (get-body (car P)))
)

(defun replace-logic (E)
	(if (null E)
		nil
		(if (atom (car E))
			(if (eq (car E) 'true)
				(cons 't (cdr E)))
			(if (eq (car E) 'False)
				(cons 'nil (cdr E)))
			)
		)
	)
)

(print (replace-logic '(true nil)))

(print (apply '(lambda (x y) (if (> x y) x (if (< x y) y nil))) '(3 5)))
(print (apply '(lambda (x y) (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))) '((1 2 3) 4)))
(print (if (interp '(xor true nil) '((xor x y = (if (equal x y) nil true)))) 'U4-OK 'U4-error))

#|
(print (if (eq (interp '(last (s u p)) '((last x = (if (null (rest x)) (first x) (last (rest x)))))) 'p) 'U6-OK 'U6-error))
(print (if (equal (interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4)) 'U7-OK 'U7-error))
(print (if (equal (interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2)) 'U8-OK 'U8-error))
(print (if (eq (interp '(power 4 2) '((power x y = (if (= y 1) x (power (* x x) (- y 1)))))) '16) 'U9-OK 'U9-error))
(print (if (eq (interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) '24) 'U10-OK 'U10-error))
(print (if (eq (interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) '6) 'U11-OK 'U11-error))
|#
