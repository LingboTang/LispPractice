(print (reverse '(1 2 3)))
;;	=>(3 2 1)

(print (card '(1 2) '(a b c)))
;;	=>((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))

(print (flatten '((a b) (b) c)))
;;	=>(a b b c)


;;>(let ((x 3) (y 4)) (* (+ x y) x))
;;21

;;> (* (+ 3 4) 3)
;;21


;;Let is a built-in function that just follow it's meaning "Assingnment"
(let ((x 1) (y (+ 1 x))) (+ x y y)))

(let ((x 1)) (let ((y (+ 1 x))) (+ x y y y)))

(let* ((x 1) (y (+ 1 x))) (+ x y y))
;;((x 1) (y (+ 1 x))) expression
;;(+ x y y) to be evaluated

(defun f (x) (+ x 1))

(let ((x (f 3))) (+ 2 x x))


;;Use "Cond" as "Switch" in C

;;Defun rev

(defun rev (L)
	(if (null L)
		L
		(append (rev (cdr L))	(list (car L)))
	)
)

(defun card (L1 L2)
	(if (null L1)
		nil
	(append	(pair (car L1) L2) 
		      (card (cdr L1) L2))))

(defun pair (E L)
	(if (null L)
		nil
		(cons	(list E (car L)) 
			(pair E (cdr L))

		)
	)
)
