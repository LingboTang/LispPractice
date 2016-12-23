;;--------------------------count
(defun countNr(L)
	(if (car L)
		(+ 1 (countNr (cdr L)))
		 0)
)

;;(print (countNr '(1 2 3 4 5)))


;;TAB or NEWLINE differs the condition and return value.
(defun maxl (L)
	(cond
		((null (cdr L))		(car L))
		((> (car L) (maxL (cdr L))) (car L))
		(T	(maxL (cdr L)))
	)
)

;;(print (maxL '(3 1 4 7 3 5 9 2 6)))

(defun flatten (L)
	(cond
		((null L) nil)
		((atom (car L)) (cons (car L) (flatten (cdr L))))
		(T	(append (flatten (car L)) (flatten (cdr L))))
	)
)


(print (countNr '(1 2 3 4 5)))
(print (maxL '(1 2 5 6 3 1)))
;;(print (count '(1 2 3 4 5)))
;;(print (max '(1 2 5 6 3 1)))
(print (flatten '((1 (2)) 3 (4 5))))
