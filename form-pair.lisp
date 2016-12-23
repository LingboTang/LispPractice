(def form-pair(L)
	(if (null(L))
		nil
		(if (= mod(length(L) 2) 0)
			(cons car(L) (cons (car(cdr(L)))))
		)
		(if (= mod(length(L) 2) 1)
			(cons car(L) (cons (car(cdr(L)))) (cons (car(cdr(L)))))
		)
	)
)
		