(defun sum1 (L)
	(if (null L)
		0
		(if (numberp L)
			L
			(+ (sum1 (car L) (sum1 (cdr L)))
		)
	)
)