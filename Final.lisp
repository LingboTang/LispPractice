% Write a Lisp program
% which returns a list of all prefixes of L. The order in which the prefixes appear in the resulting list is unimportant.

(defun prefix (L)
  (if (null L)
    (cons nil nil)
    (cons L (prefix(rmLast(L))))
  )
)

(defun rmLast (L)
  (if (null (cdr L))
    nil
    (cons (car L) (rmLast (cdr L)))
  )
)
