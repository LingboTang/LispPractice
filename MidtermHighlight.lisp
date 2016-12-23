(defun superList (S1 S2)
	(if (null S2)
		T
		(and (member (car S2) S1) (superList S1 (cdr S2)))
	)
)

(print (superList '(b b e a) '(a b)))


(print (mapcar (function (lambda (x) (+ x 1))) '(1 2 3 4 5)))



; map(inc, ((John . 23000) (Mary . 54560)))
; -->cons((John . 23100), map(inc ((Mary . 54560))))
; -->cons((John . 23100), cons((Mary . 54660), map(inc , NIL)))
; -->cons((John . 23100), cons((Mary . 54660), NIL))
; -->((John . 23100) (Mary . 54660))
; What's the difference between the dot pair and the list-like pair?
; That is a good quesiton. This question can be denoted as
; What's the difference between cons and list?
; Because if you are using list, it's not easy to make difference between
; lists so it's not easy for you to do recursion on list.
; However, with a dot notation there, it's easy to find where to start the
; the list recursion and where to stop, and where to generate the same notation
; Where to put a node and where to make a tree.
; That's a great inventation in lisp.
; Reference: http://www-formal.stanford.edu/jmc/recursive.html


(defun inc (N)
	(cons (car N) (+ (cdr N) 100))
)

(defun raise(L)
	(if (null L)
		nil
		(cons (inc (car L)) (raise (cdr L)))
	)
)

(print (raise '((John . 23000) (Mary . 54560))))

; reduce(f, id, L) =
; if null(L) then id
; else f(car(L), reduce(f, id ,cdr(L)))


(print (reduce #'* '(2 6 4)))


; reduce(*, 1, (2 6 4))
; -->(* 2 (* 6 (* 4 1)))
; -->(* 2 (* 6 4))
; -->(* 2 24)
; 48

; reduce(+, 0, map(inc, L))
; reduce(+, 0, get_number(map(inc, L)))

; where get_number(L) = if null(L) then nil
; else cons(cdr(car L)), get_number(cdr(L)))

; reduce(+, map(cdr (map(inc, L))))

; filter(Pre,L) = if null(L) then nil 
; else if Pre(car L) then cons(car L, filter(Pre,cdr L))
; else filter(Pre,cdr(L))


; vectormath
; Apply a list F of functions to an object x and get the list of all results of ; the applications.

; vector(F,x) = if null(F) then nil
; else cons(car(F)(x),vector(cdr(F),x))

; Define higher order functions in Lisp
; This can be achieved in Lisp by the builtin functions "apply" or "funcall".

; (apply function_name (arg1 .. argn))
; (funcall function_name arg1...argn)

; When apply, need to put a pair of parentheses around argns/
; (list arg1..argn) => (arg1..argn)


; map f for one of the atom in the list each time
(defun xmapcar1 (f L)
	(if (null L)
		nil
		(cons (funcall f (car L)) (xmarpcar1 f (cdr L)))
	)
)

; map f for all of the atom in the list each time
(defun xmapcar2 (f L)
	(if (null L)
		nil
		(cons (apply f (list (car L))) (xmapcar2 f(cdr L)))
	)
)

(defun xreduce (f L Idnetity)
	(if (null L)
		Idnetity
		(funcall f (car L) (xreduce f (cdr L) Idnetity))
	)
)

(defun xfilter (Pre L)
	(if (null L)
		nil
		(if (not (eq (funcall Pre (car L)) nil))
			(cons (car L) (xfilter Pre (cdr L)))
			(xfilter Pre (cdr L))
		)
	)
)

; Use lisp to solve the algorithms problem
; This is the example of binary tree
; How to represent a binary tree in lisp?
; 
;
; empty tree	by nil
; a tree with one node 	by (nil value nil) where value is the node value
; and in general	by (left_subtree value right_subtree)

(defun insert (Tr Int)
	(if (isEmptyTree Tr)
		; Int is the root of the tree
		(create_tree (create_emptry_tree) Int (create_empty_tree))
		(if (eq (node_value Tr) Int)
			; Insert tree on the root node
			Tr
			(if (< (node_value Tr) Int)
				; If the insert value < root node, insert left
				; Otherwise insert right
				(create_tree (left_subtree Tr)
					(node_value Tr)
					(insert (right_subtree Tr) Int)
				)
				(create_tree (insert (left_subtree Tr) Int)
					(node_value Tr)
					(right_subtree Tr)
				)
			)
		)
	)
)


(defun isEmptyTree (Tr)
     (null Tr)
)

(defun create_empty_tree ()
     nil
)   

; (Left Node Right)

(defun create_tree (L N R)
     (cons L (cons N (cons R nil)))
)   

(defun node_value (Tr)
	(car (cdr Tr))
)


(defun left_subtree (Tr)
     (car Tr)
)

(defun right_subtree (Tr)
   (car (cdr (cdr Tr)))
)

(defun superList (S1 S2)
	(if (null S2)
		T
		(and (member (car S2) s1) (superList S1 (cdr S2)))
	)
)

(defun sub (X V E)
	(cond	
		; First check if the E is an atom
		; Because only an atom can be replaced
		; Then check if it is equal to the given X
		; if so, replace, otherwise moving on to 
		; recursion. This is the basic part!!!!!
		((atom E) (if (eq E X) V E))
		; Then write the recursion part
		; For the recursion part
		; Devide and counqour! Pick the first one (car part)
		; And the rest one (cdr part)
		(T (cons (sub X V (car E)) (sub X V (cdr E))))
	)
)

; Useful Basic functions
; (atom X) --> to determine if X is an atom or not
; (null X) --> to determine if X is empty or not
; (eq X Y) --> to determine if X and Y are equal
; (equal X Y) --> to determine if X list vector and Y list vector are equal
; (numberp X) --> to determine if X is a number or not
; (append X Y) --> append Y to the end node of X
; (car X) -->  get the first element of a list!!! --Really useful doing recuresion!
; (cdr X) --> get the rest of the element in the list
; (cons X Y) --> cons a dot pair
; (if X Y Z) --> if condition
; (cond ...) --> condition control
; (let ((x y) (u v)) z)
; (let* ((x y) (u v)) z)
(let ((x 3) (y 4)) (* (+ x y) x))
; (* (+ 3 4) 3) --> 21
(let ((x (+4 2)) (y (+ 3 2))) (* x x y y))
; (* (+ 4 2) (+ 4 2) (+ 3 2) (+ 3 2)) --> 900
; (defun ...) --> define a function
; (quote x) and its short form 'x (made x an variable)\
; (print ...)
; (list ...) construct the real list, ignore the dot pair form
; (sort L fun)
; (defun rankSort (L)
;	(sort L 'rankOrder)

; (defun rankOrder (L1 L2)
; 	(> (cadr L1) (cadr L2))
; )

; (string= x y) compare string
; (string < x y) compare string
; (string-lessp x y) compare string, but case sensitive
; (funcall ...)
; (apply ...)
; (function ...)

; Funcall is useful when the programmer knows the length of the argument list.
; But the function to call is either computed or provided as a parameter. For
; instance, a simple implementation of MEMBER-IF (with noe of thje fancy options)
; could be written as:
; (defun member-if (predicate list)
; 	(do ((tail list (cdr tail)))
;	    ((null tail))
;	(when (funcall predicate (car tail))
;	    (return-from member-if tail)))))

; Apply is needed when the argument list itself is supplied or computed
; Its last argument must be a list, and the elements of this list become
; individual arguments to the function. This frequently occurs when a function
; takes keyword options that will be passed on to some other function,
; perhaps with application-specific defaults inserted. 

;  
; (defun funcall (function &rest arguments)
;  (apply function arguments))

; https://www.cs.cmu.edu/Groups/AI/html/faqs/lang/lisp/part3/faq-doc-11.html

(defun f (x) (* 5 x))

(let ((y (f 3))) (cons y (cons (+ y 1) (cons (+ y 2) nil))))

; >(15 16 17)

; However, you should not think of let as something similar to an assignment statement in C or Pascal. 
; Let only allows a name to be bound to an expression. You can only use it within an appropriate scope. Consider
; (let ((x 1)) (let ((x (+ x 1))) x))
;       |                 |
;       -------------------

; This is a nested let expression. 
; The occurrence of x in (+ x 1) is bound to the outside x. This expression is equivalent to
; (let ((x 1)) (let ((y (+ x 1))) y))

; The let* special form is just like let except that it allows values to reference variables defined earlier in the let*. For example.

(setq x 7)

(let* ((x 1) (y (+ x 1))) y)

; The form (let* ((x a) (y b)))
; is equivalent to (let ((x a)) (let ((y b))))

(defun call (E)
	(let ((op (car E))
	     (arg1 (cadr E))
	     (arg2 (caddr E)))
	     (cond
		((eq op 'ADD) (+ arg1 arg2))
           	((eq op 'SUB) (- arg1 arg2))
           	((eq op 'MUL) (* arg1 arg2))
           	((eq op 'DIV) (/ arg1 arg2))
           	((eq op 'LOG) (log arg1 arg2))   ; arg2 is base
           	((eq op 'EPN) (xexp arg1 arg2))
           	(T (print "invalid expression")))))

(defun xexp (a1 a2)
	(if (eq a2 0)
		1
		(* a1 (xexp a1 (- a2 1)))
	)
)

; Define a function
;       (drop L N)
; such that given a list of expressions in lisp syntax,
; evaluate each and if the result equals N, then drop it
; from the resulting list.
;
; Point: without using let, your code may look clumsy.
;
; The program uses deeply nested ifs; you may want to
; revise it using cond

(defun drop(L N)
	(if (null L)
		nil
		; If (car L) is a number
		(if (numberp (car L))
			; Then just check if (car L) == N
			(if (eq (car L) N)
				; If yes, do the recursion for the rest
				; of the list.
				(drop (cdr L) N)
				; Otherwise, drop this and recursion.
				(cons (car L) (drop (cdr L) N))
			)
			; Setting up a function to drop this
			(let ((x (apply (caar L) (cdar L))))
				(if (eq x N)
					(drop (cdr L) N)
					(cons x (drop (cdr L) N))
				)
			)
		)
	)
)

* (reduce '- '( 2 6 4))
; * (- 6 4)
; * (- 2 (- 6 4))
; * (- (- 2 6) 4)

; Note that (reduce 'cons '((1 2) (3 4) (5 6 7)))
; > (((1 2) 3 4) 5 6 7)

(defun RPN (L) (RPN1 Lnil))

(defun RPN1 (L stack)
	(if (null L)
		stack
		(if (numberp (car L))
			(RPN1 (cdr L) (cons (car L) stack))
		(let ((result (funcall (car L) (car stack) (cadr stack))))
			(RPN1 (cdr L) (cons result (cddr stack)))
		)
		)
	)
)

; Simple compose function

(defun twice (f x)
	(funcall f (funcall f x))
)




