#|
	Author: Lingbo Tang
	ID: 1353070
	Course: CMPUT 325
	Assignment: Assignment 1
|#


#|
	QUESTION 1
	This question requires that we can form a pair in a list with another
	order,For exameple: (form-pair '(a1 b 2 c 3)) ==> ((1 a) (2 b) (3 c))
	and (form-pair '(a1 b 2 c 3 d)) ==> ((1 a) (2 b) (3 c) (d d)). To solve
	this puzzle, we have to consider two cases: whether the list has an odd
	length or even length. We can solve this problem by duplicating the first
	atom in the list. In this way, we don't need to worry about how the length
	((cadr L) (cons (car L) nil)===> This line of code will form a pair and 
	leave a joint "nil" for the recursion. And the recursion for this question
	is easy. Just run the recursion for the list except the first pair.

	Test case: (form-pair '(a1 b 2 c 3)) ==> ((1 a) (2 b) (3 c))
|#



(defun form-pair (L)
    (if (null L)
        nil
        (if (null (cdr L))
            (cons (cons (car L) L) nil )
            (cons (cons (cadr L) (cons (car L) nil))
            (form-pair (cddr L)))
        )
    )
)



;Test case for QUESTION 1
(print (form-pair '(a 1 b 2 c 3 d)))


#|
	QUESTION 2
	This question requires that if there's same results for a list of operations
	which are divided in pairs say (drop-pair '(((+ 2 4) (* 3 4)) (6 (* 2 3)))) ==> ((6 12))
	Why is that? (+ 2 4) = 6, (* 3 4) = 12, we don't need to drop this pair.
	However, (* 2 3) = 6, we have to drop this pair.
	Since we can't use the built-in eval, we have to self-define a function 'evaluate' for this
	question. The recursion of this question is easy.
	Base case: if (eq (evaluate (cadr (car L))) (evaluate (caar L))), then drop this pair as 'nil'
	Recursion: Recursively call this function for the next pair.

	Test Case: (drop-pair '(((+ 2 4) (* 3 4)) (6 (* 2 3)))) ==> ((6 12))
|#


(defun drop-pair (L)
	(cond 
		((null L) nil)
		((eq (evaluate (cadr (car L))) (evaluate (caar L))) nil)
		(T (cons (list (evaluate (caar L)) (evaluate (cadr (car L)))) (drop-pair (cdr L))))
	)
)


;Self-define function 'evaluate'
;If the list is just an atom, just return the value
;otherwise we apply the first atom "operator"(car L) to the
;rest of the list(cdr L).
(defun evaluate(L)
	(if (atom L)
		L
		(apply (car L) (cdr L))
	)
)
	


;Test case for QUESTION 2
(print (drop-pair '(((+ 2 4) (* 3 4)) (6 (* 2 3)))))

#|
	Quesiton 3
	This quesiton requires It takes x as a list of atoms and 
	removes repeated ones in x. The order of the elements in
	the resulting list should preserve the order in the given list.
	Example: (remove-duplicate '(a b c a d b)) ==> (a b c d)
	or (c a d b)
	The idea is simple, carv the first atom, recursively finding if
	the first atom is in the cdr List, once find it, pop out, and then
	recursively run it for the rest of the Lisp. We can just keep the
	first appearence or last appearence. But either way requires a 
	"findAppearence" function to find the duplicate.
	
	Test case: (remove-duplicate '(a b c a d b)) ==> (c a d b)

|#

(defun remove-duplicate (L)
	(cond
		((null L) nil)
		((null (findAppearence (car L) (cdr L))) (remove-duplicate (cdr L)))
		(T (cons (findAppearence (car L) (cdr L)) (remove-duplicate (cdr L))))
	)
)



;define the "findAppearence" function here
;I find out it's not easy to keep the first appeared atom,
;because you have to go over the list and then going back,
;not easy to make a recursion. Say for the example '(a b c a d b)
;It's easy to get 'a' and 'b' removed, however, if doing recursion
;for 'c' if you go over the list, and doing (if (null L) nil)(if (eq n (car L))
;nil (findAppearence) n (cdr L)) you will drop 'c' and get nil.
;Therefore, the easier way to remove is only keep the last element.


(defun findAppearence (n L)
    (if (null L)
        n
        (if (eq n (car L))
			nil            
            (findAppearence n (cdr L))
        )
    )
)

;Test case for QUESTION3
(print (remove-duplicate '(a b c a d b)))


#|
	QUESTION 4
	Easiest Question in the Assignment
	Empty list is simply 0,
	The recursion is count(cdr L) + 1
	"+ 1" is simply the (car L)
	Remove the duplicates, and that's it!.
	

	Test case: (my-count '(a b a c d c)) ==> 4
	
|#


(defun my-count (L)
	(cond
		((null L) 0)
		((null (findAppearence (car L) (cdr L))) (+ (my-count (cdr L)) 0))
		(T (+ (my-count (cdr L)) 1))
	)
)


;Test case for QUESTION 4
(print (my-count '(a b c a d b)))


#|
	QUESTION 5
	Returns the power set the List
	So basicly, set the nil as the basic case
	Find the subset for the list
	including the list itself
	We need at list two function
	But the recursion for the main function should be pretty easy
	Just like the Quesiton 4,
	The idea is (nil powerset(L))
	Then I'm doing recursion on (cdr L)
	I used a lambda calculus operator to "define" a function in power-set
	"(lambda (subList) (cons (car L) subList)) (power-set psrest))" means
	take (power-set psrest) as the subList and then do the operation (cons
	(car (power-set psrest)) (power-set psrest))

	Test case: (power-set '(A B C D)) ==> (NIL (D) (C) (C D) (B) (B D) (B C) (B C D) (A) (A D) (A C) (A C D)
     (A B) (A B D) (A B C) (A B C D))
|#

(defun power-set(L)
	(if (null L) '(nil)
 		(append (power-set (cdr L)) (mapcar #'(lambda (subList) (cons (car L) subList)) (power-set (cdr L))))
	)
)


 		
;Test case for QUESTION 5
(print (power-set '(A B C D)))



#|
	QUESTION 6a
	This question is to find the best and worst grade of the course
	of a certain person in a list of '(name,course_name,grade)
	This is still simple, get the name, recursively reading the list.
	If the given name is equal to the tuple name,check the grade, compare
	the two nearest pair, and update the 'worst' and 'best' dynamically.
	However, it's seems like lisp can't compare the string directly.
	So we have to set the weight for the letter grade at first.
	At the beginning set the default worst to 11, set the default best to 0

	Test case: (findOne 'john 'worst '((john cmput201 A-) (lily cmput114 A) (ann cmput115 B) (john cmput229 C+) (lily cmput115 B-) (john cmput325 A+) (lily cmput229 A)))) ==> (cmput229 C+)

|#


(defun findOne (Name Type L)
	(cond
    	((eq Type 'worst) (cdr (findWorst Name 11 nil L)))
		((eq Type 'best) (cdr (findBest Name 0 nil L)))
		(T nil)
	)
)

;This is the function to find the worst in the list.
;Basically we set the default worst as the best one
;and set the WorstList as nil as our initial state.
;Because if the list is empty, we have to return an
;empty list for sure. Once we find a grade that is worse
;than the best one, we set the worse one as our new default
;and update the worslist as well. Keep updating this till the
;end of the list. We can get it.

(defun findWorst (Name default WorstList L)
	(if (null L)
		WorstList
		(if (eq Name (caar L)) 
			(if (null WorstList)
				(findWorst Name default (car L) (cdr L))
				(if (< (toNumber (cadr (cdar L))) (toNumber (car (cddr WorstList))))
					(findWorst Name default (car L) (cdr L))
					(findWorst Name (toNumber (cadr (cdar L))) WorstList (cdr L))
				)
			)
			(findWorst Name (toNumber (cadr (cdar L))) WorstList (cdr L))
		)
	)
)



;The same as the worst list but just need to flip the order of comparing.

(defun findBest (Name default BestList L)
	(if (null L)
		BestList
		(if (eq Name (caar L)) 
			(if (null BestList)
				(findBest Name default (car L) (cdr L))
				(if (> (toNumber (cadr (cdar L))) (toNumber (car (cddr BestList))))
					(findBest Name default (car L) (cdr L))
					(findBest Name (toNumber (cadr (cdar L))) BestList (cdr L))
				)
			)
			(findBest Name (toNumber (cadr (cdar L))) BestList (cdr L))
		)
	)
)

	
;Tonumber function
(defun toNumber (x)
    (if (null x)
        nil
        (cond
            ((eq x 'F) 0)
            ((eq x 'D) 1)
            ((eq x 'D+) 2)
            ((eq x 'C-) 3)
            ((eq x 'C) 4)
            ((eq x 'C+) 5)
            ((eq x 'B-) 6)
            ((eq x 'B) 7)
            ((eq x 'B+) 8)
            ((eq x 'A-) 9)
            ((eq x 'A) 10)
            ((eq x 'A+) 11)
            (T nil)
        )
    )
)

;Test case for QUESTION 6a
(print (findOne 'john 'worst '((john cmput201 A-) (lily cmput114 A) (ann cmput115 B) (john cmput229 C+) (lily cmput115 B-) (john cmput325 A+) (lily cmput229 A))))



#|

	QUESTION 6b
	This question is asking for finding all the grade of a people
	The idea is pretty simple, find all available list in the sublist

	Test case: (findAll 'ann '((john cmput201 A-) (lily cmput114 A) (ann cmput115 B) (john cmput229 C+) (lily cmput115 B-) (john cmput325 A+) (lily cmput229 A))) ==>
	((cmput115 B))

|#


(defun findAll (name L)
	(cond 
		((null L) nil)
		((eq name (caar L)) (cons (cdar L) (findAll name (cdr L))))
		(T (findAll name (cdr L)))
    )
)


;Test case for QUESTION 6b:
(print (findAll 'ann '((john cmput201 A-) (lily cmput114 A) (ann cmput115 B) (john cmput229 C+) (lily cmput115 B-) (john cmput325 A+) (lily cmput229 A))))

#|
	QUESTION 7a
	This question is a little bit complicated.
	But still simple, because I'm using the DFS algorithm
	Start from the first beginning page, tranversal the branch
	until it's reaches it furthest depth, for the second step delete the node
	If the page is redirect to itself, just ignore them.

	Test Case: (reached 'a '((a b) (b c) (b e) (p b))) ==> (e c b)

|#


(defun reached (x L)
	(cond 
		((eq x nil) nil)
		((null L) nil)
		(T (remove-first x (find-connected nil (list x) L)))
	)
)

;But the thing is how do we implement the DFS-liked algorithm?
;First of all to check each pair, we have to know if the pair
;contains the joint we want to find (see function contains)
;If the head page is linked to the previous end, we need to check
;If the head page is also linked to the end by 
;(find-connected current (cdr pending) L)
;However, they are not linked, we want to tranversal the next pair
;And remove the redundancy.
;Other helper function are remove-first and connected-pages.
;The fomer one is used to remove the target page
;The latter one is used to just detect the linkage between head and end

(defun find-connected (current pending L)
	(cond
		((eq nil pending) current)
		((contains (car pending) current) (find-connected current (cdr pending) L))
		(T (find-connected (cons (car pending) current) (remove-duplicate (append pending (connected-pages (car pending) L))) (cdr L)))
	)
)


(defun contains (x L)
	(cond 
		((null L) nil)
		((eq x (car L)) T)
		(T (contains x (cdr L)))
	)
)

(defun connected-pages (x L)
	(cond
		((eq x nil) nil)
		((null L) nil)
		((eq x (caar L)) (cons (car(cdr(car L))) (connected-pages x (cdr L))))
		(T (connected-pages x (cdr L)))
	)
)

(defun remove-first (x L)
	(cond
		((eq x nil) L)
		((null L) nil)
		((eq x (car L)) (remove-first x (cdr L)))
		(T (cons (car L) (remove-first x (cdr L))))
	)
)



(print (reached 'a '((a b) (b c) (b e) (p b))))

#|
	QUESTION 7b
	This Quesiton returns the sorted list by the order of rank.
	The order of rank is to see if the page is mostly related.
	So, how I solve this question is I traversal the list and 
	record the number of connections. And then sort them using the
	sort function provided in the hint.

 	Test case: (rank '(a b) '((a a) (b a) (q b) (a b))) ==> (b a)

|#


(defun rank (pages L)
	(cond
		((null pages) nil)
		(T (hideRank (rankSort (getRank pages L))))
	)
)

;But the thing is to get the Sortfunction work we need to construct
;a list which lets the rank attatched to each of the pages.
;Countlinks updates it's rank when it find the linkage between the pages
;and construct a list with (page rank) pair in getRank.

(defun countLinks (x L)
	(cond
		((null L) 0)
		((and (not (eq x (caar L))) (eq x (cadr (car L)))) (+ (countLinks x (cdr L)) 1))
		(T (+ (countLinks x (cdr L)) 0))
	)
)

;
(defun getRank (pages L)
	(cond
		((null pages) nil)
		((null L) nil)
		(T (cons (list (car pages) (countLinks (car pages) L)) (getRank (cdr pages) L)))
	)
)

;Once we get the ranked list, we want to hide the rank, so just simply delete the rank

(defun hideRank (L)
	(cond 
		((null L) nil)
		(T (cons (caar L) (hideRank (cdr L))))
	)
)


(defun rankSort (L)
	(sort L 'rankOrder)
)

(defun rankOrder (L1 L2)
	(> (cadr L1) (cadr L2))
)

(print (rank '(a b) '((a a) (b a) (q b) (a b))))
