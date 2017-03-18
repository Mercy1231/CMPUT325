;Mercy Woldmariam, Student ID: 1413892
;CMPUT 325 Wi17 Assignment 1 


;Question 1

;This function whether or not an object Y in within the list X. It first checks whether the X is null, and if not, begins to iterate throughout the list 
;comparing each element to Y. If it finds a match, it will print t. If not, it will print nil.

(defun xmember (X Y)
	(if (null X)
		nil
		(if (equal (car X) Y)
			t
			(xmember (cdr X) Y)
		)       
	)       
)


;Question 2

;This function flattens out a list such that every element is an atom. It first checks if the list is null,
;if not, it calls the flattencond function. 

(defun flatten (x)
	(if (null x)
		x
		(flattencond x)
	)
)

;The flattencond function iterates through the lists.
;It checks if each element is an atom, if not, it will call the flatten function on that element to flatten it as well.
;It continuously calls this function until it finds the atoms.

(defun flattencond (x)
	(cond ((atom (car x)) (cons (car x) (flatten (cdr x))))
		(t (append (flatten (car x)) (flatten (cdr x))))
	)
)


;Question 3

;The mix function along with its two helper functions, mixing and mixingCond, takes two separate lists and produces one combined
;list. The mix function begins by checking if the first list is null. If so, it outputs the second list untouched. If not, it 
; calls the mixing function.  The mixingCond function builds the list with three conditions. The first two conditions
; check if either list is empty and if so, returns the built list with the rest of the other list appended. If both lists still
; contain elements, it adds the first elements of the two lists to the combined list that is being built.
;It then recursively calls the mixing function with the new combined list and the rest of the other two lists.

(defun mix (x y)
     (if (null x)
	 	y
	 	(mixing x y nil)
     )  
)

;The mixing function then calls the mixingCond function which constructs the two lists together until 
; there are no elements in either list.

(defun mixing (x y z)
	(if (null (append x y))
		z
		(mixingCond x y z)
	)
)

;The mixingCond function builds the list with three conditions. The first two conditions
; check if either list is empty and if so, returns the built list with the rest of the other list appended. If both lists still
; contain elements, it adds the first elements of the two lists to the combined list that is being built.
;It then recursively calls the mixing function with the new combined list and the rest of the other two lists.

(defun mixingCond (x y z)
	(cond ((null y) (append z x))
			((null x) (append z y))
				(t (mixing (cdr x) (cdr y) (append z (cons (car y) (cons (car x) nil)))))
	)
)


;Question 4

;The layout of the split function is quite similar to the mix function. The spilt function accepts one list as input and 
;outputs two separate lists built from that list. The first function split checks if the list is null, if not, it calls
;the help split2 function.

(defun split (x)
	(if (null x)
		(cons 'nil '(nil))
		(split2 x nil nil)    
	)
)

;The split2 calls the splitCond.

(defun split2 (x y z) 
	(if (null x) 
		(cons y (cons z ()))
		(splitCond x y z)
	)
)

;The splitCond function checks the length of the leftover of the given
; list after each extraction of an element into the two new lists.

(defun splitCond (x y z)
	(cond ((null (cadr x)) (split2 (cddr x) (append y (cons (car x) nil)) z))
			(t (split2 (cddr x) (append y (cons (car x) nil)) (append z (cons (cadr x) nil))))
	)
)

;Question 5

;5.1
;Let L1 and L2 be lists. Is it always true that (split (mix L2 L1)) returns the list (L1 L2)? If yes, give a proof. 
;If no, describe exactly for which pairs of lists L1, L2 the result is different from (L1 L2).

;Answer:
;No, it does not always return the lists as they previously were. If the two lists are unequal in length, 
;then it will not return the lists as the were. (split (mix L2 L1)) will only return (L1 L2) when the lists originally 
;had the same number of elements.

;5.2
;Let L be a list. Is it always true that (mix (cadr (split L)) (car (split L))) returns L? If yes, give a proof. 
;If no, describe exactly for which lists L the result is different from L.

;Answer:
;Yes, it will always return L. split L returns two list, (L1, L2). If L was originally uneven, L1 will 
;receive the extra element and be longer than L2. The statement (mix (cadr (split L)) (car (split L))) = (mix L2 L1) which 
;builds one list from the two sublists starting with L1. If L1<L2, the last element will be tacked to the end and we have our 
;original list again.


;Question 6

;The subsetsum function relies on many functions in order to accomplish its goal. The subsetsum accepts two paramenters,
; a and b. a is a number and b is a list of number. It thens looks for a subset of b that adds up to a. Subsetsum accomplish 
; this in two major steps, the first being finding all the subsets of b. When comb b is passed as an argument in subsetsum,
; with helper functions, subset and combinations, all subsets of list b is found and compiled together in a list. 

(defun subsetsum (a b)
	(findSum a (comb b))
)

;From there findSum iterates through the list of subsets, finding the sums of each subset and comparing it to the given number we are 
; looking for. If a subset is found that amounts to a, it outputs that subset. IF not, it outputs nil.

(defun findSum (c d)
	(cond ((= c (sum 0 (car d))) (car d))
		((null (cdr d)) nil)
			(t (findSum c (cdr d)))
	)
)

(defun sum (a b)
	(if (null b)
		a
		(sum (+ a (car b)) (cdr b))
	)
)

; The comb function begins the proces of finding all the subsets of original list x. It first checks if 
; the list is null. If not, it calls the subset function with parameters x and an empty list.
(defun comb(x)
	(if (null x)
		x
		(subset x nil)
	)
)

;The subset function has two parameters x which has the elements of the list, and the second list y which is the list 
;of subsets we are building. The subset function has a recursive style with the base case being with x only has two 
;elements. When it reaches the base case (aka x = (a b)), subset appends three subsets (a), (b), (a b) to y.
;If x is longer than two elements, it recursively calls subset on the rest of the list until it hits the base case. When subset
; has hit the base case and returns, the calls to the combinations is made.

(defun subset (x y)
 	(cond ((null (caddr x)) (append y (cons (cons (car x) ()) (cons (cdr x) (cons x ())))))
 		(t (combinations (car x) (subset (cdr x) y) (subset (cdr x) y) ))
 	)
 )

;The combinations functions builds the combinations of the subsets of x with the result of y from the base case of subset and 
;the elements of x. If x is two elements or shorter, then combinations will never be called. Combinations is when x is longer than
; two elements and is called for elements that were in the list before the last two. What it does is that for each element a1, 
; it will reappend another version of each subset currently in y with the element a1 added to the list. For example, x = (a b c)
;subset function would output y = ((b) (c) (b c)) from the base case. Then for the leftover elements which is just a, it also
; appends (a b) (a c) (a b c). It will also append (a)
(defun combinations (z y w)
	(if (null w)
		(append y (cons (cons z ()) ()))
		(combinations z (append y (cons (cons z (car w)) ())) (cdr w))
	)
)