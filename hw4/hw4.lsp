; Chaoran Lin
; CS 161 HW 4


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; helper functions

; is-var-true?
; Parameters:
;   var - a variable 1, 2, 3...
;   assignment - a list representing the assignment of boolean values to each variable
;
; Return value:
;   A boolean value indicating whether the variable evaluates to true in the assignment
;
; Solution:
;   Simply use recursion until we reach the assignment for the variable we are interested in
(defun is-var-true? (var assignment)
  (cond ((null var) nil)
        ((null (car assignment)) t)
        ((= (abs var) (abs (car assignment))) 
          (= var (car assignment))
        )
        (t 
          (is-var-true? var (cdr assignment))
        )
  )
)

; is-clause-true?
; Parameters:
;   clause - a clause in a CNF 
;   assignment - a list representing the assignment of boolean values to each variable
;   in clause
;
; Return value:
;   A boolean value indicating whether the clause evaluates to true under the given assignment.
;
; Solution:
;   Simply use recursion to look at whether the current variable evaluates to true, or the rest of the
;   clause evaluates to true
(defun is-clause-true? (clause assignment)
  (cond ((null clause) nil)
        (t
          (or (is-var-true? (car clause) assignment) (is-clause-true? (cdr clause) assignment))
        )
  )
)

; is-true?
; Parameters:
;   delta - a CNF
;   assignment - a list representing the assignment of boolean values to each variable
;
; Return value:
;   A boolean value indicating whether the CNF evaluates to true under the given assignment.
;
; Solution:
;   Simply use recursion to look at whether the current clause evaluates to true and the rest of the
;   CNF evaluates to true
(defun is-true? (delta assignment)
  (cond ((null delta) t)
        (t
          (and (is-clause-true? (car delta) assignment) (is-true? (cdr delta) assignment))
        )
  )
)

; clause-contains?
; Parameters:
;   var - the variable to search for
;   clause - a clause in CNF, or just a general list
;
; Return value:
;   A boolean value indicating whether var is found in clause.
;
; Solution:
;   Simply use recursion to look at whether an element in clause is equal to var.
(defun clause-contains (var clause)
  (cond ((null clause) nil)
        ((= var (abs (car clause))) (car clause))
        (t (clause-contains var (cdr clause)))
  )
)

; get-containing-clauses
; Parameters:
;   var - the variable to search for
;   delta - a CNF
;
; Return value:
;   A list of clauses in delta that contain var.
;
; Solution:
;   Simply use recursion to check whether each clauses contains var using clause-contains?
;   If so, add it to the return list.
(defun get-containing-clauses (var delta)
  (cond ((null delta) (list '()))
        ((not (null (clause-contains var (car delta))))
          (cleanUpList (append (list (car delta)) (get-containing-clauses var (cdr delta))))
        )
        (t
          (cleanUpList (get-containing-clauses var (cdr delta)))
        )
  )
)

; get-map
; Parameters:
;   n - the number of variables
;   delta - a CNF
;
; Return value:
;   A list representing a dictionary/mapping between each variable and the clauses that contain them.
;
; Solution:
;   Use get-containing-clauses to collect all clauses containing every i such that i <= n.
(defun get-map (n delta)
  (cond ((= n 0) (list '()))
        ((null delta) (list '()))
        (t
          (cleanUpList (append (list (list n (get-containing-clauses n delta))) (get-map (- n 1) delta)))
        )
  )
)

; get-freq-map
; Parameters:
;   n - the number of variables
;   delta - a CNF
;
; Return value:
;   A list representing a dictionary/mapping between each variable and the number of times they appear
;   in the CNF.
;
; Solution:
;   Use get-map to get a mapping between each variable and the clauses that contain them, and just use 
;   that information to create a new map that maps each variable to the number of clauses that contain them
(defun get-freq-map (n delta)
  (let* ((map (get-map n delta)) (entry (car map)))
    (if (null entry)
      (list '())
      (if (= n (first entry))
        (cleanUpList (append (list (list n (length (second entry)))) (get-freq-map (- n 1) delta)))
      )
    )
  )
)

; delete-from-list
; Parameters:
;   target - the element to delete
;   l - a list
;
; Return value:
;   A new list with the target removed from the original list. If target is not removed, return original list
;
; Solution:
;   Use recursion to traverse until we get to target, and exclude it from the rest
(defun delete-from-list (target l)
  (cond ((null l) l)
        ((equal target (car l)) (cdr l))
        (t
          (cons (car l) (delete-from-list target (cdr l)))
        )  
  )
)
; unit clauses

; get-unit-clauses?
; Parameters:
;   delta - a CNF
;
; Return value:
;   A list of clauses that only have 1 variable in them.
;
; Solution:
;   Simply use recursion to collect and append all clauses that have a length of 1
(defun get-unit-clauses (delta)
  (cond ((null delta) nil)
        ((= 1 (length (car delta)))
          (append (car delta) (get-unit-clauses (cdr delta)))
        )
        (t
          (get-unit-clauses (cdr delta))
        )
  )
)

; unit-clause-helper
; Parameters:
;   unit-clauses - a list of unit clauses
;
; Return value:
;   A list of boolean assignments to each unit clause that would make them evaluate to true.
;   Each element in the list is of the form (unit-clause assignment-value)
;
; Solution:
;   For each unit clause, look at its value and from there determine which assignment value makes it true.
;   Append for all
(defun unit-clause-helper (unit-clauses)
  (cond ((null unit-clauses) nil)
        (t
          (if (> (car unit-clauses) 0)
              (append (list (list (car unit-clauses) 1)) (unit-clause-helper (cdr unit-clauses)))
              (append (list (list (car unit-clauses) -1)) (unit-clause-helper (cdr unit-clauses)))
          )
        )
  )
)

; get-unit-clause-constraints
; Parameters:
;   delta - a CNF
;
; Return value:
;   A list of boolean assignments to each unit clause that would make them evaluate to true.
;   Each element in the list is of the form (unit-clause assignment-value)
;
; Solution:
;   Just get the unit clauses first and delegate work to unit-clause-helper
(defun get-unit-clause-constraints (delta)
  (let ((unit-clauses (get-unit-clauses delta)))
    (unit-clause-helper unit-clauses)
  )
)

; pure literals

;
; cleanUpList (l)
; returns l with any NIL or (NIL) element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if (or (not (null cur)) (and cur (not (equal (list '()) cur))))
		 (cons cur res)
		  res
		 )
	     )
	   )
  )
)

; get-pure-literal-value
; Parameters:
;   var - the pure literal
;   clause - a clause in a CNF
;
; Return value:
;   The value of var in the clause
;
; Solution:
;   Search through each variable in clause until we obtain the value that matches var
(defun get-pure-literal-value (var clause)
  (cond ((null clause) 0) 
        (t 
          (or (clause-contains var clause)
              (get-pure-literal-value var (cdr clause))
          )
        )
  )
)

; get-pure-literals
; Parameters:
;   map - a mapping between the variables in a CNF and the clauses they appear in
;
; Return value:
;   A list containing all variables that appear just once in the CNF.
;
; Solution:
;   Using map, just collect all variables that has a value of 1 in the map.
(defun get-pure-literals (map)
  (cond ((null map) (list '()))
        ((= 1 (length (second (car map))))
          (cleanUpList 
            (append 
              (list (list (first (car map)) (get-pure-literal-value (first (car map)) (car (second (car map))))))
              (get-pure-literals (cdr map))
            )
          )
        )
        (t
          (cleanUpList (get-pure-literals (cdr map)))
        )
  )
)

; pure-literal-helper
; Parameters:
;   pure-literals - a list of pure literals in a CNF
;
; Return value:
;   A list of boolean assignments to each pure literal that would make them evaluate to true.
;   Each element in the list is of the form (pure-literal assignment-value)
;
; Solution:
;   For each pure literal, look at its value and from there determine which assignment value makes it true.
;   Append for all
(defun pure-literal-helper (pure-literals)
  (cond ((null pure-literals) nil)
        (t
          (if (> (second (car pure-literals)) 0)
              (append (list (list (first (car pure-literals)) 1)) (pure-literal-helper (cdr pure-literals)))
              (append (list (list (first (car pure-literals)) -1)) (pure-literal-helper (cdr pure-literals)))
          )
        )
  )
)

; pure-literal-constraints
; Parameters:
;   n - the number of variables in CNF
;   delta - a CNF
;
; Return value:
;   A list of boolean assignments to each pure literal in the CNF that would make them evaluate to true.
;   Each element in the list is of the form (pure-literal assignment-value)
;
; Solution:
;   Use the helper functions map and pure-literals to obtain the list of pure literals in delta, and 
;   just delegate the work to pure-literal-helper from there.
(defun get-pure-literal-constraints (n delta)
  (let* ((map (get-map n delta)) (pure-literals (get-pure-literals map)))
    (pure-literal-helper pure-literals)
  )
)

; get-constraints
; Parameters:
;   n - the number of variables in CNF
;   delta - a CNF
;
; Return value:
;   A list of boolean assignments to variables in CNF that would make them evaluate to true, at the
;   point of which we know must be true (constraint).
;   Each element in the list is of the form (variable assignment-value)
;
; Solution:
;   Aggregate the unit clause constraints and pure literal constraints.
(defun get-constraints (n delta)
  (append 
    (get-unit-clause-constraints delta) 
    (get-pure-literal-constraints n delta)
  )
)

; search-pairs
; Parameters:
;   var - the variable to search for
;   pairs - a list in which all elements appear as pairs (a b)
;
; Return value:
;   The pair whose first element matches var. Otherwise return an empty list.
;   This function is typically used to search for the value we decided to assign to 
;   a variable in a list of constraints.
;
; Solution:
;   Simply use recursion to compare var and the first element of each pair until we find
;   a match.
(defun search-pairs (var pairs)
  (cond ((null var) (list '()))
        ((null pairs) (list '()))
        ((= var (first (car pairs)))
          (car pairs)
        )
        (t
          (search-pairs var (cdr pairs))
        )
  )
)

(defun next-var (n var constraints)
  (cond ((null constraints)
          (if (< n (+ var 1))
            1
            (+ var 1)
          )
        )
        (t (first (car constraints)))
  )
)

; sat-helper
; Parameters:
;   n - the number of variables in the CNF
;   count - the current variable we are trying to assign a value to
;   delta - the CNF
;   assignment - the current assignment of values to the variables in CNF
;   constraints - the current set of constraints to help with our assignment
;
; Return value:
;   A list representing the assignment we give to each variable in CNF that would make the CNF
;   evaluate to true. For example, if 1 and 2 should be true and 3 should be false, then
;   this would return '(1 2 -3).
;
; Solution:
;   Use a backtracking approach to determine the value of the variable in our assignment.
;   If we have pre-determined its value in the constraint, search it, get its value, and assign
;   it instead. Otherwise, assign 0 to the value and keep on going to see if it makes the
;   assignment produce a true result. If it doesn't, backtrack to 1. Do this at each level
;   in the search tree.
;; (defun sat-helper (n count delta assignment constraints)
;;   (if (null delta) 
;;       nil
;;       (let ((constraint (search-pairs count constraints)))
;;         (cond ((not (equal (list '()) constraint))
;;                 (sat-helper n (+ count 1) delta (append assignment (list (* count (second constraint)))) (delete-from-list constraint constraints))
;;               )
;;               ((null assignment)
;;                 (or (sat-helper n (+ count 1) delta (append assignment (list count)) constraints)
;;                     (sat-helper n (+ count 1) delta (append assignment (list (* count -1))) constraints)
;;                 )
;;               )
;;               ((= n (length assignment))
;;                 (if (is-true? delta assignment)
;;                   assignment
;;                   nil
;;                 )
;;               )
;;               (t
;;                 (if (is-true? delta assignment)
;;                   (or (sat-helper n (+ count 1) delta (append assignment (list count)) constraints)
;;                       (sat-helper n (+ count 1) delta (append assignment (list (* count -1))) constraints)
;;                   )
;;                 )
;;               )
;;         )
;;       )
;;   )
;; )

(defun sat-helper (n count delta assignment constraints) 
  (if (null delta)
    nil
    (let* ((constraint (search-pairs count constraints)) (next (next-var n count (delete-from-list constraint constraints))))
      (cond ((= n (length assignment))
              (if (is-true? delta assignment)
                assignment
                nil
              )
            )
            ((not (equal (list '()) constraint))
              (sat-helper n next delta (append assignment (list (* count (second constraint)))) (delete-from-list constraint constraints))
            )
            ((null assignment)
              (or (sat-helper n next delta (append assignment (list count)) constraints)
                  (sat-helper n next delta (append assignment (list (* count -1))) constraints)
              )
            )
            (t
              (if (is-true? delta assignment)
                (or (sat-helper n next delta (append assignment (list count)) constraints)
                    (sat-helper n next delta (append assignment (list (* count -1))) constraints)
                )
              )
            )
        )
    )
  )
)

; sat?
; Parameters:
;   n - an integer
;   delta - a CNF defined over n variables
;
; Return value:
;   A list of n integers, representing a model of delta, if delta is satisfi-
;   able, otherwise it returns NIL
;
; Solution:
;   Get the constraints on the CNF, and call sat-helper to do the rest.
(defun sat? (n delta)
  (let ((constraints (get-constraints n delta)))
    (sat-helper n (next-var n 1 constraints) delta '() constraints)
  )
)

; solve-cnf?
; Parameters:
;   filename - the name/path of a file
;
; Return value:
;   A list of assignment values that makes the cnf in the file evaluate to true
;
; Solution:
;   Use sat? to solve the CNF
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

; Test cases
; (sat? 3 '((1 − 2 3) (−1) (−2 − 3))) returns (−1 − 2 3).
; (sat? 1 ′((1) (−1))) returns NIL