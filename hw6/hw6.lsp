;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; node2var
; Parameters:
;		n - number of the node
;   c - number of the color gets
;   k - number of possible colors
;
; Return value:
;   A number representing the index of the variable
;   that corresponds to the fact that 
;  "node n gets color c" (when there are k possible colors).
(defun node2var (n c k)
  (+ (* (- n 1) k) c)  
)

; at-least-one-color
; Parameters:
;		n - number of the node
;   c - number of the color gets
;   k - number of possible colors
;
; Return value:
;   A list representing a clause for the constraint:
;   "node n gets at least one color from the set {c,c+1,...,k}."
;
; Solution:
;   Using node2var function, return all possibilities of node n receiving
;   some color from c..k
(defun at-least-one-color (n c k)
  (cond ((> c k) nil)
        (t
          (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k))
        )
  )
)

; combine-negative-pairs
; Parameters:
;		start - a number
;   curr - the current number
;   end - the max number
;
; Return value:
;   A list containing pairs of all possible numbers between start and end, in negative
;   literals.
(defun combine-negative-pairs (start curr end)
  (cond ((> curr end) nil)
        (t
          (cons (list (* start -1) (* curr -1)) (combine-negative-pairs start (+ curr 1) end))
        )
  )
)

; combine-pairs
; Parameters:
;		start - a number
;   curr - the current number
;   end - the max number
;
; Return value:
;   A list containing pairs of all possible numbers between start and end
(defun combine-pairs (start curr end)
  (cond ((> curr end) nil)
        (t
          (cons (list start curr) (combine-pairs start (+ curr 1) end))
        )
  )
)

; at-most-one-helper
; Parameters:
;		start - a number
;   end - the max number
;
; Return value:
;   A list containing pairs of the form:
;     (~start | ~start+1) & (~start | ~start+2) & ... (~end-1 | ~end)
;
; Solution:
;   Delegate the work to combine-negative-pairs.
(defun at-most-one-helper (start end)
  (cond ((= start end) nil)
        (t
          (append (combine-negative-pairs start (+ start 1) end) (at-most-one-helper (+ start 1) end))
        )
  )
)

; at-most-one-color
; Parameters:
;		n - number of the node
;   c - number of the color gets
;   k - number of possible colors
;
; Return value:
;   A list representing a clause for the constraint:
;   "node n gets at most one color from the set {c,c+1,...,k}."
;
; Solution:
;   We use the fact that the DNF representing 
;     (node n gets c but not c+1, c+2, ..., k) or 
;     (node n gets c+1 but not c, c+2, ..., k) or
;     (node n gets k but not c, c+1, ..., k-1)
;   e.g. a DNF:
;     (a & ~b) | (~a & b)
;   can be converted to the CNF:
;     (~a | ~b)
;   
;   Using this fact, we delegate the work to at-most-one-helper
;   and return the result.
(defun at-most-one-color (n c k)
  (cond ((= c k) nil)
        (t
          (let ((helper-list (at-least-one-color n c k)))
            (at-most-one-helper (first helper-list) (car (last helper-list)))
          )
        )
  )
)

; at-most-one-color
; Parameters:
;		n - number of the node
;   k - number of possible colors
;
; Return value:
;  A list of clauses to ensure that
;  "node n gets exactly one color from the set {1,2,...,k}."
;
; Solution:
;   We just combine our previous constraints of at-least-one and at-most-one into
;   one CNF.
(defun generate-node-clauses (n k)
  (let ((at-least-one (at-least-one-color n 1 k)) (at-most-one (at-most-one-color n 1 k)))
    (append (list at-least-one) at-most-one)
  )
)

; edge-clause-helper
; Parameters:
;		e - a list representing an edge
;   curr - the current color
;   k - number of possible colors
;
; Return value:
;  A list of clauses to ensure that
;  "the nodes at both ends of edge e cannot have the 
;  same color from the set {1,2,...,k}."
;
; Solution:
;  For each curr, we know that the constraint can be expressed as the DNF:
;   ~(node 1 gets curr & node 2 gets curr)
;  which can be converted using the DeMorgan's Law to the CNF:
;   ~node 1 gets curr | ~node 2 gets curr
;  
;  Using this fact, we can obtain the list of edge clauses.
(defun edge-clause-helper (e curr k)
  (cond ((null e) nil)
        ((< curr 1) nil)
        ((> curr k) nil)
        (t
          (append
            (list (list (* (node2var (first e) curr k) -1) (* (node2var (second e) curr k) -1)))
            (edge-clause-helper e (+ curr 1) k)
          )
        )
  )
)

; generate-edge-clauses
; Parameters:
;		e - a list representing an edge
;   k - number of possible colors
;
; Return value:
;   A list of clauses to ensure that
;   "the nodes at both ends of edge e cannot have the 
;   same color from the set {1,2,...,k}."
;
; Solution:
;  Delegate the work to edge-clause-helper
;  and return the result.
(defun generate-edge-clauses (e k)
  (cond ((null e) nil)
        (t
          (edge-clause-helper e 1 k)
        )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun