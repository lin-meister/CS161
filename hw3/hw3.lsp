;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;

; goal-test
; Parameters:
; 	s - the state of the game
;   
; Return value:
;	 A boolean value indicating whether s is the goal state.
; 
;	Solution:
;		Simply call h1, which gives the number of misplaced boxes in s,
;		and check whether h1 is greater than 0 or not.
(defun goal-test (s)
	(let ((boxesLeft (h1 s)))
		(if (> boxesLeft 0)
			nil
			t
		)
	)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; get-square-content 
; Parameters:
;   row - the row to look at
;		col - the current column; initial call is 0
;
;	Return value:
;		A number representing the content of the square at (row, col)
;
;	Solution:
;		Uses recursion to traverse to position col in row, and retrieve content
(defun get-square-content (row col)
	(if (or (< col 0) (> col (- (length row) 1)))
			wall
			(cond ((= col 0)
							(let ((square (car row)))
								(cond ((isBlank square) blank)
											((isWall square) wall)
											((isBox square) box)
											((isKeeper square) keeper)
											((isStar square) star)
											((isBoxStar square) boxstar)
											((isKeeperStar square) keeperstar)
								)
							)
						)
				(t
					(get-square-content (cdr row) (- col 1))	
				)
			)
	)
)

; get-square
; Parameters:
; 	s - the state of the game
;   row - the target row
;		col - the target col
;
; Return value:
;		A number representing the square at row r and column c in the state s.
; 
;	Solution:
;		Achieves this by checking each element/row in s. If the row is our target row,
; 	then delegate work of checking the exact element in the row to get-square-content.
;		Otherwise, use recursion to find the target row in the rest of the state s.
(defun get-square (s r c)
	(if (or (< r 0) (> r (- (length s) 1)))
			wall
			(if (= r 0)
					(get-square-content (car s) c)
					(get-square (cdr s) (- r 1) c)
			)
	)
)

; set-square-content (helper function for set-square)
; Parameters:
;   row - the row to look at
;		col - the current column; initial call is 0
;		v - the content to set the square to
;
; Return value:
;   A list representing the new row, which is the same as the old row except 
;		with the content at row and col replaced with v. If col is ever out of bounds,
;		it returnsthe original row.
;
;	Solution:
;		Use recursion to progress in the list until we reach col, then set the content
;		to v.
(defun set-square-content (row col v)
	(if (or (< col 0) (> col (- (length row) 1)))
			row
			(if (= col 0)
					(cons v (cdr row))
					(cons (car row) (set-square-content (cdr row) (- col 1) v))
			)
	)
)

; set-square
; Parameters:
;		s - the state of the game
;   row - the target row
;		col - the target col
;		v - the content to set the square to
;
; Return value:
;   A list representing the state with the content at (row, col) updated with v
;
;	Solution:
;		Use recursion to progress in the list until we reach the target row, then call
;		set-square-content to set teh content in the current row
(defun set-square (s r c v)
	(if (or (< r 0) (> r (- (length s) 1)))
			s
			(if (= r 0)
					(cons (set-square-content (car s) c v) (cdr s))
					(cons (car s) (set-square (cdr s) (- r 1) c v))
			)
	)
)	

; can-move-to
; Parameters:
;		s - the state of the game
;   row - the target row
;		col - the target col
;
; Return value:
;   A boolean value indicating whether the keeper can move to (row, col)
;
;	Solution:
;		Allow keeper to move to the square (row, col) as long as the content there is not
;		a wall
(defun can-move-to (s r c)
	(let ((dest (get-square s r c)))
		(not (isWall dest))
	)
)

; can-move-box-to
; Parameters:
;		s - the state of the game
;   row - the target row
;		col - the target col
;
; Return value:
;   A boolean value indicating whether the box can move to (row, col)
;
;	Solution:
;		Allow box to move to the square (row, col) as long as the content there is blank
;		or a goal.
(defun can-move-box-to (s r c)
	(let ((dest (get-square s r c)))
		(or (isBlank dest) (isStar dest))
	)
)

; try-move-box
; Parameters:
;		s - the state of the game
;   x - the row that box is in
;		y - the column that box is in
;		direction - the direction to move the box, indicated by a number:
;								0 = up
;								1 = down
;								2 = left
;								3 = right
;
; Return value:
;   A list representing the state after moving the box from (x, y) in the given direction. 
;		If such a move is not possible, returns NIL
;
;	Solution:
;		Assuming that all the conditions for a valid move are met, updates the destination square
;		to a box (if it is originally blank) or a boxstar (if it is originally a goal).
(defun try-move-box (s x y direction)
	(if (> direction 3) 
			nil
			(let* ((dest (cond ((= direction 0) (list x (- y 1)))
												 ((= direction 1) (list x (+ y 1)))
												 ((= direction 2) (list (- x 1) y))
												 ((= direction 3) (list (+ x 1) y))
									 )
						 )
						 (next-x (first dest))
						 (next-y (second dest))
						)
				(if (can-move-box-to s next-x next-y)
						(if (isStar (get-square s next-x next-y))
							(set-square s next-x next-y boxstar)
							(set-square s next-x next-y box)
						)
						nil
				)
			)
	)
)

; try-move
; Parameters:
;		s - the state of the game
;   x - the row that keeper is in
;		y - the column that keeper is in
;		direction - the direction to move the keeper, indicated by a number:
;								0 = up
;								1 = down
;								2 = left
;								3 = right
;
; Return value:
;   A list representing the state after moving the keeper from (x, y) in the given direction. 
;		If such a move is not possible, returns NIL
;
;	Solution:
;		Assuming that all the conditions for a valid move are met, updates the destination square
;		to a keeper (if it is originally blank) or a keeperstar (if it is originally a goal).
;		If the item in front of the keeper is a box or a boxstar, then it moves the box out of that
;		square by calling try-move-box and updates accordingly. It also updates the squares that
;		the keeper and the box left behind to either a goal (if it was originally a goal) or
;		blank (if otherwise).
(defun try-move (s x y direction)
	(if (> direction 3) 
			nil
			(let* ((dest (cond ((= direction 0) (list x (- y 1)))
												 ((= direction 1) (list x (+ y 1)))
												 ((= direction 2) (list (- x 1) y))
												 ((= direction 3) (list (+ x 1) y))
									 )
						 )
						 (next-x (first dest))
						 (next-y (second dest))
						 (object-on-next (get-square s next-x next-y))
						 (object-to-move (if (or (isStar object-on-next) (isBoxstar object-on-next))
						 										 keeperstar
																 keeper
						 								 )
						 )
						)
				(if (can-move-to s next-x next-y) 
					(let ((s1 (set-square s next-x next-y object-to-move)))
						(if (= keeperstar (get-square s x y))
							(cond ((isBox object-on-next)
											(try-move-box (set-square s1 x y star) next-x next-y direction)
										)
										(t
											(set-square s1 x y star)									
										)			
							)
							(cond ((or (isBox object-on-next) (isBoxstar object-on-next))
											(try-move-box (set-square s1 x y blank) next-x next-y direction)
										)
										(t
											(set-square s1 x y blank)
										)			
							)
						)
					)
					nil
				)
			)
	)
)

; next-states
; Parameters:
;		s - the state of the game
;
; Return value:
;   A list representing the possible next states of s after making a move
;
;	Solution:
;		Just list out the possible states after trying a move by the keeper in the
;		direction of up, down, left, and right.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 			 (x (car pos))
	 			 (y (cadr pos))
	 			 ;x and y are now the coordinate of the keeper in s.
	 			 (result 
					(list 
						(try-move s y x 0)
						(try-move s y x 1)
						(try-move s y x 2)
						(try-move s y x 3)
					)	
				 )
	 			)
    (cleanUpList result);end
  );end let
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; h1
; Parameters:
;		s - the current state of the game
;
; Return value:
;   The number of misplaced boxes in s
;
;	Solution:
;		In the base case, check if state holds a box, and return
;		1 if so, otherwise 0. Else, use recursion to sum up
;		the number of misplaced boxes throughout s.
(defun h1 (s)
	(cond ((atom s)
					(if (isBox s)
						1
						0
					)
				)
				(t
					(if (null (cdr s))
						(h1 (car s))
						(+ (h1 (car s)) (h1 (cdr s)))
					)
				)
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; next-states
; Parameters:
;		r - the row to look at
;		cols - the columns to pair row up with
;
; Return value:
;   A list that contains all the pairs achieved by pairing row with the columns
;		in cols
;
;	Solution:
;		Use recursion to pair up the row with cols
(defun listCoords (r cols)
	(cond ((null cols) nil)
				(t (append (list (list r (car cols))) (listCoords r (cdr cols))))
	)
)

; getBoxColumns (helper function for getBoxPositions)
; Parameters:
;		r - the row to look at
;		col - the current column; initial call is 0
;
; Return value:
;   A list that represents the columns in row in which a box is found
;
;	Solution:
;		Use recursion to traverse the row and collect columns in which boxes appear
(defun getBoxColumns (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r))
	       (cons col (getBoxColumns (cdr r) (+ col 1)))
				 (getBoxColumns (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

; getBoxPositions 
; Parameters:
;		s - the current state of the game
;		row - the current row; initial call is 0
;
; Return value:
;   A list that contains the coordinates in s where a box is found 
;
;	Solution:
;		Use recursion to traverse each row, and call getBoxColumns to collect the columns
;		in that row where boxes appear.
(defun getBoxPositions (s row)
  (cond ((null s) nil)
	(t (let ((cols (getBoxColumns (car s) 0)))
	     (if cols
					 (listCoords row cols)
					 (getBoxPositions (cdr s) (+ row 1))
			 );end if
	       );end let
	 );end t
	);end cond
);end defun

; getGoalColumns (helper function for getGoalPositions)
; Parameters:
;		r - the row to look at
;		col - the current column; initial call is 0
;
; Return value:
;   A list that represents the columns in row in which a goal is found
;
;	Solution:
;		Use recursion to traverse the row and collect columns in which goal appear
(defun getGoalColumns (r col)
  (cond ((null r) nil)
	(t (if (isStar (car r))
	       (cons col (getGoalColumns (cdr r) (+ col 1)))
				 (getGoalColumns (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

; getGoalPositions 
; Parameters:
;		s - the current state of the game
;		row - the current row; initial call is 0
;
; Return value:
;   A list that contains the coordinates in s where a goal is found 
;
;	Solution:
;		Use recursion to traverse each row, and call getGoalColumns to collect the columns
;		in that row where goals appear.
(defun getGoalPositions (s row)
  (cond ((null s) nil)
	(t (let ((cols (getGoalColumns (car s) 0)))
	     (if cols
					 (listCoords row cols)
					 (getGoalPositions (cdr s) (+ row 1))
			 );end if
	       );end let
	 );end t
	);end cond
);end defun

; get-distance-between
; Parameters:
;		p1 - a list of length 2 representing a coordinate
;		p2 - a list of length 2 representing a coordinate
;
; Return value:
;   A number representing the manhattan distance between p1 and p2
;
;	Solution:
;		Use the manhattan distance formula to obtain the distance
(defun get-distance-between (p1 p2)
	(cond ((null p1) 0)
				((null p2) 0)
				(t
					(let ((x1 (first p1)) (y1 (second p1)) (x2 (first p2)) (y2 (second p2)))
								(cond ((> x1 x2)
												(if (> y1 y2)
														(+ (- x1 x2) (- y1 y2))
														(+ (- x1 x2) (- y2 y1))
												) 
											)
											(t 
												(if (> y1 y2)
														(+ (- x2 x1) (- y1 y2))
														(+ (- x2 x1) (- y2 y1))
												)
											)
								)
					)			
				)
	)
)

; get-min-distance-to-goal
; Parameters:
;		box - the coordinates representing a box's location
;		goals - a list of coordinates representing the locations of goals
;
; Return value:
;   The coordinates of the goal nearest to box in goals
;
;	Solution:
;		Use recursion; compare the distance between the first goal
;		and the box, with the min distance obtained from the rest of the
;		goals list. Repeat until we get min
(defun get-min-distance-to-goal (box goals)
	(cond ((null box)
					nil
				)
				((null goals)
					nil
				)
				(t 
					(let ((first-dist (get-distance-between box (car goals)))
								(min-rest (get-min-distance-to-goal box (cdr goals)))
							 )
							 (if (null min-rest)
							 		 	first-dist
										(if (< first-dist min-rest)
												first-dist
												min-rest
										)
							 )
					)
				)
	)
)

; get-manhattan-distance
; Parameters:
;		boxes - a list of coordinates representing the locations of boxes
;		goals - a list of coordinates representing the locations of goals
;
; Return value:
;   The sum of the manhattan distances for each box to get to a goal.
;
;	Solution:
;		Just gather the minimum distances for each box to get to a goal using
;		get-min-distance, and sum them up.
(defun get-manhattan-distance (boxes goals)
	(cond ((null boxes)
					0
				)
				((null goals)
					0
				)
				((atom boxes) 
					(get-min-distance-to-goal boxes goals)
				)
				(t
					(+ (get-min-distance-to-goal (car boxes) goals) (get-manhattan-distance (cdr boxes) goals))
				)	
	)
)

; h004674598
; Parameters:
;		s - the current state of the game
;
; Return value:
;   A heuristic value for A* to use to solve the Sokoban game
;
;	Solution:
;		Get the positions of boxes and goals, and call get-manhattan-distance
;		to get the manhattan distance for each box to get to a goal, in the context
;		of the current state 
(defun h004674598 (s)
	(let ((boxes (getBoxPositions s 0)) (goals (getGoalPositions s 0)))
				(get-manhattan-distance boxes goals)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
