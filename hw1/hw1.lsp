; Chaoran Lin
; CS 161 HW 1

; 1. TREE-CONTAINS
; Parameters:
;   N: a number
;   TREE: An ordered tree in the form of  either a number n or a list (L m R), where
;     - L and R are ordered trees;
;     - m is a number;
;     - all numbers appearing in L are smaller than m;
;     - all numbers appearing in R are larger than m.
;
; Return value:
;   A boolean value indicating whether N exists in TREE (t) or not (nil).
;
; Solution:
;   We first check the base cases:
;     - If TREE is empty, then N cannot exist in TREE, return nil
;     - If TREE is a number, then check if N is that number
;   Otherwise, we compare N to the middle (m) of the TREE:
;     - If TREE only has one element, then recursively call our function on the car of TREE
;     - If N equals the middle element, return nil. 
;     - If not, use the fact that the TREE is sorted, and based on whether N
;       is less than or greater than m, recursively call our function on the left or 
;       right half of the tree.
(defun TREE-CONTAINS(N TREE)
  (cond ((null TREE) 
          nil
        )
        ((numberp TREE) 
          (= N TREE)
        )
        ((null (cdr TREE)) 
          (TREE-CONTAINS N (car tree))
        )
        ((= N (cadr TREE)) 
          t
        )
        ((< N (cadr TREE)) 
          (TREE-CONTAINS N (car TREE))
        )
        (t 
          (TREE-CONTAINS N (cddr TREE))
        )
  )
)

; 2. TREE-MAX
; Parameters:
;   TREE: An ordered tree in the form of either a number n or a list (L m R), where
;     - L and R are ordered trees;
;     - m is a number;
;     - all numbers appearing in L are smaller than m;
;     - all numbers appearing in R are larger than m.
;
; Return value:
;   A number that is the greatest value found in TREE, or nil if the tree is empty.
;
; Solution:
;   We first check the base cases:
;     - If TREE is empty, then simply return nil
;     - If TREE is a number, then that number must be the greatest. Return it.
;   Otherwise, we compare N to the middle (m) of the TREE:
;     - If TREE only has one element, then recursively call our function on the car of TREE
;     - Otherwise, since TREE is ordered, we know that the max must lie somewhere in the rest of
;       the tree, so we recursively call our function on the rest of the tree
(defun TREE-MAX(TREE)
  (cond ((null TREE) 
          nil
        )
        ((numberp TREE) 
          TREE
        )
        ((null (cdr TREE)) 
          (TREE-MAX (car TREE))
        )
        (t
          (TREE-MAX (cdr TREE))
        )
  )
)

; 3. TREE-ORDER
; Parameters:
;   TREE: An ordered tree in the form of either a number n or a list (L m R), where
;     - L and R are ordered trees;
;     - m is a number;
;     - all numbers appearing in L are smaller than m;
;     - all numbers appearing in R are larger than m.
;
; Return value:
;   An in-ordered list of numbers appearing in TREE.
;
; Solution:
;   We first check the base cases:
;     - If TREE is empty, then simply return an empty list
;     - If TREE is a number, then return a list containing only that number
;   Otherwise:
;     - If the first element of the tree is an atom, add that atom to the in-ordered list
;       produced out of the rest of the tree.
;     - If not, we need to parse the first element as it is a list, by calling TREE-ORDER recursively
;       on it, and combining it with the in-ordered list of the rest of the tree.
(defun TREE-ORDER(TREE)
  (cond ((null TREE)
          '()
        )
        ((numberp TREE)
          (cons TREE '())
        )
        ((atom (car TREE))
          (cons (car TREE) (TREE-ORDER (cdr TREE)))
        )
        (t
          (append (TREE-ORDER (car TREE)) (TREE-ORDER (cdr TREE)))
        )
  )
)

; 4. SUB-LIST
; Parameters:
;   L - a list
;   START - a non-negative integer
;   LEN - a non-negative integer
;
; Return value:
;   The sublist of L starting at START and having length LEN. We assume that the
;   first element of L has position 0
;
; Solution:
;   There are a couple of base cases in which a valid sublist does not exist, in 
;   which case we return nil:
;     - L is empty to begin with
;     - START exceeds the length of L (i.e. out of bounds)
;     - LEN exceeds the length of the maximum possible sublist starting from
;       START (i.e. length of L - START)
;     - LEN is 0
;   The general strategy is:
;     - If START is 0, then we know the sublist starts from the beginning of L.
;       Combine the beginning of L and whatever should go in the sublist in the 
;       rest of L. We must make sure to decrement LEN by 1 when we make the recursive
;       call to construct the rest of the sublist.
;     - If not, we search through the rest of the list recursively for the beginning
;       of the sublist. We must make sure to decrement START by 1 to eventually reach
;       0, by which time we get to the correct index of the start of the sublist.
(defun SUB-LIST(L START LEN)
  (cond ((null L)
          nil
        )
        ((> START (- (length L) 1))
          nil
        )
        ((> LEN (- (length L) START))
          nil
        )
        ((= LEN 0)
          nil
        )
        ((= START 0)
          (cons (car L) (SUB-LIST (cdr L) START (- LEN 1)))
        )
        (t
          (SUB-LIST (cdr L) (- START 1) LEN)
        )
  )
)

; 5. SPLIT-LIST
; Parameters:
;   L - a list
;
; Return value:
;   A list containing two lists L1 and L2, such that 
;   - L is the result of appending L1 and L2
;   - Length of L2 minus length of L1 is 0 or 1
;   NOTE: this implies that the length of L2 must be greater than that of L1
;
; Solution:
;   There are a couple of base cases to consider first:
;     - The list is empty, in which case we return a list containing two empty lists
;     - The list only has one number, in which case we return a list containing an
;       empty list and a list containing that one number, in that order (in order to
;       adhere to the requirements)
;   Otherwise, we would split the list based on whether the length of 
;   L is odd or even:
;     - If odd, we split the list into L1 and L2 such that the length of L1 is
;       (length of L rounded down) and the length of L2 is the length of L - length of L1.
;       In other words, this creates two lists such that L1 has one less element than L2.
;     - If even, then we can split the list evenly.
(defun SPLIT-LIST(L)
  (cond ((null L)
          (list '() '())
        )
        ((null (cdr L))
          (list '() (list (car L)))
        )
        ((oddp (length L))
          (list (SUB-LIST L 0 (- (/ (length L) 2) (/ 1 2)))
                (SUB-LIST L (- (/ (length L) 2) (/ 1 2)) (+ (/ (length L) 2) (/ 1 2)))
          )
        )
        (t
          (list (SUB-LIST L 0 (/ (length L) 2))
                (SUB-LIST L (/ (length L) 2) (/ (length L) 2))
          )
        )
  )
)

; 6. BTREE-HEIGHT
; Parameters:
;   TREE: a binary tree such that:
;     - each node has 0 or 2 children.
;     - nodes that 0 children are leaf nodes:
;       - represented by an atom
;     - internal nodes erpresented by a list (L R), where:
;       - where L represents the left child of N and R
;         represents the right child of N.
;
; Return value:
;   A number N representing the height 
;
; Solution:
;   If node is an internal node, the height is 1 (from the parent node to its children)
;   + max(height of subtree from left child, height of subtree from right child).
;   If not, then the node is a leaf node, and its height is 0.
;   Using these two facts, we can traverse down the tree via recursion and return the 
;   height.
(defun BTREE-HEIGHT(TREE)
  (cond ((null TREE)
          0
        )
        ((atom TREE)
          0
        )
        ((listp TREE)
          (cond ((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE)))
                  (+ 1 (BTREE-HEIGHT (first TREE)))
                )
                (t
                  (+ 1 (BTREE-HEIGHT (second TREE)))
                )
          )
        )
        (t
          (+ 0 (BTREE-HEIGHT (cdr TREE)))
        )
  )
)

; 7. LIST2BTREE
; Parameters:
;   LEAVES: a list of atoms
;
; Return value:
;   A binary tree such that:
;   - The tree leaves are the elements of LEAVES;
;   - For any internal (non-leaf) node in the tree, the number of leaves in its right branch minus the
;     number of leaves in its left branch is 0 or 1.
;
; Solution:
;   Base cases: if the length of the list LEAVES is:
;     - 0: return nil
;     - 1: just return the leaf by itself
;     - 2: return LEAVES as is
;   Otherwise:
;     - Use SPLIT-LIST to split LEAVES into two halves, and recursively
;       call LIST2BTREE on them. Combine the results into a list. 
;     - The reason for this is that we want to dissect the tree down to our 
;       base cases, which is either a leaf node (length of LEAVES is 1) or
;       internal node (length of LEAVES is 2).
(defun LIST2BTREE(LEAVES)
  (cond ((null LEAVES)
          nil
        )
        ((= 1 (length LEAVES))
          (car LEAVES)
        )
        ((= 2 (length LEAVES))
          LEAVES
        )
        (t
          (list (LIST2BTREE (first (SPLIT-LIST LEAVES)))
                (LIST2BTREE (second (SPLIT-LIST LEAVES)))
          )
        )
  )
)

; 8. BTREE2LIST
; Parameters:
;   TREE: a binary tree such that:
;     - each node has 0 or 2 children.
;     - nodes that 0 children are leaf nodes:
;       - represented by an atom
;     - internal nodes erpresented by a list (L R), where:
;       - where L represents the left child of N and R
;         represents the right child of N.
;
; Return value:
;   A list of atoms, representing the nodes of the binary tree
;
; Solution:
;   Base cases:
;     - If TREE is an atom already, then simply return the atom in its own list
;     - If TREE is a list containing just 1 node, then simply return the tree as is
;   Otherwise:
;     - Use BTREE2LIST recursively on the first node of the tree, whether it is a 
;       leaf or internal node, and on the other half of the tree. Notice that cdr
;       would contain the other half of the tree in its own list, so this would be unhelpful.
;       Use cadr to strip out the nodes from that list.
(defun BTREE2LIST(TREE)
  (cond ((atom TREE)
          (list TREE)
        )
        ((= 1 (length TREE))
          TREE
        )
        (t
          (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE)))
        )
  )
)

; 9. IS-SAME
; Parameters:
;   E1 and E2, two expressions whose atoms are all numbers
;
; Return value:
;   A boolan value indicating whether E1 and E2 are identical
;
; Solution:
;   A couple of base cases to consider:
;     - E1 and E2 are both null, then return true
;     - One of the two is null but the other one isn't, then return nil
;     - The first element in one of the two is an atom but the other one isn't, then return nil
;   Otherwise:
;     - If the first element in both two lists are atoms (i.e. numbers), then
;       we test whether these first two numbers are the same, plus if the rest of the two lists
;       are the same. Both need to be true in order to return true.
;     - If not, then both of the first elements in the two lists are lists, then we must use
;       our function recursively to determine if those two elements are the same, as well as
;       the rest of the list. Both need to be true in order to return true.
(defun IS-SAME(E1 E2)
  (cond ((and (null E1) (null E2))
          t
        )
        ((and (not (null E1)) (null E2))
          nil
        )
        ((and (not (null E2)) (null E1))
          nil
        )
        ((and (atom (car E1)) (not (atom (car E2))))
          nil
        )
        ((and (atom (car E2)) (not (atom (car E1))))
          nil
        )
        ((and (atom (car E1)) (atom (car E2)))
          (and (= (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))
        )
        (t
          (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))
        )
  )
)
