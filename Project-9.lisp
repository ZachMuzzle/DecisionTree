
;Copyright 1998 Tom M. Mitchell.  This code may be freely distributed
;and used for any non-commericial purpose, as long as this copyright
;notice is retained.  The author assumes absolutely no responsibility
;for any harm caused by bugs in the code.

(setf *print-length* nil)
(setf *print-level* nil)

;;; General utility functions
;; use (print.entity instance) to see the definition of entity 

(defun put.value (attribute instance val)
  "assign a value to an attribute of an instance"
  (setf (get instance attribute) val))

(defun get.value (attribute instance)
  "retrieve the value of attribute of instance"
  (get instance attribute))

(defun print.entity (instance)
  "print the description of instance to the screen"
  (print (symbol-plist instance)))


(defun data-setup (datafile)
(setq *examples* nil)
(setq our-input-stream (open datafile :direction :input))
(setq *attributes* (read our-input-stream))
(setq *att-class* (car (read our-input-stream)))
(setq *att-values* (read our-input-stream))
   
(loop for d in (read our-input-stream) do
      (setf *examples* (cons (first d) *examples*))
      (loop for attribute in (append *attributes* (list *att-class*))
	     as value in (cdr d)
	     do (put.value attribute (first d) value)))
(put.value 'legal.values *att-class* *att-values*)
(setq *examples* (reverse *examples*))
(close our-input-stream))


;;; Top level ID3 Decision Tree learning algorithm
;
; Tree Representation: each non-terminal tree node is a list of the form 
;  (attribute (value1 subtree1)(value2 subtree2)...)
;  where subtree-n is either a non-terminal node, or a value signifying the 
;  target value associated with that terminal node

(defun id3 (examples target.attribute attributes)
;
  "TARGET.ATTRIBUTE is the attribute to be predicted by the learned tree.
   EXAMPLES are training examples.  ATTRIBUTES is a list of attributes (not
   including TARGET.ATTRIBUTE) that may be included in the learned decision 
   tree.
   Returns: a decision tree that predicts the TARGET.ATTRIBUTE over EXAMPLES"
  (let (firstvalue a partitions)
    (setq firstvalue (get.value target.attribute (first examples)))
;    (break "in id3")
    (cond 
     ;; if every example has the same target attribute value, return it as
     ;; a leaf node
     ((every #'(lambda(e)(eq firstvalue (get.value target.attribute e)))
             examples)*att-values*
      firstvalue)
     ;; if no attributes, return the most common target attribute value
     ((null attributes)
      (most.common.value target.attribute examples))
     ;; otherwise, pick the best attribute, partition training data, and call
     ;; ID3 recursively to grow subtrees below this node
     (t
      (setq partitions
            (loop for a in attributes collect (partition a examples)))
      (setq a (choose.best.partition target.attribute partitions))
      (cons (first a)
            (loop for branch in (cdr a) collect
                  (list (first branch)
                        (id3 (cdr branch) 
                             target.attribute 
                             (remove (first a) attributes)))))))))

                  
(defun partition (attribute instances)
  "returns a partion of INSTANCES according to their values for ATTRIBUTE. 
   Returns a list (attribute (value1 e11 e12 ...)(value2 e21 e22 ...)...)"
  (let (result vlist v)
    (loop for e in instances do
          (setq v (get.value attribute e))
          (if (setq vlist (assoc v result))
            (rplacd vlist (cons e (cdr vlist)))
            (setq result (cons (list v e) result))))
    (cons attribute result)))

(defun choose.best.partition (target.attribute partitions)
  "return the partition with the highest information gain.  
   PARTITIONS is of the form ((attribute1 (val1 e11 e12 ...)(val2 e21 e22 ...)...)
                              (attribute2 (...  ...........)(...  ...  )...)).
   Note for efficiency, we compute only the expected value of the entropy of the
   partition, because this is the only term in information gain that varies from
   one attribute to another"
  (let ((lowest.exp.entropy 9999) exp.entropy best.partition)
    (loop for att.partition in partitions do
          (when (< (setq exp.entropy 
                         (expected.entropy target.attribute (cdr att.partition)))
                   lowest.exp.entropy)
            (setq lowest.exp.entropy exp.entropy)
            (setq best.partition att.partition)))
    best.partition))

(defun expected.entropy (att partition)
  "returns the sum over possible values of att of the quantity
    number.of.instances.with.this.value x sample.entropy.of.this.partition"
  (loop for p in partition sum
        (* (length (cdr p))
           (loop for v in (get.value 'legal.values att) sum
                 (let ((vcount (loop for e in (cdr p) count 
                                     (eq v (get.value att e))))
                       proportion)
                   (setq proportion (/ vcount (length (cdr p))))
;;                   (format t "p: ~S, vcount: ~d, proportion: ~S~%"
;;                           p vcount proportion)
                   (* -1.0 proportion 
                      (if (zerop proportion) 0 (log proportion 2))))))))

(defun most.common.value (attribute instances)
;  (break "in most-common-value")
  (let ((length 0) longest)
    (loop for p in (cdr (partition attribute instances)) do
          (when (> (length p) length)
            (setq length (length p))
            (setq longest p)))
    (car longest)))

(defun entropy (p)
  (+ (* -1.0 p (log p 2))
     (* -1.0 (- 1 p) (log (- 1 p) 2))))
                   

(defun print.tree (tree &optional (depth 0))
  (tab depth)
  (format t "~A~%" (first tree))
  (loop for subtree in (cdr tree) do
        (tab (+ depth 1))
        (format t "= ~A" (first subtree))
        (if (atom (second subtree))
          (format t " => ~A~%" (second subtree))
          (progn (terpri)(print.tree (second subtree) (+ depth 5))))))
(defun tab (n)
  (loop for i from 1 to n do (format t " ")))

(defun compute-subset (i j instances) ; TRY TO USE SOMETHING LIKE: (+ 1 (position (car *examples*) *examples*)) returns 1 instead of 0
; i and j are integers
; instances is a list of data instances
; returns a list of the i-th thru j-th elements of instances
; YOU MUST WRITE THIS FUNCTION
(cond
  ((null instances) nil)
  ((< (+ 1 (position (car instances) *examples*)) i) (compute-subset i j (cdr instances))) ; Use position of car instances in *examples* find position cdr list if less than i and recurse
  ((equal i (+ 1 (position (car instances) *examples*))) (cons (car instances) (compute-subset i j (cdr instances)))) ; if i is equal to posistion than cons car on list and recurse
  ((and (> (+ 1 (position (car instances) *examples*)) i) (<= (+ 1 (position (car instances) *examples*)) j) (cons (car instances) (compute-subset i j (cdr instances))))) ; if between i and j cons car instances on list and recurse 
  (t (compute-subset i j (cdr instances))) ; otherwise, cdr list and recurse.
  )
)
"""
(let ((x 0))
(cond
  ((null instances) nil)
  ((< (count2 x) i) (compute-subset i j (cdr instances))) ; if count is less than i then cdr recurse list.
  ((equal i (count2 x)) (cons (car instances) (compute-subet i j (cdr instances)))) ; if i is equalt to count then cons the first element of instances a recurse the cdr instances
  ((and (> (count2 x) i) (<= (count2 x) j) (cons (car instances) (compute-subset i j (cdr instances)))))
  (t (compute-subset i j (cdr instances))) 
  )
)

(defun count2 (x) ;count for our list for each element.
(+ x 1)) ; returns a number
"""

(defun classify (instance tree) ;instance is our data-set while tree will be our produced tree.
; GOAL: The goal of this function is to take in our data and return what the classifcaion of it is. So for *examples*=aardvark would be 1 while *examples*=catfish would be 4.
; instance is a data instance from *examples*
; tree is a decision tree produced by id3
; Return the class value predicted by the decision tree for instance
; YOU MUST WRITE THIS FUNCTION
(cond 
  ((atom tree) (return-from classify tree)) ; if tree is an atom then return the tree.
  ((equal (get.value (first tree) instance) (first (assoc (get.value (first tree) instance) (cdr tree)))) (classify instance (second (assoc (get.value (first tree) instance) (cdr tree))))) ; checks if if value of first is equal to value of second. If so recurse classify.
  ; The second value will be the assoc of the get.value through our tree. Once found we only use the the first value of the list. If they are equal recurse classify with the second assoc of that same instance.
  (t 0)

  )

  )
; testcases = test data. tree = train data
(defun testing (testcases tree) 
; testcases is a list of data instances to be used as testcases
; tree is a decision tree  
; returns the number of instances in testcases that are classified correctly
; YOU MUST WRITE THIS FUNCTION 
; testcases will be all of *examples* and tree will just be from id3. We will used classify from above to test. We will need to use type? to test if it is
;the right classified item. 

;E.G If (get (nth 2 testcases) 'TYPE?) is equal to (classify (nth 0 testcases) tree) then increment 1. Otherwise, we can print that it is not equal.
; We will have to replace nth with car. Then cdr through the list.

(cond 
((null testcases) 0) ; use 0 cause we are adding.

((= (get (car testcases) 'TYPE?) (classify (car testcases) tree)) (+ 1 (testing (cdr testcases) tree))) ; if TYPE? is equal to the value from classify. Add 1.

(t (testing (cdr testcases) tree)) ;Otherwise just recurse.

)
)

(defun cross-validate (n examples size)
; n is an integer. n = 4 and size = 100
; examples is a list of data instances (in our case, it will be *examples*)
; size is the number of instances in examples 
; size must be evenly divisible by n
; returns the percentage error from n-fold cross-validation on the instances in examples
; YOU MUST WRITE THIS FUNCTION
(if (not (divisible-by-n size n))  ; e.g: n-fold = 25
  nil
(let* ((n-fold (/ size n ))

  (first_bin (/ n-fold n-fold))

  (first_last_bin (n-fold))

  (last_first_bin_test (+ 1 (- size n-fold))) ; maybe can work for all first bins. SIZE and N change each passthrough. n-fold is always the same.

  (first_bin ())
  (last_last_bin_test (size)) ;size is changed each time e.g size = 100 then size = 75 etc.

  (train_bin (- size n-fold))




  (test_fold (compute-subet (last_first_bin_test) (last_last_bin_test) examples ))
  (train_fold_subset ())
) 

(cond 
((divisible-by-n size n) (testing ())) ; have size reduced each time by amout of n-fold. e.g n=4 n-fold=25

)
)))

(defun divisible-by-n (size n)
(zerop (mod size n))
)

(defun increment_value (i)


)

(defun test(tree i)
(print (nth (+ i 1) *examples*))
(print (classify (nth (+ i 1) *examples*) tree))
(if (<= i 99)
(test tree (+ i 1))
(princ "End of i")
)
)

"""
(print (classify (nth 2 *examples*) tree))
(print (classify (nth 3 *examples*) tree))
(print (classify (nth 4 *examples*) tree))
(print (classify (nth 5 *examples*) tree))
(print (classify (nth 6 *examples*) tree))
(print (classify (nth 7 *examples*) tree))
(print (classify (nth 8 *examples*) tree))
(print (classify (nth 9 *examples*) tree))
(print (classify (nth 10 *examples*) tree))
(print (classify (nth 11 *examples*) tree))
)
"""
"""
TREE1 OUTPUT
(LEGS (5 7) (8 7) (6 (AQUATIC (1 7) (0 6))) (2 (HAIR (0 2) (1 1)))
 (0 (FINS (1 (EGGS (0 1) (1 4))) (0 (TOOTHED (1 3) (0 7)))))
 (4 (HAIR (0 (AQUATIC (0 3) (1 (TOOTHED (0 7) (1 5))))) (1 1))))

TREE1 OUTPUT MADE PRETTY
 LEGS
 = 5 => 7
 = 8 => 7
 = 6
     AQUATIC
      = 1 => 7
      = 0 => 6
 = 2
     HAIR
      = 0 => 2
      = 1 => 1
 = 0
     FINS
      = 1
          EGGS
           = 0 => 1
           = 1 => 4
      = 0
          TOOTHED
           = 1 => 3
           = 0 => 7
 = 4
     HAIR
      = 0
          AQUATIC
           = 0 => 3
           = 1
               TOOTHED
                = 0 => 7
                = 1 => 5
      = 1 => 1
 """