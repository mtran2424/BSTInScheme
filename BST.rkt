;Description: Program implements a binary search tree using functional
;             programming.
;Assumptions: BST organizes integers
;             BST cannot contain duplicate values
;Author: My Tran

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;Contruct BST Functions:

;construct-empty
;Purpose: Returns an empty list.
;Takes no arguments
(define construct-empty '());return an empty list




;construct-node
;Purpose: Returns tree with given element in tree. Node consists of
;         as follows (value (left-subtree) (right-subtree)). Subtree
;         lists are empty lists if they have no value.
;Takes one argument consisting of an element to be stored in the BST
(define construct-node
  (lambda (element)
    (cons element '(() ()))
    ));make and return list consisting of element value, 


;construct-tree
;Purpose: Returns a tree given left tree, element, and right tree.
;Takes three elements consisting of a root element, left-subtree list
;and right-subtree list
(define construct-tree
  (lambda (element lTree rTree)
    (if (or (list? lTree) (list? rTree)) ;check if the left or right subtree are lists
        (list element lTree rTree);returns list comprised of the element followed by a left subtree
                                  ;list and a right subtree list
        '() ; return empty list if parameters are not valid
        )))



;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;BST Access Functions:

;return-root
;Purpose: Returns the root value of a given node or BST
;Takes one argument consisting of the tree or node being accessed
(define return-root
  (lambda (tree)
    (if (list? tree) ;check if tree given is a list
        (if (null? tree) ;check for empty tree 
            '() ;if empty, return empty
            (car tree));returns the element of the list that is found in first field of a given tree
        '() ;return empty if tree given is not a list
  )))



;return-left
;Purpose: Returns the left-subtree of a given BST or node
;Takes one argument consisting of a tree from which the left-subtree
;is obtained
(define return-left
  (lambda (tree)
    (if (list? tree) ;check if parameter given is a list
        (car (cdr tree));returns the left subtree list that is the second field of the
                        ;given tree. (first field of the CDR of the given tree)
        '() ;return empty list if tree given is not a valid input (valid input is a list)
        )))


;return-right
;Purpose: Returns the right-subtree of a given BST or node
;Takes one argument consisting of a tree from which the right-subtree
;is obtained
(define return-right
  (lambda (tree)
    (if (list? tree)
        (car (cdr (cdr tree)))
        '())
    ));retuns the right subtree list that is the third field of the given
     ;tree. (CDR of the CDR of the given tree)




;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;BST Search:

;search? predicate function
;Purpose: Searches for element a given tree and returns true if found. False otherwise.
;Takes two arguments consisting of the element being searched for and the tree being
;searched in.
(define search?
  (lambda (element tree)
    (if (list? tree) ;check if tree is a list
        (if (empty-tree? tree);check if the tree is empty
            #f                ;return false because empty lists have no elements
            (if (eq? element (return-root tree))  ;check root of tree for target element
                #t            ;return true because target element is found
                ;otherwise traverse tree recursively until element is found or empty list
                ;is found in the position where the element should be
                (if (> element (return-root tree))   ;if the target element is greater than
                                                 ;that of the current tree,
                    (search? element (return-right tree)) ;traverse to the right subtree in recursive call
                
                    (search? element (return-left tree))))) ;(element > root case) traverse to left subtree
                                                       ;in recursive call
        #f);returns false if tree parameter is passed a non list
    ))




;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;Insertion:

;insert
;Purpose: Given an element and a tree, returns the original tree if the element is found
;         in the tree already. Otherwise, returns a tree with the element inserted in
;         its appropriate location. Elements less than a node value are found to the left
;         of that node. Elements greater than a node are found to the right of a node.
;Takes two arguments consisting of an element to be inserted and the tree to be inserted in.
(define insert
  (lambda (element tree)
    (if(list? tree) ;check if tree is a list
       (if (empty-tree? tree) ;check if the given tree is empty
           (construct-node element) ;create and return a new list in the form of a node with given element
           ;else path
           (if (search? element tree) ;search for element in the list
               tree ;if the element is found, return the given tree as is
               (insert-helper element tree) ;otherwise, call insert-helper to build identical tree with
                                         ;element inserted into tree
               ))
    '()))) ;if parameter passed in a non-list, return an empty list





;helper function for insert
;insert-helper
;Purpose: Handles the case where the element was not found in the list and needs to be
;         inserted into the tree. Recursively rebuilds the tree with the element built
;         into the appropriate location.
;Takes two arguments consisting of an element to be inserted and the tree to be inserted in.
(define insert-helper
  (lambda (element tree)
    (if (list? tree) ;check if tree is a list
        (if (empty-tree? tree) ;check if given tree is empty
            (construct-node element) ;exit condition: if empty leaf, create a new node with given element
                                 ;empty list indicates that function has reached location for new element
        ;else path
            (if (> element (return-root tree)) ;greater than root condition
            ;creates and returns tree as list with recursively modified right subtree.
                (construct-tree (return-root tree)
                                (return-left tree)
                                (insert-helper element (return-right tree)))
                ;otherwise, creates and returns tree as list with recursively modified left subtree.
                (construct-tree (return-root tree)
                                (insert-helper element (return-left tree))
                                (return-right tree))))
    '()))) ;return empty list if tree is non-list




;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;Check for Empty:

;empty-tree? predicate
;Purpose: Return true if the given tree is empty. False otherwise.
;Takes one argument representing a tree to be checked for emptiness.
(define empty-tree?
  (lambda (tree)
    (if (list? tree) ;check if tree is a list
        (null? tree)
        #t))) ;check if tree is an empty list. Return #t if empty.
              ;#f otherwise. Returns #t if non-list is passed in.


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;Traversals:

;preorder
;Purpose: Returns a list containing all values in the given tree in the order they
;         were visited in a preorder traversal (N L R)
;Takes one argument consisting of a tree to be traversed
(define preorder
  (lambda (tree)
    (if (list? tree) ;check if tree passed in is a list
        (if (empty-tree? tree) ;check for empty list
            '() ;return empty list if list is empty
            (list (return-root tree)
                  (preorder (return-left tree))
                  (preorder (return-right tree)))) ;otherwise return list containing tree
                                               ;in preorder
        '());return empty list if passed in tree is non-list
    ))


;postorder
;Purpose: Returns a list containing all values in the given tree in the order they
;         were visited in a postorder traversal (L R N)
;Takes one argument consisting of a tree to be traversed
(define postorder
  (lambda (tree)
    (if (list? tree) ;check if tree is a list
    (if (empty-tree? tree) ;check for empty list
        '() ;return empty list if list is empty
        (list (postorder (return-left tree))
              (postorder (return-right tree))
              (return-root tree)));otherwise return list containing tree
                                  ;in postorder
    '()) ;return empty list if tree is non-list
    ))


;inorder
;Purpose: Returns a list containing all values in the given tree in the order they
;         were visited in an inorder traversal (L N R)
;Takes one argument consisting of a tree to be traversed
(define inorder
  (lambda (tree)
    (if (list? tree) ;check if tree is a list
        (if (empty-tree? tree) ;check for empty list
            '() ;return empty list if list is empty
            (list (inorder (return-left tree))
              (return-root tree)
              (inorder (return-right tree))));otherwise return list containing tree
                                               ;in inorder
        '()) ;return empty list if tree is non-list
    ))



;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;Test:

;test-BST
;Purpose: Test all the functions the BST program
;Take a list of integers and a tree as a parameter
(define test-BST
  (lambda (list-elements)
    (if (list? list-elements) ;check if the parameter passed in is a list
        (if (null? list-elements) ;check for empty list
            construct-empty ;return empty list if empty
            ;else path: test all functions required by program
            ;create and return list comprised of function outputs
            (list "Construct BST Functions:"
                  construct-empty ;returns emtpy list
                  (construct-node (car list-elements)) ;returns leaf node
                  (construct-tree (car list-elements) ;returns full tree from list-elements
                                  (return-left (test-helper list-elements construct-empty)) ;ltree of whole
                                  (return-right (test-helper list-elements construct-empty))); rtree of whole
                  ;Test return-root
                  "Access BST Functions:"
                  (return-root (return-left (test-helper list-elements construct-empty))) ;get root of list-elements tree
                  (return-root (test-helper list-elements construct-empty));get root of list-elements left subtree
                  (return-root (return-right (test-helper list-elements construct-empty))) ;get root of list-elements right subtree

                  ;Test search?
                  "Search BST Functions:"
                  (search? (car list-elements)
                           (test-helper list-elements construct-empty)) ;search for existing case
                  (search? (/ 2 (car list-elements))
                           (test-helper list-elements construct-empty)) ;search for non existing < case
                  (search? (* 20 (car list-elements))
                           (test-helper list-elements construct-empty)) ;search for non existing > case
                  (search? (car list-elements) construct-empty) ;search empty tree case
                  (search? (car list-elements) "ughh") ;search in non-list case

                  ;Test insert
                  "BST Insertion Function:"
                  (insert 12 '()) ;insert into empty list case
                  (insert 15 (insert 12 '())) ;insert greater than root into previous
                  (insert 6 (insert 15 (insert 12 construct-empty)));insert less than root into previous
                  (insert 15 (insert 15 (insert 12 construct-empty))) ;insert greater than root duplicate
                  (insert 6 (insert 6 (insert 12 construct-empty))) ;insert less than root duplicate
                  (insert 12 (insert 12 construct-empty)) ;insert root duplicate
                  (insert (car list-elements) list-elements) ;insert root into improper tree
                  (insert (car (cdr list-elements)) list-elements) ;insert non-root into improper tree

                  "Check Empty Tree Function:"
                  (empty-tree? construct-empty) ;check empty tree
                  (empty-tree? '()) ;check explicit empty tree
                  (empty-tree? (return-left (construct-node 6))) ;check empty tree in node
                  (empty-tree? (return-right (construct-node 6)));check empty tree in node
                  (empty-tree? 1) ;check non-list
                  (empty-tree? (construct-node (car list-elements))) ;check leaf node
                  (empty-tree? (test-helper list-elements construct-empty)) ;check with full tree
                  
                  "Traversals:"
                  "Preorder - "
                  (preorder '()) ;traverse empty list case
                  (preorder construct-empty) ;traverse empty list case
                  (preorder "booo") ;traverse non-list
                  (preorder list-elements) ;traverse improper tree
                  (preorder (test-helper ;traverse full tree from given list
                             list-elements
                             construct-empty))
                  (preorder (return-left (test-helper ;traverse left subtree from given list
                                          list-elements
                                          construct-empty)))
                  (preorder (return-right (test-helper ;traverse right subtree from given list
                                           list-elements
                                           construct-empty)))
                  (preorder (construct-node (car list-elements)));traverse leaf node
                  "Postorder - "
                  (postorder '()) ;traverse empty list case
                  (postorder construct-empty);traverse empty list case
                  (postorder "booo") ;traverse non-list
                  (postorder list-elements) ;traverse improper tree
                  (postorder (test-helper ;traverse full tree from given list
                              list-elements
                              construct-empty))
                  (postorder (return-left (test-helper ;traverse left subtree from given list
                                           list-elements
                                           construct-empty)))
                  (postorder (return-right (test-helper ;traverse right subtree from given list
                                            list-elements
                                            construct-empty)))
                  (postorder (construct-node (car list-elements)));traverse leaf node
                  "Inorder - "
                  (inorder '()) ;traverse empty list case
                  (inorder construct-empty) ;traverse empty list case
                  (inorder "booo") ;traverse non-list
                  (inorder list-elements) ;traverse improper tree
                  (inorder (test-helper ;traverse full tree from given list
                            list-elements
                            construct-empty))
                  (inorder (return-left (test-helper ;traverse left subtree from given list
                                         list-elements
                                         construct-empty)))
                  (inorder (return-right (test-helper ;traverse right subtree from given list
                                          list-elements
                                          construct-empty)))
                  (inorder (construct-node (car list-elements))) ;traverse leaf node
                  
                  (test-helper list-elements construct-empty)))
        construct-empty)))

;test-helper
;Purpose: Creates a tree from elements in a given list in the order they are
;         given from left to right
;Take a list of integers and a tree as a parameter
(define test-helper
  (lambda (list-elements tree)
    (if (null? list-elements) ;exit condition: once empty list is encountered,
        tree ;tree should be returned
        (test-helper (cdr list-elements) (insert (car list-elements) tree))
        ;recursive call to build up tree by inserting elements and returning trees
        )))


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;Test Cases
"Function 1: construct-empty"
"Test case 1:"
(define tree1 construct-empty)
tree1

"Test case 2:"
construct-empty

;"Function 1: construct-empty"
;"Test case 1:"
;()
;"Test case 2:"
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 2: construct-node"
"Test case 1:"
(define tree2 (construct-node 50))
tree2

"Test case 2:"
(construct-node 21)


;"Function 2: construct-node"
;"Test case 1:"
;(50 () ())
;"Test case 2:"
;(21 () ())
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 3: construct-tree"
"Test case 1:"
(define tree3 (construct-tree 55 tree2 tree1))
tree3

"Test case 2:"
(construct-tree 49 (construct-node 25) tree3)


;"Function 3: construct-tree"
;"Test case 1:"
;(55 (50 () ()) ())
;"Test case 2:"
;(49 (25 () ()) (55 (50 () ()) ()))
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 4: return-root"
"Test case 1:"
(define root3 (return-root tree3))
root3

"Test case 2:"
(return-root tree1)
(return-root 1)


;"Function 4: return-root"
;"Test case 1:"
;55
;"Test case 2:"
;()
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

""
"Function 5: return-left"
"Test case 1:"
(define ltree (return-left tree3))
tree3
ltree

"Test case 2:"
ltree
(return-left ltree)
(return-left "Sasd")

;"Function 5: return-left"
;"Test case 1:"
;(55 (50 () ()) ())
;(50 () ())
;"Test case 2:"
;(50 () ())
;()
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 6: return-right"
"Test case 1:"
(define rtree (return-right tree3))
tree3
rtree

"Test case 2:"
tree2
(return-right tree2)
(return-right #\g)

;"Function 6: return-right"
;"Test case 1:"
;(55 (50 () ()) ())
;()
;"Test case 2:"
;(50 () ())
;()
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 7: search?"
"Test case 1:"
tree3
(search? 55 tree3)
(search? 12 tree3)
"Test case 2:"
(search? 10 construct-empty)
(search? 1 1212)


;"Function 7: search?"
;"Test case 1:"
;(55 (50 () ()) ())
;#t
;#f
;"Test case 2:"
;#f
;#f
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 8: insert"
"Test case 1:"
(define tree3 (insert 70 (insert 90 (insert 20 (insert 40 (insert 30 (insert 80 (insert 50 tree1))))))))
tree3

"Test case 2:"
(define tree4 tree3)
(define tree4 (insert 70 (insert 90 (insert 20 (insert 40 (insert 30 (insert 80 (insert 50 tree4))))))))
tree4
(insert 19 '(9 (4 (7 () ()) ()) 12))


;"Function 8: insert"
;"Test case 1:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;"Test case 2:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;(9 (4 (7 () ()) ()) ())
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 9: empty-tree?"
"Test case 1:"
tree3
(empty-tree? tree3)

"Test case 2:"
tree1
(empty-tree? tree1)
(empty-tree? construct-empty)
(empty-tree? "weeee")


;"Function 9: empty-tree?"
;"Test case 1:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;#f
;"Test case 2:"
;()
;#t
;#t
;#t
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 10: preorder"
"Test case 1:"
tree3
(preorder tree3)

"Test case 2:"
tree1
(preorder tree1)
(preorder '(13 (6 () 9) (19 17 ())))
(preorder 11)


;"Function 10: preorder"
;"Test case 1:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;"Test case 2:"
;()
;()
;(13 (6 () ()) (19 () ()))
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 11: postorder"
"Test case 1:"
tree3
(postorder tree3)

"Test case 2:"
tree1
(postorder tree1)
(postorder '(13 (6 () 9) (19 17 ())))
(postorder 11)


;"Function 11: postorder"
;"Test case 1:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;(((() () 20) (() () 40) 30) ((() () 70) (() () 90) 80) 50)
;"Test case 2:"
;()
;()
;((() () 6) (() () 19) 13)
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 12: inorder"
"Test case 1:"
tree3
(inorder tree3)

"Test case 2:"
tree1
(inorder tree1)
(inorder '(13 (6 () 9) (19 17 ())))
(inorder 11)


;"Function 12: inorder"
;"Test case 1:"
;(50 (30 (20 () ()) (40 () ())) (80 (70 () ()) (90 () ())))
;(((() 20 ()) 30 (() 40 ())) 50 ((() 70 ()) 80 (() 90 ())))
;"Test case 2:"
;()
;()
;((() 6 ()) 13 (() 19 ()))
;()
;""
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
""
"Function 13: Test"
"Test case 1:"
(define list1 '(32 37 48 55 24 12 29 27 30 6 14))
(test-BST list1)
"Test case 2:"
(test-BST '())
(test-BST #f)


;"Test case 1:"
;("Construct BST Functions:"
; ()
; (32 () ())
; (32 (24 (12 (6 () ()) (14 () ())) (29 (27 () ()) (30 () ()))) (37 () (48 () (55 () ()))))
; "Access BST Functions:"
; 24
; 32
; 37
; "Search BST Functions:"
; #t
; #f
; #f
; #f
; #f
; "BST Insertion Function:"
; (12 () ())
; (12 () (15 () ()))
; (12 (6 () ()) (15 () ()))
; (12 () (15 () ()))
; (12 (6 () ()) ())
; (12 () ())
; (32 37 48 55 24 12 29 27 30 6 14)
; (32 37 ())
; "Check Empty Tree Function:"
; #t
; #t
; #t
; #t
; #t
; #f
; #f
; "Traversals:"
; "Preorder - "
; ()
; ()
; ()
; (32 () ())
; (32 (24 (12 (6 () ()) (14 () ())) (29 (27 () ()) (30 () ()))) (37 () (48 () (55 () ()))))
; (24 (12 (6 () ()) (14 () ())) (29 (27 () ()) (30 () ())))
; (37 () (48 () (55 () ())))
; (32 () ())
; "Postorder - "
; ()
; ()
; ()
; (() () 32)
; ((((() () 6) (() () 14) 12) ((() () 27) (() () 30) 29) 24) (() (() (() () 55) 48) 37) 32)
; (((() () 6) (() () 14) 12) ((() () 27) (() () 30) 29) 24)
; (() (() (() () 55) 48) 37)
; (() () 32)
; "Inorder - "
; ()
; ()
; ()
; (() 32 ())
; ((((() 6 ()) 12 (() 14 ())) 24 ((() 27 ()) 29 (() 30 ()))) 32 (() 37 (() 48 (() 55 ()))))
; (((() 6 ()) 12 (() 14 ())) 24 ((() 27 ()) 29 (() 30 ())))
; (() 37 (() 48 (() 55 ())))
; (() 32 ())
; (32 (24 (12 (6 () ()) (14 () ())) (29 (27 () ()) (30 () ()))) (37 () (48 () (55 () ())))))
;"Test case 2:"
;()
;()