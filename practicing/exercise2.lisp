; Iterative solutions

; 1) Input: Element, Index, List -> Output: Bool according if the 'Elem' is in List[Index]
;   Note: 0 <= Index < length(List)    
    (defun ElemInPos(elem index list)
        (dotimes ( i index (= elem (nth i list)) ))
    )
    (print (ElemInPos 8 4 '(1 3 5 7 8 3)))

; 2) Input: Element, List -> Output: Sublist from the first appearence of Elemen to the end of List
    (defun Inicion-en(elem list)
        (let 
            ((newList '()))
            (dolist (i list)
                (cond 
                    ( (equal i elem)    (push i newList) )
                    ( (consp newList)   (push i newList) )
                )
            )
            (reverse newList)
        )
    )
    (print (Inicion-en 'a '(112 2 3 4 5 654 1 er a 4 7 8 9)))

; 3) Input: Element, List -> Output: Sublist from the first elem in given list to the last appearence of 'Element'
    (defun Termina-en(elem list)
        (let 
            ((newList '()))
            (dolist (i (reverse list))
                (cond 
                    ( (equal i elem)    (push i newList))
                    ( (consp newList)   (push i newList))
                )
            )
            newList
        )
    )
    (print (Termina-en 'a '(112 2 3 4 5 654 1 er a 4 7 8 9)))

; 4) Input: List -> Output: List wich the first element is the first odd number in the given list and the second one is its index 
;   Note: The elements in the list can be any type of data.
    (defun Primer-impar(list)
        (let
            ((tuple '()))
            (dolist (elem (reverse list))
                (if (numberp elem)
                    (if (= 1 (mod elem 2))
                        (setq tuple (list elem (position elem list :test #'equal)) )
                    )
                )
                ;; (print elem)
            )
            tuple
        )
    )
    (print (Primer-impar '(4 e ds g 6 30/2 2 1 51 6)))

; 5) Input: List -> Output: List wich the first element is the last real number >= 0 and the second one is the times it appears in the list given
;   Note: The elements in the list can be any type of data.
    (defun Ultimo-real(list)
        (let
            (
                (numReal -1)
                (times 0)
                (found nil)
            )
            (dolist (elem (reverse list))
                (if (and (realp elem) (>= elem 0))
                    (cond 
                        ( (null found)    (setq numReal elem) (setq times (1+ times)) (setq found t) (print elem))
                        ( (equal numReal elem) (setq times (1+ times)) )
                    )
                )
            )
            (list numReal times) 
        )
    )
    (print (Ultimo-real '(24/4 4 12/2 12/2 qw e 6 ds g 6 30/2 2 1 51 6 -1 as)))

; 6) Input: List -> Output: List wich first element is the number of numeric elems in the given list and the second one is the number of sublist in the given list
;    Note: It just counts the numeric type according the exercise
    (defun Conteo(list)
        (let
            (
                (subList 0)
                (numbers 0)
            )
            (dolist (elem list)
                (cond
                    ( (numberp elem) (setq numbers (1+ numbers)) )
                    ( (listp elem) (setq subList (1+ subList)) )
                )
            )
            (list numbers subList)
        )
    )
    (print (Conteo '(1 45 6 2 3 (a b s) (ba 3 2) a 3)))

; 7) Input: List with any number of deeper sublists -> Output: A simple list wich contains the elements of all the emelents in the given list in the order of appearence
    (defun Aplana(list)
        (let 
            ((plainList '()))
            (dolist (deep0 list)
                
            )
        )
    )

; 8) Input: List wich contains N sublists of N elements (matrix) -> Output: List wich contains the principal diagonal matrix
    (defun diagonal(L) 
        (let 
            ( (diag '()) )
            (dotimes (i (length L))
                (push (nth i (nth i L)) diag)
            )
            (reverse diag)
        )
    )
    (print (diagonal '((1 2 3) (4 5 6) (7 8 9)) ))

; 10) Input: List wich elements are any type of data -> Output: The sum of the numeric values in the list
    (defun sumaNum(L)
        (let 
            ( (sum 0) )
            (dolist (i L)
                (if (numberp i)
                    (setq sum (+ sum i))
                )
            )
            sum
        )
    )
    (print (sumaNum '(1 a 4 5 8/2)))    

; 12) Input: List, num -> Output: List wich contains only the elements not multiple of num in the given list
; Note: It is assumed that the elements are only numeric type according to the exercise
    (defun filtraMult(L num)
        (let
            ( (newList '()) )
            (dolist (elem L)
                (when (numberp elem)
                    (if (/= (mod elem num) 0)
                        (push elem newList)
                    )
                )
            )
            (reverse newList)
        )
    )

    (print (filtraMult '(1 45 23 66 100 2 6) 2))

; 15) Input: Two matrix of any dimension -> Output: The result of they multiplication if it can be calculate
    (defun multMatix(m1 m2)
        (let
            ( 
                (matrixRes '())
                (rowRes '())
                (sum 0) 
                (rowm1 (length m1))
                (colm1 (length (nth 1 m1)))
                (rowm2 (length m2))
                (colm2 (length (nth 1 m2)))
            )
            (if (= colm1 rowm2)
                (dotimes (i rowm1)
                    (dotimes (j colm2)
                        (dotimes (k rowm2)
                            (setq sum (+ sum (* (nth k (nth i m1)) (nth j (nth k m2))) ))
                        )
                        (push sum rowRes)
                        (setq sum 0)
                    )
                    (push (reverse rowRes) matrixRes)
                    (setq rowRes nil)                
                )
            )
            (reverse matrixRes)
        )
    )
    (print (multMatix '((2 4 1) (2 3 9) (3 1 8)) '((1 2 3) (3 6 1) (2 4 7))))
    ;   (2 4 1)     (1 2 3)
    ;   (2 3 9)     (3 6 1)
    ;   (3 1 8)     (2 4 7)