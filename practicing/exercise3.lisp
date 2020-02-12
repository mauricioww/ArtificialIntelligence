;                =============   Solutions for the exercises in the second package using recursion    ================
; 1) Input: Element, Index, List -> Output: Bool according if the 'Elem' is in List[Index]
;   Note: 0 <= Index < length(List)    
    (defun ElemInPos(elem pos L)
        (cond 
            ( (= pos 0) (equal elem (first L)))
            ( t (ElemInPos elem (1- pos) (rest L)))
        )
    )
    (print (ElemInPos 8 4 '(1 3 5 7 8 3)))

; 2) Input: Element, List -> Output: Sublist from the first appearence of Elemen to the end of List
    (defun Inicio-en(elem L)
        (cond
            ( (null L)          nil)
            ( (eq elem (car L)) (cons elem (Inicio-en (cadr L) (cdr L))) )
            ( (not (null L))    (Inicio-en elem (cdr L)) )
        )
    )
    (print (Inicio-en 'a '(112 2 3 4 5 654 1 er a 4 7 8 9)))

; 3) Input: Element, List -> Output: Sublist from the first elem in given list to the last appearence of 'Element'
;   NOTE: The element must be within the list otherwise the answer will be nil
    (defun  Termina-en(elem L)
        (cond 
            ( (null (intersection (list elem) L)) nil)
            (t (cons (car L) (Termina-en elem (cdr L))) )
        )
    )
    (print (Termina-en 1 '(112 2 3 4 5 654 1 er a ab 4 7 8 9 1 l o 12)))

; 4) Input: List -> Output: List wich the first element is the first odd number in the given list and the second one is its index 
;   NOTE: The elements inside the given list can be any type of data.
    (defun Primer-impar(L)
        (let
            ( (res '()) )
            (cond ( (numberp (car L)) 
                (cond
                    ( (= 1 (mod (car L) 2)) (list (car L) 0) )
                    (t 
                        (setq res (Primer-impar (cdr L))) 
                        (list (nth 0 res) (1+ (nth 1 res)))
                    )
                ) )
                (t 
                    (setq res (Primer-impar (cdr L))) 
                    (list (nth 0 res) (1+ (nth 1 res)))
                ) 
            )
        )
    )
    (print (Primer-impar '(4 e ds g 6 30/2 2 1 51 6)))

; 5) Input: List -> Output: List wich the first element is the last real number >= 0 and the second one is the times it appears in the list given
;   Note: The elements in the list can be any type of data.
    (defun Ultimo-real(L)
        (let 
            ( 
                (res '()) 
                (times 0)
                (next '())
            )
            (cond
                ( (null L) nil ) ; 
                ( (and (>= (car L) 0 ) (realp (car L))) (setq res (list carL 0)) )
                (t (setq next (Ultimo-real (cdr L))) ) ; Write another cond 
                ((equal (car res) (car next)) (list (car res) (1+ (nth 1 res))))
            )
        )
    )

; 8) Input: List wich contains N sublists of N elements (matrix) -> Output: List wich contains the principal diagonal matrix

    (defun diagonal(matrix &optional (i 0) (j 0))
        (cond
            ((null matrix) nil)
            (t  (cons (nth j (nth i matrix)) (diagonal (cdr matrix) i (1+ j)) ))  
        )
    )
    (print (diagonal '((1 2 3) (4 5 6) (7 8 9)) ))


; 10) Input: List wich elements are any type of data -> Output: The sum of the numeric values in the list
    (defun sumaNum(L)
        (cond
            ((null L) 0)
            ((numberp (car L)) (+ (car L) (sumaNum (cdr L))))
            (t (sumaNum (cdr L)))
        )
    )
    (print (sumaNum '(1 a 4 5 8/2)))    

; 12) Input: List, num -> Output: List wich contains only the elements not multiple of num in the given list
; Note: It is assumed that the elements are only numeric type according to the exercise

    (defun filtraMult(L n)
        (cond
            ( (null L) nil)
            ( (numberp (car L))
            (cond
                ((/= (mod (car L) n) 0) (cons (car L) (filtraMult (cdr L) n)))
                (t (filtraMult (cdr L) n))
            ) )
            (t (filtraMult (cdr L) n))
        )
    )
    (print (filtraMult '(1 45 23 66 100 2 6) 2))

; 16) Input: List, elem1, elem2 -> Output: Similar list to the input list but elem2 instead of elem1 in it.

    (defun cambia(L e1 e2)
        (cond
            ((null L) nil)
            ((eq (car L) e1) (cons e2 (cambia (cdr L) e1 e2)))
            (t (cons (car L) (cambia (cdr L) e1 e2)))
        )
    )

    (print (cambia '(1 e e 4 5 e 1) 'e 'a))

; 18) Implement your own mapcar function, it must behave equal to the original.
;    NOTE: The list in the function must have the same length

    (defun myMapcar(func &optional rest) ; At least one list to operate
        (cond
            ((null rest) nil)
            (t          (cons (apply func (mapcar 'car rest))
                        (apply 'myMapcar func (mapcar 'cdr rest)) )
            )
        )
    )

    (defun mapcar* (function &rest args)
            "Apply FUNCTION to successive cars of all ARGS.
          Return the list of results."
            ;; If no list is exhausted,
            (if (not (null args))
                ;; apply function to cars.
                (cons (apply function (mapcar 'car args))
                      (apply 'mapcar* function
                             ;; Recurse for rest of elements.
                             (mapcar 'cdr args)))
                            ))

    ;; (print (mapcar* 'list '(2 3 5) '(1 2 3 4)))

; 20) Input: List, num -> Output: List wich contains only numeric values >= num.    

    (defun elimina(L n)
        (cond
            ((null L) nil)
            ((not (numberp (car L))) (elimina (cdr L) n))
            ((<= (car L) n) (elimina (cdr L) n))
            (t (cons (car L) (elimina (cdr L) n)) )
        )
    )

    (print (elimina '(a b 12 4 2 14 53 0 65.2) 5))

; 21) Input: List1, List2, elem1, elem2 -> Output: List wich contains all elements in List1 following by the elements in List2
;   also in the final list elem2 instead of elem1

    (defun pegaYcambia(l1 l2 e1 e2)
        (labels (
            (cambiaLista(lista e1 e2)
                (cond
                    ( (null lista) nil)
                    ( (equal (car lista) e1) (cons e2 (cambiaLista (cdr lista) e1 e2)) )
                    (t (cons (car lista) (cambiaLista (cdr lista) e1 e2)) )
                )
            )
        )
        (append (cambiaLista l1 e1 e2) (cambiaLista l2 e1 e2)))
        
    )

    (print (pegaYcambia '(10 1 2 3 4 1 n) '(1 5 2 1) 'n 8))

; 22) Input: List which contains any type of data -> Output: List which contains just numeric data and were sort by the quicksort algorithm

    (defun quicksort(L)
    	(labels (
            (filtNum(L) ; Clear the no-numeric values in list
                (cond
                    ((null L) nil) 
                    ((numberp (car L)) (cons (car L) (filtNum (cdr L))))
                    (t (filtNum (cdr L)))
                )
            )
            (operator (e L func)
                (cond
                    ((or (null e) (null L)) nil)
                    ((eval (list func e (car L))) (operator e (cdr L) func))
                    (t (cons (car L) (operator e (cdr L) func)))
                )
            )
            (qsort (L)
                (cond
                    ((null L) nil)
                    (t  (append (qsort (operator (car L) (cdr L) '<))
                                (cons (car L) nil)
                                (qsort (operator (car L) (cdr L) '>=))
                        )
                    )
                )
            )
        ) 
            (qsort (filtNum L))
        )
    )

    (print (quicksort '(124 a sf 4 13 v 2 0 -12 40)))
