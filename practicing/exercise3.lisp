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
            )
            (cond
                ( (null L) nil ) ; 
                ( (and (>= (car L) 0 ) (realp (car L))) () )
            )
        )
    )