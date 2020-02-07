; Soluciones usando iteraciones

; 1) Entrada: Elemento, Indice, Lista -> Salida: Booleano indicando si el 'Elemento' esta en el 'Indice' de la 'Lista' 
;   NOTA: 0 <= Indice < length(Lista)    
    (defun ElemInPos(elem index list)
        (dotimes ( i index (= elem (nth i list)) ))
    )
    (print (ElemInPos 8 4 '(1 3 5 7 8 3)))

; 2) Entrada: Elemento, Lista -> Salida: Inicion-ena empezando desde la primera ocurrencia de 'Elemento'
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

; 3) Entrada: Elemento, Lista -> Salida: Sublista terminando en la ultima ocurrencia de 'Elemento'
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

; 4) Entrada: Lista -> Salida: Lista con la posicion del primer número impar seguido con su indice (en caso de haber).
;   NOTA: La lista puede contener cualquier tipo de dato, el segundo miembro de la respuesta va de 0 a length(Lista)
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

; 5) Entrada: Lista -> Salida: Lista con el ultimo número real >= 0 seguido del número de ocurrencias en la Lista
;   NOTA: La lista puede contener cualquier tipo de dato
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

; 5) Entrada: Lista -> Salida: Lista con el número de elementos NÚMERICOS seguido del número de sublistas
;    NOTA: Solo contará elementos númericos por especificación del ejercicio
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

; 6) Entrada: Lista con posibles diferentes niveles de anidacion -> Salida: Lista con un solo nivel de anidación en el orden de aparición
    (defun Aplana(list)
        (let 
            ((plainList '()))
            (dolist (deep0 list)
                
            )
        )
    )

; 8) Entrada: Lista con cualquier tipo de elementos de NxN -> Salida: La diagonal principal de la Lista en una lista
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

; 10) Entrada: Lista con cualquier tipo de elementos -> Salida: Suma de solo los elementos numéricos

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

; 12) Entrada: Lista, num -> Salida: Lista sin los elementos que son multiplos de num en la lista original
; NOTA: Se asume que los elementos de la lista de entrada son númericos por el ejercicio
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

; 15) Entrada: 2 matrices de cualquier dimension -> Salida: El resultado de la multiplicacion en caso de que se pueda realizar
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
                            ;; (print (list (nth k (nth i m1)) (nth j (nth k m2))))
                            ;; (print sum)
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
