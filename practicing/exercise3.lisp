;                   ============= Ejercicios del paquete 2 resueltos con recursión ================
; 1) Entrada: Elemento, Indice, Lista -> Salida: Booleano indicando si el 'Elemento' esta en el 'Indice' de la 'Lista' 
;   NOTA: 0 <= Indice < length(Lista)    
    (defun ElemInPos(elem pos L)
        (cond 
            ( (= pos 0) (equal elem (first L)))
            ( t (ElemInPos elem (1- pos) (rest L)))
        )
    )
    (print (ElemInPos 8 4 '(1 3 5 7 8 3)))

; 2) Entrada: Elemento, Lista -> Salida: Sublista empezando desde la primera ocurrencia de 'Elemento' de la lista original
    (defun Inicio-en(elem L)
        (cond
            ( (null L)          nil)
            ( (eq elem (car L)) (cons elem (Inicio-en (cadr L) (cdr L))) )
            ( (not (null L))    (Inicio-en elem (cdr L)) )
        )
    )
    (print (Inicio-en 'a '(112 2 3 4 5 654 1 er a 4 7 8 9)))

; 3) Entrada: Elemento, Lista -> Salida: Sublista terminando en la ultima ocurrencia de 'Elemento'
;   NOTA: El elemento de busqueda debe de estar en la lista, sino el resultado sera nil
    (defun  Termina-en(elem L)
        (cond 
            ( (null (intersection (list elem) L)) nil)
            (t (cons (car L) (Termina-en elem (cdr L))) )
        )
    )
    (print (Termina-en 1 '(112 2 3 4 5 654 1 er a ab 4 7 8 9 1 l o 12)))

; 4) Entrada: Lista -> Salida: Lista con la posicion del primer número impar seguido con su indice (en caso de haber).
;   NOTA: La lista puede contener cualquier tipo de dato, el segundo miembro de la respuesta va de 0 a length(Lista)
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

; 5) Entrada: Lista -> Salida: Lista con el ultimo número real >= 0 seguido del número de ocurrencias en la Lista
;   NOTA: La lista puede contener cualquier tipo de dato
    (defun Ultimo-real(L)
        (let 
            ( 
                (res '()) 
                (times 0)
            )
            (cond
                ( (null L) nil )
                ( (cond ()

                ) () )
            )
        )
    )