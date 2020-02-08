; 1.- Escribir una sola expresion para los siguientes incisos:
    ; a) El quinto elemento de la lista
        (print (car (cddddr '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))) ))
    
    ; b) Número de segundos que tiene el año bisiesto 2004.
        (print (* 60 60 24 366))
    
    ; c) Si el valor numérico asociado a la variable x es diferente de cero y además
    ; menor o igual que el valor asociado a la variable y
        (defun compare(x y)
            (and (/= x 0) (<= x y))
        )
        (print (compare 4 12))

    ; d) Una lista con las dos soluciones reales de la ecuación  2x^2 + 7x + 5 = 0.
    ; x1 = 0.6084952830141508, x2 = -4.10849528301415
        (print (list
            (+ (- (/ 7 (* 2 2))) (/ (sqrt (- (* 7 7) (* 4 2 5))) (* 2 2)) )
            (- (- (/ 7 (* 2 2))) (/ (sqrt (- (* 7 7) (* 4 2 5))) (* 2 2)) )
        ))

; 2.- Escriba, en notación prefija y evalúe las siguientes expresiones aritméticas
    ; a) 
    (print (+ (* 2 4) (- 6 8)))
    ; b)
    (print (/ (+ 5 -3 +4) (+ 6 2/5)))
    ; c)
    (print (sqrt (/ (+ (* -1 (+ -4 -3/8)) 1.4502) (expt -1 (expt (- 3 5) 1/3)))))
    ; d)
    (print (expt (/ (expt (/ 65.402 (sqrt -1)) 1/5) 0.17) 1/7))

; 3.-
    ; a) (two) como lista, car = first, cdr = rest
    (print (cdar '((one two) three four)))
    ; b) ((EVA LISA) KARL SVEN EVA LISA KARL SVEN) solo crea una celda para la primera sublista y entonces une las tras con el metodo append
    (print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))
    ; c) Crea una nueva lista sustituyendo el segundo argumento por el el primero en la lista original -> (EVA GITAN LISA GITAN KARIN)
    (print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))
    ; d) Borra el primer argumento en la lista dada -> (EVA LISA ANNA)
    (print (remove 'sven '(eva sven lisa sven anna)))
    ; e) Borra los ultimos n elementos en la lista dada -> (KARL ADAM NILSSON)
    (print (butlast '(karl adam nilsson gregg alisson vilma) 3))
    ; f) Obtiene el elemento de la lista en el indice dado -> list[n]: C
    (print (nth 2 '(a b c d e)))
    ; g) Combina las funciones cdr y nth -> (C D E)
    (print (nthcdr 2 '(a b c d e)))
    ; h) Maneja  las listas como conjuntos y saca la interseccion entre ellas -> (B C) 
    (print (intersection '(a b c) '(x b z c)))
    ; i) Al igual que en el incisio a) obtiene un elemento de la lista (4) 
    (print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))

; 4.- Entrada: Lista con la estructura  ((A . x) (B . y) (C . z)) -> Salida: Lista con las siguiente estrucura: ( ((x y) . A) ((y z) . C) ((z y x) . B) )
    (defun recombina(L)
        (list 
            (cons (list (cdar L) (cdadr L)) (caar L))
            (cons (list (cdadr L) (cdaddr L)) (caaddr L) )
            (cons (list (cdaddr L) (cdadr L) (cdar L)) (caadr L))
        )
    )
    ;(print (caar (list (cons 'hola 1) (cons 'B 2) (cons 'C 3))))
    (print (recombina (list (cons 'dificil 1) (cons 'listas 2) (cons 'adios 33))))

; 5.-

; 6.- Entrada: Cualquier tipo de dato -> Salida: Lista con valores booleanos especificanto si el elemento es de ese tipo
    (defun analiza(x)
        (list (atom x) (numberp x) (listp x) (consp x) (null x))
    )

; 7.-
    
; 8.- Entrada: Dos lista -> Salida: Booleano que indica si las listas contienen los mismos elementos en las mismas posiciones
    (defun mismotipo(l1 l2)
        (let (
                (same t) ;vars
                (len (length l1))
            )
            (
                loop for i from 0 to (1- (length l1)) do
                    (setq same (and (equalp (nth i l2) (nth i l2)) same))
                    (print same)
                
            )   
        )
    (print (mismotipo '(ab 11 4 .1) '(ab 32 4 .5)) )

; 9.- Entrada: String -> Salida: Un string que contiene el palíndromo del string de entrada
    (defun palindromo(str)
        (let (
            (strlist (coerce str 'list))
            (pal '())
        )
            (dolist (i strlist pal)
                (setq pal (append 'i pal i))
            )
        )
    )
    ;(print (palindromo "Como")))

    ;; (print (coerce (coerce "hola" 'list) 'string))

; 10.- Entrada: Un número -> Salida: Booleano si es que el número representa un año bisiesto

    (defun añoBisiesto(año)
        (cond 
            ( (= (mod año 100) 0)   (= (mod año 400) 0))
            (t                      (= (mod año 4) 0))
        )
    )

    (print (añoBisiesto 2011))