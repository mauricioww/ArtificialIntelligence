; 1.- Write one expression for the following sentences
    ; a) The fifth of the list
        (print (car (cddddr '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))) ))
    
    ; b) Number of seconds in the leap year 2004.
        (print (* 60 60 24 366))
    
    ; c) If the associated value to x is diffetent to 0 and minor or equal to the 
    ; associated value to y
        (defun compare(x y)
            (and (/= x 0) (<= x y))
        )
        (print (compare 4 12))

    ; d) A list which elements are the answers for the following equation 2x^2 + 7x + 5 = 0.
    ; x1 = 0.6084952830141508, x2 = -4.10849528301415
        (print (list
            (+ (- (/ 7 (* 2 2))) (/ (sqrt (- (* 7 7) (* 4 2 5))) (* 2 2)) )
            (- (- (/ 7 (* 2 2))) (/ (sqrt (- (* 7 7) (* 4 2 5))) (* 2 2)) )
        ))

; 2.- Write in prefix notation and execute the following math expressions
    ; a) 
    (print (+ (* 2 4) (- 6 8)))
    ; b)
    (print (/ (+ 5 -3 +4) (+ 6 2/5)))
    ; c)
    (print (sqrt (/ (+ (* -1 (+ -4 -3/8)) 1.4502) (expt -1 (expt (- 3 5) 1/3)))))
    ; d)
    (print (expt (/ (expt (/ 65.402 (sqrt -1)) 1/5) 0.17) 1/7))

; 3.-
    ; a) (two) as list, car = first, cdr = rest
    (print (cdar '((one two) three four)))
    ; b) It just creates a cell for the first sublist then they are joined with append method
    ; result: ((EVA LISA) KARL SVEN EVA LISA KARL SVEN)
    (print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))
    ; c) It makes a sublist replacing the second argument by the first one in the original list
    ; result: (EVA GITAN LISA GITAN KARIN)
    (print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))
    ; d) It removes the elements into the given list which are equal to the first argument 
    ; result: (EVA LISA ANNA)
    (print (remove 'sven '(eva sven lisa sven anna)))
    ; e) It femoves the last n elements in the given list
    ; result: (KARL ADAM NILSSON)
    (print (butlast '(karl adam nilsson gregg alisson vilma) 3))
    ; f) It gets the list[n]
    ; result: C
    (print (nth 2 '(a b c d e)))
    ; g) It mixes the functions cdr and nth
    ; result: (C D E)
    (print (nthcdr 2 '(a b c d e)))
    ; h) It takes the list as sets and make the intersection between they 
    ; result: (B C) 
    (print (intersection '(a b c) '(x b z c)))
    ; i) As the a) it gets an element in the given list
    ; result: (4)
    (print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))

; 4.- Input: Lista con la estructura  ((A . x) (B . y) (C . z)) -> Output: Lista con las siguiente estrucura: ( ((x y) . A) ((y z) . C) ((z y x) . B) )
    (defun recombina(L)
        (list 
            (cons (list (cdar L) (cdadr L)) (caar L))
            (cons (list (cdadr L) (cdaddr L)) (caaddr L) )
            (cons (list (cdaddr L) (cdadr L) (cdar L)) (caadr L))
        )
    )
    ;(print (caar (list (cons 'hola 1) (cons 'B 2) (cons 'C 3))))
    (print (recombina (list (cons 'dificil 1) (cons 'listas 2) (cons 'adios 33))))

; 5.- Input: Number n -> Output: Boolean according if is real and != 0

    (defun RealnoCero(n)
        (and (realp n) (not (zerop n)))
    )
    (print (RealnoCero 2))

; 6.- Input: Any data -> Output: List which values are bool type according if the data belongs that type
    (defun analiza(x)
        (list (atom x) (numberp x) (listp x) (consp x) (null x))
    )

; 7.- Input: List1 List2 -> Output: List which contains the elements in list1 and list 2 interchanging their elements
    (defun intercalar(l1 l2)
        (cond
            ( (null l1) l2)
            ( (null l2) l1)
            (t (append (list (car l1) (car l2)) (intercalar (cdr l1) (cdr l2))))
        )
    )
    (print (intercalar '(a s) '(1 2 3 4 5 6 5 a d f)))
    
; 8.- Input: Two lists -> Output: Bool which says if the given list are identically equal
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
    )
    (print (mismotipo '(ab 11 4 .1) '(ab 32 4 .5)) )

; 9.- Input: String -> Output: Palindrome of the input
    (defun palindromo(str)
        (let 
        ( (pal (coerce str 'list)) )
            (loop for c across (reverse str) do
                (setq pal (append pal (list c)))
            )
            (coerce pal 'string)
        )
    )
    (print (palindromo "Como"))

; 10.- Input: Number -> Output: Bool according if the number is a leap year

    (defun añoBisiesto(año)
        (cond 
            ( (= (mod año 100) 0)   (= (mod año 400) 0))
            (t                      (= (mod año 4) 0))
        )
    )
    (print (añoBisiesto 2011))