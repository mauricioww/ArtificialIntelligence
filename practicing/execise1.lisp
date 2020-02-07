; 1.-
    ; a)
    (defun get5(list)
        ;(first (rest (rest (rest (rest list)))))
        (car (cddddr list))
    )
    (print (get5 '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))) )

    ; b)
    (defun getsec2004()
        (* 60 60 24 366)
    )
    (print (getsec2004 ))

    ; c) 
    (defun compare(x y)
        (and (/= x 0) (<= x y))
    )
    (print (compare 4 12))

    ; d)
    (defun equa_2(a b c)
        (   let (
                    (raiz (/ (sqrt (- (* b b) (* 4 a c))) 2))   ; raiz var
                    (not_raiz (- 0 (/ b 2)))                    ; not raiz var
                )
            (   
                list (+ not_raiz raiz) (- not_raiz raiz)
            )
        )
    )
    (print (equa_2 1 -3 2))
; 2.-
    ; a) 6
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
    ; b) ((EVA LISA) KARL SVEN EVA LISA KARL SVEN) just create a cell for the first
    ; argument and it points to the following arguments
    (print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))
    ; c) new list with first argument instead of second one
    (print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))
    ; d) removes the first argument from the list given
    (print (remove 'sven '(eva sven lisa sven anna)))
    ; e) removes the n last elements 
    (print (butlast '(karl adam nilsson gregg alisson vilma) 3))
    ; f) gets the list[n]
    (print (nth 2 '(a b c d e)))
    ; g)  gets the list[n:]
    (print (nthcdr 2 '(a b c d e)))
    ; h) intersection between the lists given
    (print (intersection '(a b c) '(x b z c)))
    ; i) (4)
    (print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))

; 4.-
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

; 6.-
    (defun analiza(x)
        (list (atom x) (numberp x) (listp x) (consp x) (null x))
    )

; 7.-
    
; 8.-
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

    ;; (print (mismotipo '(ab 11 4 .1) '(ab 32 4 .5)) )

; 9.-
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
    (print (palindromo "Como")))

    ;; (print (coerce (coerce "hola" 'list) 'string))