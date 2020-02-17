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
                ( (null L) nil ) 
                ( (realp (car L))
                    (cond ( (>= (car L) 0 )     (setq res (list (car L) 1)) (setq next (Ultimo-real (cdr L)) )
                                                (cond 
                                                    ((null next) res)
                                                    ((= (car res) (car next)) (list (car res) (1+ (nth 1 res))))
                                                    (t next)
                                                ) 
                        )
                        (t (Ultimo-real (cdr L)))
                    ) 
                ) 
                (t  (Ultimo-real (cdr L))) 
            )
        )
    )
    (print (Ultimo-real '(1/1 24/4 4 12/2 12/2 qw e 6 ds g 6 30/2 2 1 51 6 -1 4 15 6 as 1 6)))

; 6) Input: List -> Output: List wich first element is the number of numeric elems in the given list and the second one is the number of sublist in the given list
;    Note: It just counts the numeric type according the exercise
    (defun conteo(L)
        (cond 
            ((null L) '(0 0))
            ((listp (car L)) (mapcar '+ '(0 1) (conteo (cdr L))) )
            ((numberp (car L)) (mapcar '+ '(1 0) (conteo (cdr L))) )
            (t (conteo (cdr L)))
        )
    )
    (print (Conteo '(1 45 6 2 3 (a b s) (ba 3 2) a 3 341 .34 .5 12 (2) (j))))
    
; 7) Input: List of any type of data even sublist, and each sublist contains any type of data and so on (Unknown deep) 
;   -> Output: A one-level list wich contains all the simple elements in the given list
    (defun aplanap2(L)
        (let
            ( (res '()) )
            (cond
                ((null L) nil)
                ((not (listp (car L))) (cons (car L) (aplanap2 (cdr L))))
                (t (setq res (append (aplanap2 (car L)) (aplanap2 (cdr L)))))
                )
            )
        )
    (print (aplanap2 '(a v (3 1) c s (2 (14 6 q v) 34))))

; 8) Input: List wich contains N sublists of N elements (matrix) -> Output: List wich contains the principal diagonal matrix
    (defun diagonal(matrix &optional (i 0) (j 0))
        (cond
            ((null matrix) nil)
            (t  (cons (nth j (nth i matrix)) (diagonal (cdr matrix) i (1+ j)) ))  
        )
    )
    (print (diagonal '((1 2 3) (4 5 6) (7 8 9)) ))

; 9) Input: List with any type data -> Output: List which contains, according de data, 'A' if is an atom
;    'L' if is list and 'N' if is an empty list
    (defun  info(L)
        (cond
            ( (null L) nil) 
            ( (null (car L)) (cons 'n (info (cdr L))) )
            ( (atom (car L)) (cons 'a (info (cdr L))) )
            ( (listp (car L)) (cons 'l (info (cdr L))) )
        )
    )
    (print (info '(12 (1) () 4 a () g a (asv v 4) a)))

; 10) Input: List wich elements are any type of data -> Output: The sum of the numeric values in the list
    (defun sumaNum(L)
        (cond
            ((null L) 0)
            ((numberp (car L)) (+ (car L) (sumaNum (cdr L))))
            (t (sumaNum (cdr L)))
        )
    )
    (print (sumaNum '(1 a 4 5 8/2)))    

; 11) Input: List wich contains any type of data and even sublist to any depet -> Output: List without the vowels in the given list
;   NOTE: According the exersice the output list also contains sublist at any depth and even they can be empty if they just stored vowels
    (defun filtraVocales(L)
        (labels (
                (esVocal(v) (if (member v '(a e i o u)) t nil) )
                (filtro(L)
                    (cond
                        ( (null L ) nil)
                        ( (esVocal (car L)) (filtro (cdr L)))
                        ( (listp (car L)) (cons (filtro (car L)) (filtro (cdr L))) )
                        (t (cons (car L) (filtro (cdr L))))
                    )
                )
            )
            (filtro L)
        )    
    )
    (print (filtraVocales '(a b a i 21 o (2 a (b a 2) (a e) 3) ab c 3)))

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

; 13) Input: List wich contains any type of data and even sublist to any depet -> Output: Number of the total constructed cell in the given list
;   NOTE: The input must contain only porper sublists
    (defun celdas(L)
        (cond 
            ((null L) 0)
            ( (listp (car L)) (+ (celdas (car L)) (celdas (cdr L))) )
            (t (1+ (celdas (cdr L))))
        )
    )
    (print (celdas '(1 2 4 5 (3 9 (a f (gd (a 0)))) 6)))

; 14) Input: Any number of arguments of any type of data -> Ouput: According to the arguments they will be evualate
; NOTE: The implication or conditional is an operator that operates on two truth values, typically the truth 
; values of two propositions, returning the value of false only when the first proposition is true and the second 
; proposition false, and true in any other case.
; SO just null will be taken as false and everything else is true

    ;; (defun implicaa(&rest args)
    ;;     (labels (
    ;;         (recursivo(L)
    ;;             (cond
    ;;                 ((null L)) t)
    ;;                 (t (recursivo (cdr L)))
    ;;             )
    ;;         ) )
    ;;         (recursivo args)
    ;;     )
    ;; )
    ;; (trace implicaa)
    ;; (print (implicaa 2 'a 'gfa 'g 'awf))

; 15) Input: Two matrix of any dimension -> Output: The result of they multiplication if it can be calculate
;   NOTE: (null '(nil)) -> T
    (defun multMatrix(m1 m2)
        (labels (
                (puedeDiv(m horizontal) 
                    (cond 
                        ((null m) nil)
                        (t (if (null horizontal) (> (length m) 1) (> (length (nth 0 m)) 1) ) )
                    )
                )
                
                (obtieneNrenglones(m n flag &optional top) ;; get n rows from the matrix m 
                    (let 
                        ( (res '()) )
                        (if (null flag) (loop for i in m for stop from 0 below n do (setq res (cons i res))) 
                                        (loop for i from n below top do (setq res (cons (nth i m) res))))
                        (reverse res)
                    )
                )

                (obtieneNcolumnas(m n flag &optional top)  ;; get n colums from the matrix m
                    (let
                        ( (res '()) )
                        (if (null flag) (dolist (r m) (setq res (cons (subseq r 0 n)  res)))
                                        (dolist (r m) (setq res (cons (subseq r n top) res))))
                        (reverse res)
                    )
                )
                
                (divMatrix(m horizontal flag n)
                    (if (null horizontal)
                        (obtieneNrenglones m n flag (length m)) ; true
                        (obtieneNcolumnas m n flag (length (nth 0 m))) ; false
                    ) 
                )

                (dot(m1 m2) ; product m1Xm2, since m1 and m2 are the most basic matrix from their original form
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
                        (reverse matrixRes)
                    )
                )

                (juntaMatriz(a b c d) ; Using final as stack
                    (let
                        ( (res '()) )
                        (loop for i in a for j in b do
                            (push (append i j) res)
                        )
                        (loop for i in c for j in d do
                            (push (append i j) res )
                        )
                        (reverse res)
                    )
                )

                (valida (m1 m2)
                    (= (length (nth 1 m1)) (length m2))
                )

                (productoRecur(m1 m2)
                    (let
                        (
                            (r1 (+ 0 (floor (length m1) 2)))
                            (c2 (+ 0 (floor (length (nth 0 m2)) 2)))
                            (a' ())
                            (b' ())
                            (c' ())
                            (d' ())
                            (e' ())
                            (f' ())
                            (g' ())
                            (h' ())
                            (dd '())
                        )
                        (if (and (puedeDiv m1 '()) (puedeDiv m2 '(TRUE))) ; m1 will be horizontally divided and m2 vertically
                            (progn ; two actions if true
                                (setq 
                                    a (divMatrix m1 '() '() r1)
                                    b (divMatrix m1 '() '(F) r1)
                                    c (divMatrix m2 '(F) '() c2)
                                    d (divMatrix m2 '(F) '(F) c2)

                                    e (productoRecur a c)
                                    f (productoRecur b c)
                                    g (productoRecur a d)
                                    h (productoRecur b d)
                                )
                                (juntaMatriz e g f h)
                            ) 
                                (dot m1 m2)
                        )
                    )
                )
            )

            (if (valida m1 m2) (productoRecur m1 m2))

            )

        
    )
    (print (multMatrix '((1 2 3 4 5) (4 5 1 3 5) (6 7 8 9 10)) '((1 2) (3 8) (9 5) (7 3) (10 6))))

;                =============   Solutions for the remaining exercises in the third package using recursion    ================

; 17) Here are several ways to compute the Fibonacci serie, also these are gonna be compared with the number 50 as argument.
;               Definition of the functions
;   a)
        (defun fibo1 (n)
        ;; "Naive recursive computation of the nth element of the Fibonacci sequence"
            (if (< n 2) n
                (+ (fibo1 (1- n)) (fibo1 (- n 2)))))

;   b)
        (defun fibo2 (n)
        ;; "Tail-recursive computation of the nth element of the Fibonacci sequence"
            (labels ((fib-aux (n f1 f2)
                                (if (zerop n) f1
                                (fib-aux (1- n) f2 (+ f1 f2)))))
                    (fib-aux n 0 1)))

;   c) 
        (defun fibo3(n)
        ;"loop-based iterative computation of the nth element of the Fibonacci sequence"
            (loop for f1 = 0 then f2
                    and f2 = 1 then (+ f1 f2)
                    repeat n finally (return f1)))

;   d)
        (defun fibo4 (n)
        ;"do-based iterative computation of the nth element of the Fibonacci sequence"
            (do ((i n (1- i))
                (f1 0 f2)
                (f2 1 (+ f1 f2)))
                ((= i 0) f1)))

;   e) 
        (defun fibo5 (n)
            ;"CPS computation of the nth element of the Fibonacci sequence"
            (labels ((fib-aux2 (n k)
                                (if (zerop n)
                                    (funcall k 0 1)
                                (fib-aux2 (1- n) (lambda (x y)
                                                    (funcall k y (+ x y)))))))
                    (fib-aux2 n #'(lambda (a b) a))))

;   f) 
        (defun fibo6 (n)
            (labels ((fib2 (n)
                            (cond ((= n 0)
                                    (values 1 0))
                                (t
                                    (multiple-value-bind (val prev-val)
                                                        (fib2 (- n 1))
                                    (values (+ val prev-val)
                                            val))))))
                (nth-value 0 (fib2 n))))

;   g) 
        (defun fibo7 (n)
            ;"Successive squaring method from SICP"
            (labels ((fib-aux3 (a b p q count)
                                (cond ((= count 0) b)
                                    ((evenp count)
                                    (fib-aux3 a
                                                b
                                                (+ (* p p) (* q q))
                                                (+ (* q q) (* 2 p q))
                                                (/ count 2)))
                                    (t (fib-aux3 (+ (* b q) (* a q) (* a p))
                                                (+ (* b p) (* a q))
                                                p
                                                q
                                                (- count 1))))))
                    (fib-aux3 1 0 0 1 n)))

;   h) 
        (defun fibo8 (n)
            (if (< n 2) n
                (if (oddp n) 
                (let ((k (/ (1+ n) 2)))
                    (+ (expt (fibo8 k) 2) (expt (fibo8 (1- k)) 2)))
                (let* ((k (/ n 2)) (fk (fibo8 k)))
                    (* (+ (* 2 (fibo8 (1- k))) fk) fk)))))

;   i)
        (let ((aFibo 1)
             (bFibo 5))
        (defun fibo9 ()
            (prog1 aFibo (psetf aFibo bFibo bFibo (+ aFibo bFibo)))))

;   j) 
        ;; Taken from Winston's Lisp, 3rd edition, this is a tail-recursive version, w/o an auxiliary function
        (defun fibo10 (n &optional (i 1) (previous-month 0) (this-month 1)) 
            (if (<= n i)
                this-month
                (fibo10 n (+ 1 i) this-month (+ this-month previous-month))))
;   k)
        (defun fast-fib-pair (n)
            ;"Returns f_n f_{n+1}."
            (case n
                ((0) (values 0 1))
                ((1) (values 1 1))
                (t (let ((m (floor n 2)))
                    (multiple-value-bind (f_m f_m+1)
                        (fast-fib-pair m)
                    (let ((f_m^2   (* f_m f_m))
                            (f_m+1^2 (* f_m+1 f_m+1)))
                        (if (evenp n)
                            (values (- (* 2 f_m+1^2)
                                        (* 3 f_m^2)
                                        (if (oddp m) -2 2))
                                    (+ f_m^2 f_m+1^2))
                            (values (+ f_m^2 f_m+1^2)
                                    (- (* 3 f_m+1^2)
                                        (* 2 f_m^2)
                                        (if (oddp m) -2 2))))))))))
                            
;   l) 
        ;; Fibonacci - Binet's Formula
        (defun fibo11(n)
            (* (/ 1 (sqrt 5))
                (- (expt (/ (+ 1 (sqrt 5)) 2) n)
                (expt (/ (- 1 (sqrt 5)) 2) n))))

;   m) 
        (defun fibo12 (n)
            (/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
                (expt (/ (- 1 (sqrt 5)) 2) n))
                (sqrt 5)))

;   Now we just need execute the functios one at time, and observe their performance. Some of them are better than the others.
    ;; (time (print (fibo1 13)))
    ;; (time (print (fibo2 13)))
    ;; (time (print (fibo3 13)))
    ;; (time (print (fibo4 13)))
    ;; (time (print (fibo5 13)))
    ;; (time (print (fibo6 12))) ; Here we use n-1 becuase of the definition of the function it takes n-1 as the real n argument
    ;; (time (print (fibo7 13)))
    ;; (time (print (fibo8 13)))
    ;; (time (print (fibo9)))
    ;; (time (print (fibo10 13)))
    ;; (time (print (fast-fib-pair 13)))
    ;; (time (print (fibo11 13)))
    ;; (time (print (fibo12 13)))

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
;    NOTE: The list in the function must have the same length, the remaining items in list will not be computed by func
    (defun myMapcar(func &rest args) ; At least one list to operate
        (if (member nil args)
            nil
            (cons (apply func (mapcar 'car args)) (apply 'myMapcar func (mapcar 'cdr args)))
        )
    )
    (print (myMapcar '/ '(1 2 3) '(4 5 6 )))

; 19) Input: List of any type of data even sublist, and each sublist contains any type of data and so on (Unknown depth) 
;   -> Output: A one-level list wich contains all the simple elements in the given list
    (defun aplanap3(L)
        (let
            ( (res '()) )
            (cond
                ((null L) nil)
                ((not (listp (car L))) (cons (car L) (aplanap3 (cdr L))))
                (t (setq res (append (aplanap3 (car L)) (aplanap3 (cdr L)))))
                )
            )
        )
    (print (aplanap3 '(a v (3 1) c (3 5 (r (4 32 (4) (32 3)) (3 2 k))) s (2 (14 6 q v) 34))))

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
