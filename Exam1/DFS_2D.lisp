;;                  =============        Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============
;;                                                 2D Mazes with DFS

;;                                  Initial state           |                   Final state
;;             (xi, yi)  ->  This is the initial position        (xf, yf)  ->   This is the final position

;;          Explanation: As you can see both states are simple coordinates in the maze, it's very obvious. 

;;               Rules: 1. The agent cannot move to other cell which has the constraints according to a normal maze

;;          =============      Im gonna use some of the previous functions because this is the DFS algorithm      ==============

(load "maze_lib.lisp")

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos


(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *expanded*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *maxima-frontera*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen

(defparameter *listSolution* '())

(defparameter  *operators*  '(  
                                (:up            0)
                                (:up_right      1)
                                (:right         2)
                                (:down_right    3)
                                (:down          4)
                                (:down_left     5)
                                (:left          6)
                                (:up_left       7)
                            )
)

(defparameter *time1* 0) ;; Variables to calculate the execute time of the two algorithms 
(defparameter *time2* 0)

;; It returns a list with the binary num representation of the input
(defun getBinaries (n)
    "Gets the configuration of a specific cell"
    (reverse (loop for i below 4 collect (if (logbitp i n) 1 0)) )
)

(defun array-to-list(arr)
    (let 
        (
            (x (aref arr 0))
            (y (aref arr 1))
        )
        (list x y)
    )
)

;;;=======================================================================================
;;  CREATE-NODE (estado  op)                  This function is imported.
;;      estado - Un estado del problema a resolver (sistema)  -> (id estado ancestro operation-name)
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================

(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (second op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN        These functions are imported.
;;        
;;        Insert-to-open  recibe una lista y lo inserta bajo el criterio de DFS
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================

(defun insert-to-open (estado  op) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (setq *maxima-frontera* (max (+ 1 (length *open*)) *maxima-frontera*))
         (push  nodo  *open*) )
)

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

(defun valid-state? (x y)
    (let
        (
            (rows (get-maze-rows))
            (cols (get-maze-cols))
        )
        (and (< x rows) (>= x 0) (< y cols) (>= y 0))
    )
)

(defun corner(config i j xi yi)
    "It says if at least one wall of the corner in a cell is free-way"
    (and (and (zerop (nth i config)) xi) (and (zerop (nth j config))) yi)
)

(defun valid-operator? (op state)
    (let*
        (    
            (x (first state))
            (y (second state))

            (x++ (1+ x))
            (x-- (1- x))
            (y++ (1+ y))
            (y-- (1- y))

            (rows (get-maze-rows))
            (cols (get-maze-cols))

            (walls (getBinaries (get-cell-walls x y)))

            (w0 (zerop (nth 0 walls)))
            (w1 (zerop (nth 1 walls)))
            (w2 (zerop (nth 2 walls)))
            (w3 (zerop (nth 3 walls)))

            (name (first op)) 
        )   

        (case name
            (:up            (and    (>= x-- 0)          w3) )
            (:right         (and    (< y++ cols)        w2) )
            (:down          (and    (< x++ rows)        w1) )
            (:left          (and    (>= y-- 0)          w0) )
            
            (:up_rigth      (and    (>= x-- 0)      (< y++ cols)
                                (corner (getBinaries (get-cell-walls x-- y++)) 0 1 w3 w2)
                            )
            )

            (:down_right    (and    (< x++ rows)     (< y++ cols)
                                (corner (getBinaries (get-cell-walls x++ y++)) 0 3 w1 w2)                                
                            )
            )

            (:down_left     (and     (< x++ rows)     (>= y-- 0)
                                (corner (getBinaries (get-cell-walls x++ y--)) 3 2 w0 w1)                                
                            )
            )

            (:up_left       (and     (>= x-- 0)       (>= y-- 0)
                                (corner (getBinaries (get-cell-walls x-- y--)) 1 2 w0 w3)                              
                            )
            )
            
        )
    )
)

(defun apply-operator (op state)    
    (let*
        (
            (x (first state))
            (y (second state))
            (x++ (1+ x))
            (x-- (1- x))
            (y++ (1+ y))
            (y-- (1- y))
            (name (first op))
        )
        (case name
            (:up            (list x-- y)    )
            (:up_right      (list x-- y++)  )
            (:right         (list x y++)    )
            (:down_right    (list x++ y++)  )
            (:down          (list x++ y)    )
            (:down_left     (list x++ y--)  )
            (:left          (list x y--)    )
            (:up_left       (list x-- y--)  )
        )
    )
)

;;;=======================================================================================
;;  EXPAND (estado)                             These functions is imported.
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (new-state  nil))
            (incf  *expanded*)
           (dolist  (op  *operators*  descendientes) 
	         (setq  new-state  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado) (valid-state? (first new-state) (second new-state)))           ;; se valida el resultado...
	                (setq  descendientes  (cons  (list new-state op) descendientes))))) )

(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta



;;;=======================================================================================
;;  EXTRACT-SOLUTION                           These functions are imported.
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solution* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================

(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)) (idOp 0))
	     (loop  while  (not (null  current))  do    
		 (setq  idOp  (fourth current))     ;; agregar a la solución el nodo actual
		 (push  idOp  *solution*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
         (pop *solution*)
	     *solution*))



(defun  display-solution ()
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "1) ~A nodos creados. ~%" *id*)
    (format  t  "2) ~A nodos expandidos. ~%" *expanded*)
    (format  t  "3) Longitud máxima de la Frontera de búsqueda: ~A.~%" *maxima-frontera*)
    (format  t  "4) Longitud de la solución: ~A operadores.~%" (1- (length  *solution*)))
    (print *solution*)
    (format  t  "5) Tiempo: ~A segundos.~%~%" (float (/ (- *time2* *time1*) internal-time-units-per-second))) ;; Running time
   
)  
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  ())
     (setq  *memory*  ())
     (setq  *id*  -1)
     (setq  *current-ancestor*  nil)
     (setq  *expanded*  0)  
     (setq  *maxima-frontera*  0) 
     (setq  *solution*  nil)
     (setq *time1* 0)
     (setq *time2* 0))

(defun  blind-search-DFS ()
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
      (edo-inicial  (array-to-list *start*))
      (edo-meta     (array-to-list *goal*))
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
      (setq *time1* (get-internal-run-time))
      (insert-to-open   edo-inicial  nil)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...

	   (cond    ((equal  edo-meta  estado)
                    (setq *time2* (get-internal-run-time))
		                (format  t  "Éxito. Meta encontrada ~%~%")
                        (extract-solution  nodo) 
		                (display-solution)
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...

			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)))))))  )
     
;;;=======================================================================================
;;        Load functions and invoking star_maze
;;;=======================================================================================

(add-algorithm 'blind-search-DFS)
(start-maze)