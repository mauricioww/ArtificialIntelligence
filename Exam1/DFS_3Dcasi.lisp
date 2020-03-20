;;                  =============        Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============
;;                                                 2D Mazes with DFS

;;                                  Initial state           |                   Final state
;;             (xi, yi)  ->  This is the initial position        (xf, yf)  ->   This is the final position

;;          Explanation: As you can see both states are simple coordinates in the maze, it's very obvious. 

;;               Rules: 1. The agent cannot move to other cell which has the constraints according to a 3d maze

;;          =============      Im gonna use some of the previous functions because this is the DFS algorithm      ==============

(load "maze_lib.lisp")

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos


(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *expanded*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *maxima-frontera*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter *real-ancestor* nil)
;;(defparameter  *solution*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

(defparameter *listSolution* '())

(defparameter  *operators*  '(  
                                (:up            0)
                                (:right         2)
                                (:down          4)
                                (:left          6)
                            )
)

(defparameter *time1* 0) ;; Variables to calculate the execute time of the two algorithms 
(defparameter *time2* 0)

;; It returns a list with the binary num representation of the input
(defun getBinaries (n &optional flag)
    "Gets the configuration of a specific cell"
    (if (null flag)
        ;; (reverse (loop for i below 5 collect (if (logbitp i n) 1 0)) )
        (reverse (loop for i below 4 collect (if (logbitp i n) 1 0)) )
    )
)

(defun and-operator (config n)
    "Returns boolean according the config allows the movement"
    (= (logand config n) n)
)

(defun get-config (coord)
    (get-cell-walls (first coord) (second coord))
)

(defun get-walls-ancestor(idAncestor nodes)
    ;; (format t "~% In id ~A ~%" (first (first nodes)))
    (cond
        ((null nodes)                               nil)
        ((= idAncestor (first (first nodes)))       (get-config (second (first nodes))))
        (t                                          (get-walls-ancestor idAncestor (rest nodes)))
    )
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

(defun flippea (b)
    (boole BOOLE-XOR b 1)
)

;;      This function is a little hard to understand, arguments: the code of the wall of the a cell, we suppose a level in the bridge
;;      and finally a list with all the posibilities of the ancestor's wall code
;;      It computes the current position in the 'bridge' 0-down 1-up.
;;      
(defun get-level(conf level args)
;; 12 6 9 5 3
    ;; (if (or (and-operator conf 3) (and-operator conf 5) (and-operator conf 6) (and-operator conf 9) (and-operator conf 12))
    ;; (if (apply 'or (mapcar #'and-operator list (list conf)))
        ;; (flippea level)
        ;; (return-from get-level level)
    ;; )
    (if (mapcar (lambda (n args) (funcall #'and-operator n (car args)) ) (make-list (length args) :initial-element conf) (mapcar #'list args))
        (flippea level)
        level
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
    (let*
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


(defun free-way (config bit1 bit2)
    (if (or (= 0 (logand config bit1)) (= 0 (logand config bit2))) 1 0)
)

(defun valid-operator? (op state)
    (let*
        (    
            (config (get-config state))

            (bridge (> config 15))

            (configParent 0)
            (idWanted 0)

            (name (first op))
            (level 0)
            (lastMov nil)
        )

        (format T "~% Inspeccionando movimiento ~A con cells config ~A ~%" (first op) config) 

        (if bridge
            (progn
                (setq idWanted (third (first *memory*)))
                (setq lastMov
                    (case (fourth (first *memory*)) 
                        (0      :up)
                        (2      :right)
                        (4      :down)
                        (6      :left)
                    )
                )
                ;; (print *memory*)
                (setq configParent (get-walls-ancestor idWanted *memory*))
                ;; (format T "~% Parent ~A, has config ~A . The current cell is config ~A ~%" idWanted configParent config)
                (setq level 
                        (case lastMov
                            (:up                 (if (= config 17) 1 0) )
                            (:right              (if (= config 16) 1 0) )
                            (:down               (if (= config 17) 1 0) )
                            (:left               (if (= config 16) 1 0) )
                        )
                        
                        ;; (cond 
                        ;;     ((= config 16)      (free-way configParent 2 8))
                        ;;     ((= config 17)      (free-way configParent 1 4))
                        ;; )
                )
            )
        )
        (case name
            (:up    
                    
                    (cond
                        ;; (flagB              (or (and-operator configC level) (and-operator configC 1)))
                        ((and (= config 16) (= level 1))              nil)
                        ((and (= config 17) (= level 0))              nil)
                        ;; ((= config 17)              (and-operator configC level))
                        ;; (t                  (not (and-operator configC 1)))
                        ((not bridge)                                 (not (and-operator config 1)))
                        (t                                            t)
                    )
                    ;; (setq flag (and flag (not (and-operator config 1))))
            )
            (:right         
                    (cond
                        ((and (= config 16) (= level 0))              nil)
                        ((and (= config 17) (= level 1))              nil)
                        ;; ((= config 17)              (and-operator configC level))
                        ;; (t                  (not (and-operator configC 1)))
                        ((not bridge)                                 (not (and-operator config 2)))
                        (t                                            t)
                        ;; (flagB              (and-operator configC (flippea level)))
                        ;; (flagB               (or (and (= level 1) (= configC 16)) (and (= level 0) (= configC 17) )) ) 
                        ;; (t                  (not (and-operator configC 2)))
                    )
                    ;; (setq flag (and flag (not (and-operator config 2))))
            )
            (:down  
                    (cond
                        ((and (= config 16) (= level 1))              nil)
                        ((and (= config 17) (= level 0))              nil)
                        ;; ((= config 17)              (and-operator configC level))
                        ;; (t                  (not (and-operator configC 1)))
                        ((not bridge)                                 (not (and-operator config 4)))
                        (t                                            t)
                        ;; (flagB              (or (and-operator configC level) (and-operator configC 1)))
                        ;; (flagB              (and-operator configC level))
                        ;; ((and (= config 16) (= level 1))              nil)
                        ;; ((or (and-operator config 4) (= config 17))        (setq flag nil))
                        ;; (t                  (not (and-operator configC 4)))
                    )     
                    ;; (setq flag (and flag (not (and-operator config 4)))) 
            )
            (:left         
                    (cond
                        ((and (= config 16) (= level 0))              nil)
                        ((and (= config 17) (= level 1))              nil)
                        ;; ((= config 17)              (and-operator configC level))
                        ;; (t                  (not (and-operator configC 1)))
                        ((not bridge)                                 (not (and-operator config 8)))
                        (t                                            t)
                        ;; (flagB          (and-operator configC (flippea level)))
                        ;; (flagB              (or (and (= level 1) (= configC 16)) (and (= level 0) (= configC 17)) ) ) 
                        ;; (t                  (not (and-operator configC 8)))
                    )
                    ;; (setq flag (and flag (not and-operator config 8)))
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
            (:right         (list x y++)    )
            (:down          (list x++ y)    )
            (:left          (list x y--)    )
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
            (FORMAT T "~%~{~a~} a esto ~{~a~}" estado new-state) ;; delete
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
      (print ())
      (setq *time1* (get-internal-run-time))
      (insert-to-open   edo-inicial  nil)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
        ;; (print *open*)
        ;; (print *goal*)
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