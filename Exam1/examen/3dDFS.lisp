;;                  =============        Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============
;;                                                 3D Mazes with DFS

;;                                  Initial state           |                   Final state
;;             (xi, yi)  ->  This is the initial position        (xf, yf)  ->   This is the final position

;;          Explanation: As you can see both states are simple coordinates in the maze, it's very obvious. 

;;               Rules: 1. The agent cannot move to other cell which has the constraints according to a 3d maze

;;          =============      Im gonna use some of the previous functions because this is the DFS algorithm      ==============

(load "maze_lib.lisp")

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos

(defparameter  *memory-operations*  (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-ancestor*    (make-hash-table))    ;; Memoria de ancestros

(defparameter  *memory-open*       (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-distance*   (make-hash-table))    ;; Memoria de la distancia a ese nodo


(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *expanded*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *maxima-frontera*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen

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

(defun and-operator (config n)
    "Returns boolean according the config allows the movement"
    (= (logand config n) n)
)

(defun get-config (coord)
    "Here I write to save code, I mean here I split the state to get the config wall"
    (get-cell-walls (first coord) (second coord))
)


(defun get-walls-ancestor(idAncestor nodes)
    "Returns the walls configurations of some previous state"
    (cond
        ((null nodes)                               nil)
        ((= idAncestor (first (first nodes)))       (get-config (second (first nodes))))
        (t                                          (get-walls-ancestor idAncestor (rest nodes)))
    )
)

(defun array-to-list(arr)
    "It turns the original array into a list to make easier the programming"
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
    "It cheks if the state is inside the maze"
    (let*
        (
            (rows (get-maze-rows))
            (cols (get-maze-cols))
        )
        (and (< x rows) (>= x 0) (< y cols) (>= y 0))
    )
)

;;      Here one of the most important function in the whole code, the idea is simple: check the last transtion to know
;;      and also check if the current state is one new config (16, 17) in the moment we know that we could check 
;;      if this new transitions is a simple one o a complex one.
;;      I use some tricks with logic bit operations to make easier the programming, and also it looks very fancy.
(defun valid-operator? (op state)
    "This function checks if the transition in the maze is correct"
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
        (if bridge
            (progn
                (setq lastMov
                    (case (fourth (first *memory*)) 
                        (0      :up)
                        (2      :right)
                        (4      :down)
                        (6      :left)
                    )
                )
                (setq level 
                        (case lastMov
                            (:up                 (if (= config 17) 1 0) )
                            (:right              (if (= config 16) 1 0) )
                            (:down               (if (= config 17) 1 0) )
                            (:left               (if (= config 16) 1 0) )
                        )
                )
            )
        )
        (case name
            (:up    
                    (cond
                        ((and (= config 16) (= level 1))              nil)
                        ((and (= config 17) (= level 0))              nil)
                        ((not bridge)                                 (not (and-operator config 1)))
                        (t                                            t)
                    )
            )
            (:right         
                    (cond
                        ((and (= config 16) (= level 0))              nil)
                        ((and (= config 17) (= level 1))              nil)
                        ((not bridge)                                 (not (and-operator config 2)))
                        (t                                            t)
                    )
            )
            (:down  
                    (cond
                        ((and (= config 16) (= level 1))              nil)
                        ((and (= config 17) (= level 0))              nil)
                        ((not bridge)                                 (not (and-operator config 4)))
                        (t                                            t)
                    )     
            )
            (:left         
                    (cond
                        ((and (= config 16) (= level 0))              nil)
                        ((and (= config 17) (= level 1))              nil)
                        ((not bridge)                                 (not (and-operator config 8)))
                        (t                                            t)
                    )
            )
        )
    )
)

(defun get-z(config op)
    "According to the parameter config it computes if we are over the 'bridge' or under the 'bridge'"
    (case op
        (:up            (if (= config 17) 1 0) )
        (:right         (if (= config 16) 1 0) )
        (:down          (if (= config 17) 1 0) )
        (:left          (if (= config 16) 1 0) )
    )
)

;;      As I like the simple programming the firs idea I got was: Why just don't make a special state when is 16 or 17.
;;      I mean, in those states we just need to know if we are over o under the bridge so in that case the representation
;;      of the state change from two coordinates to three cooridnates.
;;      And thats it. Obviously is the responsability of the programmer to know this and argue the fact of why using this technique.
(defun apply-operator (op state)    
    "This function checks simple apply the operator, but it make a trick."
    (let*
        (
            (x (first state))
            (y (second state))
            (x++ (1+ x))
            (x-- (1- x))
            (y++ (1+ y))
            (y-- (1- y))
            (name (first op))
            (nextConfig 0)
            
        )
        (setq nextConfig
            (case name
                (:up                (if (valid-state? x-- y) (get-cell-walls x-- y) -1) )
                (:right             (if (valid-state? x y++) (get-cell-walls x y++) -1) ) 
                (:down              (if (valid-state? x++ y) (get-cell-walls x++ y) -1) )
                (:left              (if (valid-state? x y--) (get-cell-walls x y--) -1) )
            )
        )
        (if (> nextConfig 15)
                (case name
                    (:up            (return-from apply-operator (list x-- y (get-z nextConfig name)))    )
                    (:right         (return-from apply-operator (list x y++ (get-z nextConfig name)))   )
                    (:down          (return-from apply-operator (list x++ y (get-z nextConfig name)))    )
                    (:left          (return-from apply-operator (list x y-- (get-z nextConfig name)))    )
                )
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

(defun expandDFS (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (new-state  nil))
            (incf  *expanded*)
           (dolist  (op  *operators*  descendientes) 
	         (setq  new-state  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado) (valid-state? (first new-state) (second new-state)))           ;; se valida el resultado...
	                (setq  descendientes  (cons  (list new-state op) descendientes))))) )

(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"
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

(defun extract-solution-DFS (nodo)
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

(defun  simple-DFS ()
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
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...

	   (cond    ((equal  edo-meta  estado)
                    (setq *time2* (get-internal-run-time))
		                (format  t  "Éxito. Meta encontrada ~%~%")
                        (extract-solution-DFS  nodo) 
		                (display-solution)
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expandDFS estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)))))))  )
     
;;;=======================================================================================
;;        Load functions and invoking star_maze
;;;=======================================================================================

(add-algorithm 'simple-DFS)
(start-maze)