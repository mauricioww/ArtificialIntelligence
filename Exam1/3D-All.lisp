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

(defparameter *flag* 0)
(defparameter *plain-list* nil)
(defparameter *leaf* nil)
(defparameter *list-nodes* nil)


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
        (list x y 0)
    )
)


(defun  get-key (x y z)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+(* 2 (+ x (* y (+ 1 (get-maze-rows))))) z))


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


(defun get-mov-coord(start end)
    (let
       (
            (x1     (first start))
            (y1     (second start))

            (x2     (first end))
            (y2     (second end))
        )

        (cond
            ( (> y1 y2) :left)
            ( (< y1 y2) :right)
            ( (> x1 x2) :up)
            ( (< x1 x2) :down)
        )
    )
)

(defun get-grandpa (coord)
    (let*
        (
            (x (first coord))
            (y (second coord))
            (z (third coord))
            (val (get-key x y z))
            (op nil)
            (ancestor nil)
            (key nil)
            (grandpa nil)
        )
        (setq ancestor (gethash val *memory-ancestor*)) ;; Parent
        (setq key val)
        (setq val (get-key (first (second ancestor)) (second (second ancestor)) (third (second ancestor))) ) ;; New key
        (setq grandpa (gethash val *memory-ancestor*))  ;; Grandpa

        (case (gethash key *memory-operations*)
            (0      :up)
            (2      :right)
            (4      :down)
            (6      :left)
        )
    )
)

;;      Here is one of the most important function in the whole code, the idea is simple: check the last transtion to know
;;      and also check if the current state is one new config (16, 17) in the moment we know that we could check 
;;      if this new transitions is a simple one o a complex one.
;;      I use some tricks with logic bit operations to make easier the programming, and also it looks very fancy.
(defun valid-operator? (op state &optional new-state)
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
            (realParent nil)
        )
        (if bridge
            (progn
                (setq lastMov
                    (if *flag* 
                        (get-grandpa state)
                        (case (fourth (first *memory*)) 
                            (0      :up)
                            (2      :right)
                            (4      :down)
                            (6      :left)
                        )
                    
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
            (:up            (list x-- y 0)    )
            (:right         (list x y++ 0)    )
            (:down          (list x++ y 0)    )
            (:left          (list x y-- 0)    )
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
    (setq  *time1* 0)
    (setq  *time2* 0)
    (setq  *memory-operations*  (make-hash-table))   
    (setq  *memory-ancestor*    (make-hash-table))
    (setq  *memory-open*        (make-hash-table))
    (setq  *memory-distance*    (make-hash-table))
    (setq *plain-list* nil)
    (setq *flag* nil)
)

;;;=======================================================================================
;;                  Best-First-Search   Algorithm
;;             From here to forward is the implementation of the second algorithm with 
;;             the manhattan function aptitude. It's important to say that Im gonna
;;             add new functions and I will use some previous functions.

;;             State representation:
;;                                          (aptitude (xi yi)) 
;;;=======================================================================================


;;=======================================================================================
;;The two following functions are made for insert in order the new nodes into the open
;;=======================================================================================

;;  The state contains the aptitudeValue, I just pass it as argument to make it easier to read



(defun insert-in-ordered-list (value state states)
  "Inserta en una lista ordenada en O(n)"
  (let
    ((front (first states)))

    (if (null states)
      (cons state nil)
      (if (<= value (first front))
        (cons state states)
        (cons front (insert-in-ordered-list value state (rest states))))
        )
    ))

(defun insert-to-open-order (node)
    "It adds the new state to open"
    (setq *open*                (insert-in-ordered-list (first node) node *open*))
)

(defun  never-seen  (x y z)
  "Predicado. Te regresa si este es la primera vez que veo este estado"
  (null (gethash (get-key x y z) *memory-operations*)))

(defun  add-to-hash (state op)
  "Añade un estado a la memoria"
    (let*  
        ( 
            (coordinates (second state)) 
            (x           (first  coordinates)) 
            (y           (second coordinates))
            (z           (third coordinates))
            (val         (get-key x y z))
        )
        (setf (gethash val *memory-operations*) op)
        (setf (gethash val *memory-ancestor*) *current-ancestor*)
    )
)

;;  Aptitude function
(defun Manhattan(coord)
    "It returns the distance from the current coords to the goal coords"
    (let 
        ( 
            (x1 (first  coord))
            (y1 (second coord))  
            (x2 (aref *goal* 0))
            (y2 (aref *goal* 1))
        )
        (+ (abs (- x2 x1)) (abs (- y2 y1)) )
    )
)

(defun extract-solution-informed (state)
"Rastrea en *memory* todos los descendientes de state hasta llegar al estado inicial"
  (let
    (
      (current    state)
      (operation  nil)
      (ansestor   nil)
      (value      nil))

      (loop  while  (not (null current)) do
        (setq value     (get-key (first (second current)) (second (second current)) (third (second current))))
        (setq operation (gethash value *memory-operations*))
        (setq ansestor  (gethash value *memory-ancestor*))
        (setq current   ansestor)

        (push operation *solution*))

      (setq *solution* (rest *solution*))))

(defun expandBFS(state)
    (let
        (
            (currentC       (second state))
            (neww       nil)
            (new-state      nil)
        )
        (incf *expanded*)
        (dolist (op *operators*)
            (setq new-state (apply-operator op currentC))      ;; Make a transition in the maze
            (cond (
                (and (valid-operator? op currentC new-state) (valid-state? (first new-state) (second new-state)) )   ;; Check if is correct the new transition
                    (if (never-seen (first new-state) (second new-state) (third new-state))        ;; If this transition is not in memory we can continue
                        (progn 
                            (setq neww (list (Manhattan new-state) new-state))
                            (add-to-hash neww (second op))
                            (insert-to-open-order neww)                                      ;; And finally insert into open but in order
                        )
                    )
                )
            )
        )
    )   
)

(defun Best-First-Search()
"Realiza una búsqueda best First, desde un estado inicial hasta un estado meta"
    (reset-all)
    (setq *flag* t)
    (let
        (
            (start          (list (Manhattan (array-to-list *start*)) (array-to-list *start*)))    ;; We create a new node to instert into open
            (goal           (list (Manhattan (array-to-list *goal*)) (array-to-list *goal*)))    ;; We create a new node to instert into open

            (node           nil)
            (goal-found     nil)
        )

        (insert-to-open-order   start)
        (add-to-hash            start -1 )
        
        (loop until (or goal-found (null *open*)) do

            (setq node (pop *open*))

            (cond    
                ((equal node goal)                  (setq  goal-found t) (format t "Éxito. Meta encontrada ~%") )        ;; Here we just have to check coords
                (t                                  
                                                    (setq *current-ancestor* node)     ;; Update the ancestor check this, get the grandpa 
                                                    (expandBFS node) ;; Here needs a if to choose which type of expand we need 
                )
            )
        )
        (extract-solution-informed node)
    )
)


;;;=======================================================================================
;;                          A*   Algorithm
;;             From here to forward is the implementation of the third algorithm with 
;;             the manhattan function aptitude. It's important to say that Im gonna
;;             add new functions and I will use some previous functions.

;;             State representation:
;;                                          (aptitude (xi yi)) 
;;;=======================================================================================


;;; ==================================================================================
;;;     The two following functions are helpers to compute the depth in the search tree
;;; ==================================================================================
(defun  set-depth  (state distance)
  "Add to memory distance"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (z           (third coordinates))
      (val         (get-key x y z)))

    (setf (gethash val *memory-distance*) distance)))


(defun  get-depth  (state)
  "Get the value in the memory, defualt zero"
    (let*  
        ( 
            (coordinates (second  state)) 
            (x           (first  coordinates)) 
            (y           (second coordinates))
            (z           (third coordinates))

            (val         (get-key x y z)) 
            (depth    (gethash val *memory-distance*))
        )
            (if (null depth) 0 depth)
    )
)

(defun cost-plus-aptitude (coords)
    "This function is very clear"
    (let
        (
            (apt    (Manhattan coords))
            (dist   (get-depth (list 0 coords)) )
        )
        (+ apt dist)
    )
)

(defun delete-from-ordered-list (coordinates states)
  (if (null states) (return-from delete-from-ordered-list nil))
  (let
    ( 
      (front (first states))
      (end    (rest states)))
    
        (if (equal coordinates (second front))
            end
        (cons front (delete-from-ordered-list coordinates end))
        )
    )
)


(defun get-hash-coord (coords)
    (get-key (first coords) (second coords) (third coords))
)

(defun expand-A-star (state)
  (let*
    (
      (key        (get-hash-coord (second state)))
      (new-coord  nil)
      (new-state  nil)
      (maybe      nil))

    (incf  *expanded*)
    (setf (gethash key *memory-open*) nil)

    (dolist  (op  *operators*)
      (setq new-coord (apply-operator op (second state)))
      (cond 
        ( (and (valid-state? (first new-coord) (second new-coord)) (valid-operator? op (second state) new-coord) ) 

          (if (never-seen (first new-coord) (second new-coord) (third new-coord))
            (progn
                (incf  *id*)
                (setq  new-state  (list (cost-plus-aptitude new-coord) new-coord))

                (setq maybe (gethash (get-hash-coord new-coord) *memory-open*))

                (set-depth new-state (1+ (get-depth *current-ancestor*)))

                (add-to-hash new-state (second op))

                (if  (and (not (null maybe)) (< maybe (first new-state)))
                    (delete-from-open (second new-state)) )

                (insert-to-open-order new-state)
            ))
        )
      )
    ) )
)


;;;=======================================================================================
;;        Load functions and invoking star_maze
;;;=======================================================================================


(defun A-Star()
    (reset-all)
    (setq *flag* t)
    (let
        (
            (start          (list (Manhattan (array-to-list *start*)) (array-to-list *start*)))    ;; We create a new node to instert into open
            (goal           (list (Manhattan (array-to-list *goal*)) (array-to-list *goal*)))    ;; We create a new node to instert into open

            (node           nil)
            (goal-found     nil)
        )

        (insert-to-open-order   start)
        (add-to-hash            start -1)
        
        (loop until (or goal-found (null *open*)) do

            (setq node (pop *open*))

            (cond    
                ((equal node goal)                  (setq  goal-found t) (format t "Éxito. Meta encontrada ~%") )        ;; Here we just have to check coords
                (t                                  
                                                    (setq *current-ancestor* node)     ;; Update the ancestor 
                                                    (expand-A-star node) ;; Here needs a if to choose which type of expand we need 
                )
            )
        )
        (extract-solution-informed node)
    )
)

(add-algorithm 'A-Star)
(add-algorithm 'Best-First-Search)
(start-maze)