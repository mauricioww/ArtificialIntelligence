;;                  =============        Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============
;;                                                 Puzzle: GLOL

;;                                  Initial state           |                   Final state
;;                        ((t t t t) (nil nil nil nil))                 ((nil nil nil nil) (t t t t))
;;          Explanation: As the name of the program "glol.lisp" stands for granjero: farmer, lobo: wolf, obeja: sheep, legumbre: legume
;;                       Im gonna take the same order as above in order to it makes sense in programming. 
;;                       Each of the two sublist represents one shore in the ocean.
;;                       Why did I use nil and t? Well... this is the easiest way to solve the puzzle because the validations are faster
;;                       than if I would have used another data type.
;;               Rules: 1. The farmer always must be in the ship.
;;                      2. Wolf and sheep never must be together.
;;                      3. Sheep and legume never must be together.
;;                      4. The puzzle has finished when all the characters have crossed to the other shore.

;;          =============       Some of the following code is imported from 'Misioneros-Canibales.lisp'.      ==============

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos


(defparameter  *operators*  '(  (:viaja_granjero_solo     0 )
                                (:viaja_granjero_lobo     1 )
                                (:viaja_granjero_oveja    2 )
                                (:viaja_granjero_legumbre 3 ) )
)

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *expanded*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *maxima-frontera*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

(defparameter *time1* 0) ;; Variables to calculate the execute time of the two algorithms 
(defparameter *time2* 0)

;;;=======================================================================================
;;  CREATE-NODE (estado  op)                  This function is imported.
;;      estado - Un estado del problema a resolver (sistema)  -> (id estado ancestro operation-name)
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================

(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN        These function are imported.
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================

(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (setq *maxima-frontera* (max (+ 1 (length *open*)) *maxima-frontera*))
         (cond 
            ((eql  metodo  :depth-first)
	            (push  nodo  *open*))
	          ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)                  This function is imported.
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (caar estado)  0  1))

;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]            This function is mine.
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================

(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."  
  (let*  
      (
        (orilla   (barge-shore  estado))                     
        (a-mover  (second op))  )
        (nth a-mover (nth orilla estado)) ) ;; We just check if there is the element that we wanna move.
) 

;;;=======================================================================================
;;  VALID-STATE (estado)                  This function is mine.
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en la orilla en la que estan solas las cosas no este el lobo y oveja
;;                          o oveja y legumbre
;;;=======================================================================================

(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema..."
  (let* 
    (
      ;;  Here we split the state in its components just for compute the constraint.
      (shore1     (first   estado))   
      (shore2     (second  estado)) 

      (ship1      (first   shore1))
      (ship2      (first  shore2))
      (wolf1      (second  shore1))   
      (wolf2      (second  shore2))
      (sheep1     (third   shore1))
      (sheep2     (third   shore2))
      (legume1    (fourth  shore1))
      (legume2    (fourth  shore2))
    )
    ;;  Here we apply the constraint.
    (not 
      (or  
      (and wolf1 sheep1 (not ship1) )
      (and wolf2 sheep2 (not ship2) )
      (and sheep1 legume1 (not ship1) )
      (and sheep2 legume2 (not ship2) ))
    )
  )
)


;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)               This function is mine.
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  
      (
        (shoreShip    (barge-shore estado))
        (otherShore   (flip shoreShip))
        (origin       (copy-list (nth shoreShip estado)))
        (destiny      (copy-list (nth otherShore estado)))

        (aux    (second op))  ;; Id of operator just for programming
      )

      (if (not (eql aux 0))
        (setf (nth aux origin) nil) ;; Here we take the character from the 'origin'
      ) 
      (if (not (eql aux 0))
        (setf (nth aux  destiny) t)  ;; And here we leave it
      ) 
      (setf (nth 0 origin) nil)     ;; Interchange the ship
      (setf (nth 0 destiny) t)

      (if (eql shoreShip 0)   
        (list origin destiny)
        (list destiny origin)
      ) 
    ) 
)            

;;;=======================================================================================
;;  EXPAND (estado)                             This function is imported.
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (new-state  nil))
            (incf  *expanded*)
           (dolist  (op  *operators*  descendientes) 
	         (setq  new-state  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado) (valid-state? new-state))           ;; se valida el resultado...
	                (setq  descendientes  (cons  (list new-state op) descendientes))))) )

;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES           These function are imported.
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================

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
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION           These function are imported.
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================

(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))

(defun  display-solution (lista-nodos exeTime)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "1) ~A nodos creados. ~%" *id*)
    (format  t  "2) ~A nodos expandidos. ~%" *expanded*)
    (format  t  "3) Longitud máxima de la Frontera de búsqueda: ~A.~%" *maxima-frontera*)
    (format  t  "4) Longitud de la solución: ~A operadores.~%" (1- (length  lista-nodos)))
    (format  t  "5) Tiempo: ~A segundos.~%~%" (float exeTime)) ;; Running time

    (let  ((nodo  nil))
        (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  Aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))
       
)  

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH                    These function are imported.
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================

(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  ())
     (setq  *memory*  ())
     (setq  *id*  -1)
     (setq  *current-ancestor*  nil)
     (setq  *expanded*  0)  
     (setq  *maxima-frontera*  0) 
     (setq  *solucion*  nil)
     (setq *time1* 0)
     (setq *time2* 0))

(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
      (setq *time1* (get-internal-run-time))
      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...

	   (cond    ((equal  edo-meta  estado)
                    (setq *time2* (get-internal-run-time))
		                (format  t  "Éxito. Meta encontrada ~%~%")
		                (display-solution  (extract-solution  nodo) (/ (- *time2* *time1*) internal-time-units-per-second))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...

			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )
     
;;;=======================================================================================
;;        Invoking the main function:
;;;=======================================================================================

(format  t  "~% Depth-first: ~%~%")
(blind-search '((t t t t) (nil nil nil nil)) '((nil nil nil nil) (t t t t)) :depth-first)

(format  t  "~%~% Breath-first: ~%~%")
(blind-search '((t t t t) (nil nil nil nil)) '((nil nil nil nil) (t t t t)) :breath-first)