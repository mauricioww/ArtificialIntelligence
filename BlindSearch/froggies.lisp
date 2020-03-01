;;          =============        Solved by Marcos Mauricio Carpintero Mendoza    2017630231      ===============
;;                                            Puzzle: River Crossing

;;                              Initial state           |                   Final state
;;                              (g g g _ b b b)                            (b b b _ g g g)
;;          Explanation: In both states there are 7 symbols; green frogs (g) and brown frogs (b), which everyone is stand on a rock
;;                       in some lake, and the last symbol is an empty rock (_) .
;;               Rules: 1. Any frog can jump only to an empty rock.
;;                      2. Any frog can jump either one or two positions, it means over another frog.
;;                      3. The puzzle is finished when the frogs have croseed the river in the oppositive direction than in the beggining.


;;          =============       Some of the following code is imported from 'Misioneros-Canibales.lisp'.      ==============


(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos

;; The operators are described below:
;;      First: We have 7 rocks then the frogs can jump at any of them.
;;      Second: The esiest way to solve the puzzle is observe the rocks as an bidirection array for the two types of frogs.
;;              I mean, for the green frogs we can take it as a simple ascending array, but for the brown ones is a descending array.                
;;              All of that is for programming technique, now for human understanding we can describe it as follow.
(defparameter *operators*   '(  (:verde_roca[0]_salta    0)
                                (:verde_roca[1]_salta    1)
                                (:verde_roca[2]_salta    2)
                                (:verde_roca[3]_salta    3)
                                (:verde_roca[4]_salta    4)
                                (:verde_roca[5]_salta    5)
                                (:verde_roca[6]_salta    6)
                                (:cafe_roca[1]_salta    -1)
                                (:cafe_roca[2]_salta    -2)
                                (:cafe_roca[3]_salta    -3)
                                (:cafe_roca[4]_salta    -4)
                                (:cafe_roca[5]_salta    -5)
                                (:cafe_roca[6]_salta    -6)
                            ) )

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
;;  VALID-OPERATOR [op, estado]               This function is mine.
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun la disposicion de las rocas actuales
;;;=======================================================================================

(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."  
  (let*  
    (
      (id-operator        (second op))
      (new-position    (abs id-operator))
      (green-jump?       (<= 0 id-operator)) 
      (aux                  (if green-jump? 'g 'b)) 
      (empty-stone        (+ (if green-jump? 1 -1) new-position)) 
      (frog-over-frog   (+ (if green-jump? 2 -2) new-position))
      (valido-saltar-vacia (and (>= empty-stone 0)  (<= empty-stone 6)))
      (valido-saltar-otra  (and (>= frog-over-frog 0)  (<= frog-over-frog 6)))
    )
    (and
      (eql aux (nth new-position estado))
      (or valido-saltar-otra valido-saltar-vacia)
      (or (and valido-saltar-vacia (eql '_ (nth empty-stone estado))) (and valido-saltar-otra (eql '_ (nth frog-over-frog estado))) )
    ) 
  ) 
)

;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)               This function is mine.
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  
      (
        (new-state       (copy-list estado))
        (id-operator       (second op))
        (new-position   (abs id-operator))
        (green-jump?      (<= 0 id-operator)) 
        (aux                 (if green-jump? 'g 'b))
        (empty-stone       (+ (if green-jump? 1 -1) new-position)) 
        (frog-over-frog  (+ (if green-jump? 2 -2) new-position))
      )
      (setf (nth new-position new-state) '_) 
      (if (valid-operator? op estado)                 
        (if (eql '_ (nth empty-stone estado))          
          (setf (nth empty-stone new-state) aux)    
          (setf (nth frog-over-frog new-state) aux)  
        ) 
      )
      new-state 
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
		 (when (valid-operator?  op  estado)           ;; se valida el resultado...
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
;;  RESET-ALL  y  BLIND-SEARCH
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
(blind-search '(g g g _ b b b) '(b b b _ g g g) :depth-first)

(format  t  "~%~% Breath-first: ~%~%")
(blind-search '(g g g _ b b b) '(b b b _ g g g) :breath-first)