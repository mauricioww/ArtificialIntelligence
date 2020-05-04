;;;                               TicTacToe 4x4      =========   negamax with alphabeta
;;;
;;;                                 Marcos Mauricio Carpintero Mendoza      2017630231


;;;En la pestaña debug siempre se imprimirá el tablero y la variable *output*
;; (print "Esta es una prueba para la pestaña debug")

;;; 		The order of moves according the posibility of wining the game
;;;			I mean, there are cells with more chance to win



(defparameter *tic*     (make-array '(3 3)))
(defparameter *rows*    3)
(defparameter *cols*    3)
(defparameter *IA*      '())
(defparameter *output*  0)
(defparameter *killer*  (make-hash-table :test 'equal))    ;; Storage for killer moves
;; ==========================================================================================
 
(defparameter *moves*
  '(
    ;; As you can see there the this sections are consecutive moves either cols or rows
    (0 1   2)
    (1 0   4)
    (1 2   6)
    (2 1   8)
    
    ;;  Now the corners of the board
    (0 0   1)
    (0 2   3)
    (2 0   7)
    (2 2   9)

    ;;  The center
    (1 1   5)
  )
)

(defun copy-array (oldArray) 
	"Returns a copy of the current board"
  (let*
    (
      ;; (dimensions     (array-dimensions   oldArray))
      ;; (rows           (first              dimensions))
      ;; (cols           (second             dimensions))
      (new-array      (make-array         '(3 3)))
    )
    (dotimes (x *rows*)
      (dotimes (y *cols*)
        (setf (aref new-array x y) (aref oldArray x y))
      )
    )
    new-array
  )
)

(defun print3x3(board)
    (format t "~%")
    (dotimes (x *rows*)
      (dotimes (y *cols*)
        (format t " ~A " (aref board x y))
      )
      (format t "~%")
    )
)

(defun copy-tic() 
	"Returns a copy of the current board"
  (let*
    (
      ;; (dimensions     (array-dimensions   oldArray))
      ;; (rows           (first              dimensions))
      ;; (cols           (second             dimensions))
      (new-array      (make-array         '(3 3)))
    )
    (dotimes (x *rows*)
      (dotimes (y *cols*)
        (setf (aref new-array x y) (aref *tic* x y))
      )
    )
    new-array
  )
)

(defun get-key(board)
  (let 
    ((key   '()))
    (dotimes (x *rows*)
      (dotimes (y *cols*)
        (push (aref board x y) key)
      )
    )
    (reverse key)
  )
)

;; (defun get-key(board)
;;   (let 
;;     ((key   '()))
;;     (dotimes (x 3)
;;       (dotimes (y 3)
;;         (push (aref board x y) key)
;;       )
;;     )
;;     (reverse key)
;;   )
;; )

(defun before-seen?(board)
  (not (null (gethash (get-key board) *killer*)))
)

;; 	It checks the score syms according to the player in order to evaluate the scenario
(defun count-sym-on-lines(f board sym)
	(let
    ((times 0) (final 0))
		(loop for x from 0 to 2 do
      (setq times 0)
			(loop for y from 0 to 2 do
				(if (null f) 
					(if (equal (aref board x y) sym) (incf times)) 
					(if (equal (aref board y x) sym) (incf times)) 
				)
			)
        ;; (setq final (+ final (expt times times)))   ;; check if is better expt 10 time
        (setq final (+ final (expt 10 times)))   ;; check if is better expt 10 time
		)
		final
	)
)

(defun count-sym-on-diag(f board sym)
	(let
		((times 0))
		(loop for i from 0 to 2 do
			(if (null f)
				(if (equal (aref board i i) sym) 		(incf times) )
				(if (equal (aref board i (- 2 i)) sym) 	(incf times) )
			)
		)
		;; (expt times times)  ;; check if is better expt 10 time
		(expt 10 times)  ;; check if is better expt 10 time
	)
)

;; It computes the numbers to eval the below function 
(defun score(board player)
	"Number of points according to the number of marks on either line or diag"
  (+  (count-sym-on-lines t     board player) 
      (count-sym-on-lines nil   board player)
      (count-sym-on-diag  t 	  board player)
      (count-sym-on-diag  nil   board player)
  )
)

;;  Eval the function  f(en) = G(a) - P(s)
(defun value-of (board)
  (- (score board 'O) (score board 'X))
)

(defun check-lines(f board sym)
	(let
		((flag t))
    (loop for x from 0 to 2 do
      (setq flag t)
      (loop for y from 0 to 2 do
        (if f
          (setq flag (and flag (equal (aref board x y) sym) ) )
          (setq flag (and flag (equal (aref board y x) sym) ) )				
        )
      )
      (if flag (return-from check-lines t))
    )
    nil
  )
)

;; It checks according to the player symbol
(defun check-diag(f board sym)
	(let
		((flag t))
		(loop for i from 0 to 2 do
			(if f
				(setq flag (and flag (equal (aref board i i) sym) ) )
				(setq flag (and flag (equal (aref board i (- 2 i)) sym) ) )
			)
		)
		flag
	)
)

(defun game-over-player (board player)
  (if (check-lines 	t	  board player) (return-from game-over-player t))
  (if (check-lines 	nil	board player) (return-from game-over-player t))
  (if (check-diag 	t 	board player) (return-from game-over-player t))
  (if (check-diag 	nil	board player) (return-from game-over-player t))
  nil
)

;; It just stops the game
(defun game-over (board)
  (or (game-over-player board 'O) (game-over-player board 'X))
)

;; The moves are sort according te posibility of winning
(defun get-moves(board computerPlaying?)
  "Apply all moves if is possible"
	(let
		(
			(sym 		      (if computerPlaying? 'O 'X))
			(list-moves   '())
			(new-board	  nil)
			(i 			      0)
			(j			      0)
			(pos 		      0)
		)
		(dolist (mov *moves* list-moves)
			(multiple-value-setq (i j pos) (values (first mov) (second mov) (third mov)))
			(cond ((null (aref board i j))
				(setq new-board (copy-array board))
				(setf (aref new-board i j) sym)
				(push (list new-board pos) list-moves)
			))
		)
	)
)

;; Search algorithm 
;; (defun negamax-alphabeta(board depth alpha beta maxi)
;;   (if (or (zerop depth) (game-over board)) 
;;           (return-from negamax-alphabeta 
;;             (if (> depth 0) (list (- 0 (value-of board)) nil) (list (value-of board) nil) )) ;; We must check de depth
;;   )
;;   (let
;;     (
;;       (new-board    nil)
;;       (best-move    nil)
;;       (move         0)
;;       (evaluation   0)
;;       (player       (if maxi nil t))
;;       (best-value   most-negative-fixnum)
;;     )
    
;;     (loop for board-state in (get-moves board maxi) do
;;       ;; (print board-state)
;;       (multiple-value-setq (new-board move) (values (first board-state) (second board-state)) )
;;       (setq evaluation 
;;           (first (negamax-alphabeta new-board (1- depth) (- 0 beta) (- 0 alpha) player)))
;;       (setq evaluation (- evaluation))
;;       (if (> evaluation best-value)
;;         (multiple-value-setq (best-value best-move) (values evaluation move))
;;       )
;;       (if (> best-value alpha) (setq alpha best-value))

;;       (if (>= best-value beta) (return-from negamax-alphabeta (list best-value best-move)))
;;     )
;;     (return-from negamax-alphabeta (list best-value best-move))
;;   )
;; )

(defun negamax-alphabeta(board depth alpha beta maxi)
  (if (or (zerop depth) (game-over board)) 
        (return-from negamax-alphabeta 
          (if (> depth 0) (list (- 0 (value-of board)) nil) (list (value-of board) nil) ) ;; We must check de depth
          
        ) ;; We must check de depth
          ;; (list (value-of board) nil) )
  )
  (let
    (
      (new-board    nil)
      (best-move    nil)
      (move         0)
      (evaluation   0)
      (player       (if maxi nil t))
      (best-value   most-negative-fixnum)
    )
    ;; (format t "~%    alpha=~D,  Beta=~d" alpha beta)
    (loop for board-state in (get-moves board maxi) do
      ;; (print board-state)
      (multiple-value-setq (new-board move) (values (first board-state) (second board-state)) )
      (setq evaluation 
          (first (negamax-alphabeta new-board (1- depth) (- 0 beta) (- 0 alpha) player)))
      (setq evaluation (- evaluation))
      (if (> evaluation best-value)
        (multiple-value-setq (best-value best-move) (values evaluation move))
      )
      ;; (format t "~%best value=~D at depth=~d" best-value depth)
      (if (> best-value alpha) (setq alpha best-value))

      ;; Trying to figure out this

      (if (> (* 2 alpha) beta)  
        (cond 
          ( (before-seen? new-board)      (format t "~% This board has seen before, then is killer move") )
          ;; ((not (before-seen? new-board)) (setf (gethash new-board *killer*) 'Listo) (format t "~%Agregado"))
          (t 
                                          (setf (gethash (get-key new-board) *killer*) 'Listo) (format t "~%Agregado")
                                          ;; (format t "~% New killer move  alpha=~D,  Beta=~d ~% new move   " alpha beta) 
                                          (print3x3 new-board)
          ) 
        )
        (progn (format t "~% Other available board ") (print3x3 new-board))
      )

      (if (>= best-value beta) (return-from negamax-alphabeta (list best-value best-move)))
    )
    (return-from negamax-alphabeta (list best-value best-move))
  )
)


;; (defun tictactoe (list-board)
;;   "Main function"
;;   (setq  *output* 
;;     (second  
;;         (negamax-alphabeta (list-to-array list-board) 2 most-negative-fixnum most-positive-fixnum t) ;; With a depth of 2 is enough
;;     )
;;   )
;; )

;;  Board
;; (1, 2, 3)
;; (4, 5, 6)
;; (7, 8, 9)

(defun insert-move(move symbol)
  (setq move (1- move))
  (let*
    (
      (x (floor (/ move 3)))
      (y (- move (* x 3)))
    )
    (setf (aref *tic* x y) symbol)
    (print3x3 *tic*)
  )
)

(defun set-move-machine()
  "Aux negamax-alphabeta"
  (let
    (
      (output 0)
      (inter-board *tic*)
    )
    (setq  output 
      (second (negamax-alphabeta *tic* 4 most-negative-fixnum most-positive-fixnum t))
    )
    (format t "~%Agente jugador tira en la posición: ~D ~%" output)
    (insert-move output 'o)
  )
)


;; (defparameter *length*  3)
;; (defparameter *pipe*    nil)

;; (defun print-row(board row flag)
;;   ;; (let
;;     ;; ((sym nil))
;;     (dotimes (i *length*)
;;       (setq *pipe* (if (< i 2) (char-code #\|)  (char-code #\ )))
;;       (format t "  ~A  ~A" (aref board row i) (code-char *pipe*))
;;     )
;;     (if flag (format t " ||"))
;;     (if (null flag) (format t "~%"))
;;     ;; (format t " ~A ~A ~A ~A ~A" '- '+ '- '+ '- #\  )
;;     ;; (format t "~%")
;;   ;; )
;; )

;; (defvar gatote (make-array '(3 3) :adjustable t :initial-contents nil))

;; (defvar array3 (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9) ) ) )
;; (defvar array2 (make-array '(3 3) :initial-contents '( (7 8 9) (4 5 6) (1 2 3) ) ) )

;; (defvar gatote (make-array '(3 3)))

;; gatote 

;; (print-row array1 0 t)
;; (print-row array2 0 nil)

;; (print-row array1 1 t)
;; (print-row array2 1 nil)

;; (print-row array1 2 t)
;; (print-row array2 2 nil)

;; (print3x3 *tic*)


(defun get-winner()
  "Print winner"
  (loop for player in '(o x) do
    (if (game-over-player *tic* player) (return-from get-winner 
        (format t "~%El jugador con simbolo '~A' ha ganado. ~%" player) ))
  )
)

;; It returns 
;;    true if there is some empty position in the board
;;    false if the board is full
;;    false if someone has won

(defun not-yet-finish()
  (cond ((game-over *tic*) 
    (get-winner)
    (print3x3 *tic*)
    (return-from not-yet-finish nil))
  )
  (let 
    ((flag nil))
    (dotimes (x *rows*)
      (dotimes (y *cols*)
        (setq flag (or (null (aref *tic* x y)) flag))
      )
    )
    (cond ((null flag) (format t "~% Empate! Nadie gana...") (print3x3 *tic*)))
    flag
  )
)

;; It checks if the position which the user input is available

(defun validate-move (move)
  (do ()
    (nil)
    (let* 
      (
        (x (floor (/ (1- move) 3))) 
        (y (- (1- move) (* x 3)))
      )
      (if (null (aref *tic* x y))
        (return-from validate-move move)   ;Process items until NIL seen.
        (progn 
          (format t "~% ERROR... en pos ~D esta ocupado por: ~A " move (aref *tic* x y))
          (format t "~% Ingrese posicion valida en el tablero: ")
          (setq move (read))
        )
      )
    )
  )
)

(defun tictactoe()
  "Main function"
  (do ()
    (nil)                       ;Do forever.
    (let ((move 0))
      (cond ((not-yet-finish)
                (format t "~% Ingrese posicion en el tablero: ")
                (setq move (read))
                (setq move (validate-move move))
                (insert-move move 'x)
            )
            (t (return)) ;Process items until NIL seen.
      )
      (if (not-yet-finish) (set-move-machine) (return)) ;; Machine can play?
    )
  )  
)

(tictactoe)