;;;                               TicTacToe 4x4      =========   negamax with alphabeta
;;;
;;;                                 Marcos Mauricio Carpintero Mendoza      2017630231


;;;En la pestaña debug siempre se imprimirá el tablero y la variable *output*
;; (print "Esta es una prueba para la pestaña debug")

;;; 		The order of moves according the posibility of wining the game
;;;			I mean, there are cells with more chance to win

(defparameter *moves*
  '(
    ;; As you can see there the this sections are consecutive moves either cols or rows
    (0 1   2)
    (0 2   3)
    (1 0   5)
    (2 0   9)
    (1 3   8)
    (2 3  12)
    (3 1  14)
    (3 2  15)
    
    ;;  Now the corners of the board
    (0 0   1)
    (0 3   4)
    (3 0  13)
    (3 3  16)

    ;;  The center in this case is 4-cell-size
    (1 1   6)
    (1 2   7)
    (2 1  10)
    (2 2  11)
  )
)

(defun copy-array (oldArray) 
	"Returns a copy of the current board"
  (let*
    (
      (dimensions     (array-dimensions   oldArray))
      (rows           (first              dimensions))
      (cols           (second             dimensions))
      (new-array      (make-array         dimensions))
    )
    (dotimes (x rows)
      (dotimes (y cols)
        (setf (aref new-array x y) (aref oldArray x y))
      )
    )
    new-array
  )
)

(defun list-to-array(listBoard)
  "Copy of the current board"
  (make-array '(4 4) :initial-contents listBoard)
)

;; 	It checks the score syms according to the player in order to evaluate the scenario
(defun count-sym-on-lines(f board sym)
	(let
    ((times 0) (final 0))
		(loop for x from 0 to 3 do
      (setq times 0)
			(loop for y from 0 to 3 do
				(if (null f) 
					(if (equal (aref board x y) sym) (incf times)) 
					(if (equal (aref board y x) sym) (incf times)) 
				)
			)
        (setq final (+ final (expt times times)))   ;; check if is better expt 10 time
		)
		final
	)
)

(defun count-sym-on-diag(f board sym)
	(let
		((times 0))
		(loop for i from 0 to 3 do
			(if (null f)
				(if (equal (aref board i i) sym) 		(incf times) )
				(if (equal (aref board i (- 3 i)) sym) 	(incf times) )
			)
		)
		(expt times times)  ;; check if is better expt 10 time
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
    (loop for x from 0 to 3 do
      (setq flag t)
      (loop for y from 0 to 3 do
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
		(loop for i from 0 to 3 do
			(if f
				(setq flag (and flag (equal (aref board i i) sym) ) )
				(setq flag (and flag (equal (aref board i (- 3 i)) sym) ) )
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
(defun negamax-alphabeta(board depth alpha beta maxi)
  (if (or (zerop depth) (game-over board)) 
          (return-from negamax-alphabeta 
            (if (> depth 0) (list (- 0 (value-of board)) nil) (list (value-of board) nil) )) ;; We must check de depth
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
    
    (loop for board-state in (get-moves board maxi) do
      (print board-state)
      (multiple-value-setq (new-board move) (values (first board-state) (second board-state)) )
      (setq evaluation 
          (first (negamax-alphabeta new-board (1- depth) (- 0 beta) (- 0 alpha) player)))
      (setq evaluation (- evaluation))
      (if (> evaluation best-value)
        (multiple-value-setq (best-value best-move) (values evaluation move))
      )
      (if (> best-value alpha) (setq alpha best-value))

      (if (>= best-value beta) (return-from negamax-alphabeta (list best-value best-move)))
    )
    (return-from negamax-alphabeta (list best-value best-move))
  )
)

(defun tictactoe (list-board)
  "Main function"
  (setq  *output* 
    (second  
        (negamax-alphabeta (list-to-array list-board) 2 most-negative-fixnum most-positive-fixnum t) ;; With a depth of 2 is enough
    )
  )
)