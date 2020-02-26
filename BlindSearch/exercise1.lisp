;                                  Solved by Marcos Mauricio Carpintero Mendoza - 2017630231

;;                                          Puzzle: River Crossing

;;                              Initial state           |                   Final state
;;                              (g g g _ b b b)                            (b b b _ g g g)
;;          Explanation: In both states there are 7 symbols; green frogs and brown frogs, which everyone is on a rock in some lake, 
;;                       and the last symbol is an empty rock.
;;               Rules: 1. Any frog can jump only to an empty rock.
;;                      2. Any frog can jump either one or two positions, it means over another frog.
;;                      3. The puzzle is finished when the frogs have croseed the river in the oppositive direction than in the beggining.

(defparameter *operators* '(
                                (:green_jump_0)




                            ) )