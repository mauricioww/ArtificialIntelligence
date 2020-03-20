(defun and-operator (config n)
    "Returns boolean according the config allows the movement"
    (= (logand config n) n)
)


(defun flippea (b)
    (boole BOOLE-XOR b 1)
)

(defun get-levell(conf level args)
;; 12 6 9 5 3
    ;; (if (or (and-operator conf 3) (and-operator conf 5) (and-operator conf 6) (and-operator conf 9) (and-operator conf 12))
    ;; ;; (if (apply 'or (mapcar #'and-operator list (list conf)))
    ;;     (flippea level)
    ;;     (return-from get-level level)
    ;; )
    ;; (setq args (mapcar #'list args))
    (if (mapcar (lambda (n args) (funcall #'and-operator n (car args)     ) ) (make-list (length args) :initial-element conf) (mapcar #'list args))
        (flippea level)
        level
    )
    
    ;; (mapcar (lambda (a b)    (funcall #'toy-fnx      a (car b)        ) ) (make-list (length b) :initial-element a      ) b   )


    ;; (mapcar (lambda (a    b) (funcall #'toy-fnx      a (car b) (cdr b)) ) (make-list (length b) :initial-element a) b)
)

;; (trace get-levell)
(defparameter level 0)
(setq level (get-levell 5 0 '(1 3 5 7)))
(print level)

(defvar packages '(("complete" . "complete")
                   ("defunkt" . "markdown-mode")))

(defparameter *simple* "a")

(defun toy-fnx (c author )
  "Just testing."
  (format t "constant: ~a and var Package ~a by - " c  author)
  )

(defun func (a b)
(mapcar (lambda (a b) (funcall #'toy-fnx a (car b) ) ) (make-list (length b) :initial-element a) b)
)

;; (func *simple* packages)