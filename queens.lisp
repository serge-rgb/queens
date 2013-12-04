;; Eight queens solver.
;; - Sergio Gonzalez 2013.

(declaim (optimize (speed 3) (space 0) (debug 0)))

(defvar *num-populations* 1000)
(defvar *board-size* 8)

(defun gen-population ()
  " Population: a list of *board-size* (x, y) cartesian coordinates."
  (loop for i from 1 to *board-size* collect
       (list (random *board-size*) (random *board-size*))))

(defun print-population (pop)
  "Print a grid representing a solution"
  (print "* 0 1 2 3 4 5 6 7")
  (loop for y from 0 to (- *board-size* 1) do
       (let ((line ""))
         (setq line (concatenate 'string (write-to-string y) " "))
         (loop for x from 0 to (- *board-size* 1) do
              (setq line (concatenate 'string line (if (find (list x y) pop :test
                                                             #'(lambda (a b)
                                                                 (and (= (first a) (first b))
                                                                      (= (second a) (second b)))))
                                                       "* "
                                                       "_ "))))
         (print line))))

(defun individual-fitness (queen queens)
  "Lower is better. 0 is impossible because everyone is penalized for being equal to itself."
  (let ((score 0)
        (x (first queen))
        (y (second queen)))
    (loop for other in queens do
         (let ((x_o (first other))
               (y_o (second other)))
           (cond
             ;; Penalization for being in the same spot.
             ((and (= x x_o) (= y y_o)) (setq score (+ score 100)))
             ;; Horizontal
             ((= x x_o) (setq score (+ score 1)))
             ;; Vertical
             ((= y y_o) (setq score (+ score 1)))
             ;; Diagonal
             ((let ((diff_x (abs (- x x_o)))
                    (diff_y (abs (- y y_o))))
                (and (= diff_x diff_y) (> diff_x 0))) (setq score (+ score 1))))))

    score))

(defun fitness (queens)
  (let ((score 0))
    (map nil #'(lambda (queen) (setq score (+ score (individual-fitness queen queens)))) queens)
    score))

(defun sorted-pops (evaled-pops)
  (sort evaled-pops #'(lambda (a b)
                        (let ((s1 (first a))
                              (s2 (first b)))
                          (if (<= s1 s2)
                              1)))))

(defun evaled-pops (pops)
  "Returns (score (queen1, queen2, ..., queen8))"
  (mapcar 'list (mapcar #'fitness pops) pops))

(defun wiggle () (- (random (floor (/ *board-size* 2))) 1))
(defun mutate-pop (pop)
  "Take the queen with greatest individual-fitness and wiggle it"
  (let* (;; Get fitness for each element of pop
         (individual-fitness (mapcar #'individual-fitness pop (make-list (length pop) :initial-element pop)))
         ;; zip fitness to pop
         (scored (mapcar #'list individual-fitness pop))
         ;; sort pop by fitness
         (sorted (mapcar #'second (sort scored #'(lambda (a b)
                                  (let ((sa (first a))
                                        (sb (first b)))
                                    (if (> sa sb) 1))))))
         ;; get the worst element
         (candidate (first sorted))
         ;; wiggle it
         (changed (list (+ (first candidate) (wiggle))
                        (+ (second candidate) (wiggle)))))
    ;; Abort mission if going out of the board
    (if (or (< (first changed) 0)
            (< (second changed) 0)
            (> (first changed) 7)
            (> (second changed) 7)) (setq changed candidate))
    (cons changed (rest sorted))))

(defun sort-by-fitness (pops)
  (let* ((evaled-pops (funcall #'evaled-pops pops))
         (sorted-pops (funcall #'sorted-pops evaled-pops))
         (stripped (mapcar #'second sorted-pops)))
    stripped))

(defun new-generation (pops)
  (let* (
         (stripped (sort-by-fitness pops))
         (children (sort-by-fitness (mapcar #'mutate-pop (copy-list stripped))))
         (reversed (reverse stripped))
         (new-gen (mapcar #'(lambda (dad child)
                              (if (< (fitness child) (fitness dad))
                                  child
                                  dad)) reversed children)))
    new-gen))

(time (let ((pops (loop for i from 1 to *num-populations* collect (gen-population)))
      (min 10000))
  (loop for i from 0 do
       (setq pops (new-generation pops))
       (let* ((best (first (sort-by-fitness pops)))
              (f (fitness best)))
         (if (< f min)
             (progn
               (setq min f)
               (print-population best)
               (print min)))
         (if (= f (* 100 *board-size*)) (return))))
  (print (list "===== done ===="))))
