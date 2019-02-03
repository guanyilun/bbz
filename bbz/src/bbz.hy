(import [hylab.core [*]])
(require [hylab.core [*]])
(import sys)


;; define a macro to register player actions
(eval-and-compile
  ;; define the game rules

  (defparameter *rules*
    ; (action  qi def reaction order name)
    '((bor     -1   0   bor-ed     2  "b")
      (bbor    -4   0  bbor-ed     2 "bb")
      (zan     +1   0       ()     1  "z")
      (defend   0  +1       ()     1  "d")
      (ddefend -2  +2       ()     1 "dd")        
      (bor-ed   0  -1       ()     0   ())
      (bbor-ed  0  -2       ()     0   ())))

  (defmacro register-actions ()
    (lfor rule *rules*
          (let ((action (get rule 0))
                (qi (get rule 1))
                (def (get rule 2))
                (reaction (get rule 3)))
            (if reaction
                `(defun ~action (self player)
                   (if (>= (+ self.qi ~qi) 0)
                       (progn
                         (incf self.qi ~qi)
                         (~#. reaction player))))
                `(defun ~action (self)
                   (if (>= (+ self.qi ~qi) 0)
                       (progn
                         (setf
                           self.qi (+ self.qi ~qi)
                           self.def (+ self.def ~def)
                           self.life (- self.life (if (< self.def 0) 1 0))))))))))

  (defmacro resolve-actions ()
    (+ (lfor rule (filter (λ (x) (= (get x -2) 1)) *rules*)
             `(for [p (, self.p1 self.p2)]
                (if (= p.action ~(last rule))
                    (~#. (first rule) p))))
       (lfor rule (filter (λ (x) (= (get x -2) 2)) *rules*)
             `(progn (if (= self.p1.action ~(last rule))
                         (~#. (first rule) self.p1 self.p2))
                     (if (= self.p2.action ~(last rule))
                         (~#. (first rule) self.p2 self.p1)))))))


(defclass Player [object]
  (defun --init-- (self name)
    (setf self.name name)        
    (setf self.life 2)
    (setf self.qi 0)
    (setf self.def 0))
  
  (defun get-action (self)
    (setf self.action (input #f"Action for {self.name}: ")))

  (defun reset (self)
    (setf self.def 0))
  
  (defun stats (self)
    (print #f"=======================")    
    (print #f"Player: {self.name}")
    (print #f"--> Lives: {self.life}")
    (print #f"--> Zan: {self.qi}")
    (print #f"--> Def: {self.def}"))

  (register-actions))

(defclass Game [object]
  (defun --init-- (self)
    (setf
      self.p1 (Player "1")
      self.p2 (Player "2")))
  
  (defun run (self)
    (setf p1 self.p1
          p2 self.p2)
    
    (while True
      (.reset p1)
      (.reset p2)
      
      (.get-action p1)
      (.get-action p2)
      
      (resolve-actions)

      (self.stats)
      
      (let ((e (.endgame? self)))
        (cond [(= e 1) (do (print #f"Player {(. self p2 name)} wins!")
                           (.exit sys 0))]
              [(= e 2) (do (print #f"Player {(. self p1 name)} wins!")
                           (.exit sys 0))]))))
  
  (defun stats (self)
    (for [p (, self.p1 self.p2)] (.stats p)))
  
  (defun endgame? (self)
    (cond
      [(= (. self p1 life) 0) 1]
      [(= (. self p2 life) 0) 2]
      [True 0])))

(.run (Game))
