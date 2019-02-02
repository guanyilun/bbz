(import [hylab.core [*]])
(require [hylab.core [*]])
(import sys)

;; define the game rules
(setf *rules*
      ; (action  qi def reaction order name)      
      '((bor     -1   0   bor-ed     2  "b")
        (bbor    -4   0  bbor-ed     2 "bb")
        (zan      1   0       ()     2  "z")
        (defend   0  +1       ()     1  "d")
        (ddefend -2  +2       ()     1 "dd")        
        (bor-ed   0  -1       ()     0   ())
        (bbor-ed  0  -2       ()     0   ())))

;; define a macro to register player actions
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
                       (setf self.qi (+ self.qi ~qi))
                       (~(HySymbol (+ "." reaction)) player))))
              `(defun ~action (self)
                 (if (>= (+ self.qi ~qi) 0)
                     (progn
                       (setf
                         self.qi (+ self.qi ~qi)
                         self.def (+ self.def ~def)
                         self.life (- self.life (if (< self.def 0) 1 0))))))))))

(defclass Player [object]
  (defun --init-- (self name)
    (setf self.name name)        
    (setf self.life 1)
    (setf self.qi 0)
    (setf self.def 0))
  
  (defun get-action (self)
    (setf self.action (input #f"Action for {self.name}: ")))
  
  (defun stats (self)
    (print #f"=======================")    
    (print #f"Player: {self.name}")
    (print #f"--> Lives: {self.life}")
    (print #f"--> Zan: {self.qi}")
    (print #f"--> Zan: {self.def}"))

  (register-actions))

;; clojure notation
(defclass Game [object]
  (defn --init-- [self]
    (setv
      self.p1 (Player "1")
      self.p2 (Player "2")))
  
  (defn run [self]
    (setv p1 self.p1 p2 self.p2)
    (while True
      (self.stats)
      
      (.get-action p1)
      (.get-action p2)
      
      (self.resolve-action)
      
      (let ((e (.endgame? self)))
        (cond [(= e 1) (do (print #f"Player {(. self p2 name)} wins!")
                           (.exit sys 0))]
              [(= e 2) (do (print #f"Player {(. self p1 name)} wins!")
                           (.exit sys 0))]))))
  
  (defn stats [self]
    (for [p (, self.p1 self.p2)] (.stats p)))
  
  (defn endgame? [self]
    (cond
      [(= (. self p1 life) 0) 1]
      [(= (. self p2 life) 0) 2]
      [True 0]))
  
  (defn resolve-action [self]
    (for [p (, self.p1 self.p2)]
      (if (= p.action "d") (p.defend))
      (if (= p.action "z") (p.zan))
      (if (= p.action "e") (p.add-life)))
    (if (= self.p1.action "b")
        (.bor self.p1 self.p2))
    (if (= self.p2.action "b")
        (.bor self.p2 self.p1))
    (if (= self.p1.action "bb")
        (.bbor self.p1 self.p2))
    (if (= self.p2.action "bb")
        (.bbor self.p2 self.p1))))

(.run (Game))
