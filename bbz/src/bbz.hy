(import [hylab.core [*]])
(require [hylab.core [*]])
(import sys)

;; common lisp notation
(defclass Player [object]
    (defun --init-- (self name)
        (setf self.name name)        
        (setf self.live 1)
        (setf self.nzan 0)
        (setf self.defend? False))
    
    (defun bor (self player)
        (if (> self.nzan 0)
            (progn
                (decf self.nzan)
                (.bor-ed player))))
    
    (defun bbor (self player)
        "stronger bor, ignore defend, require nzan = 4"
        (if (>= self.nzan 4)
            (progn
                (setf self.nzan (- self.nzan 4))
                (.bbor-ed player))))

    (defun add-life (self)
        (if (>= self.nzan 4)
            (progn
                (setf self.nzan (- self.nzan 4))
                (incf self.live))))
    
    (defun zan (self)
        (incf self.nzan))

    (defun defend (self)
        (setf self.defend? True))
    
    (defun bor-ed (self)
        (if self.defend?
            (setf self.defend? False)
            (decf self.live)))
    
    (defun bbor-ed (self)
        (decf self.live))
    
    (defun get-action (self)
        (setf self.action (input #f"Action for {self.name}: ")))
    
    (defun stats (self)
        (print #f"Player: {self.name}")
        (print #f"--> Lives: {self.live}")
        (print #f"--> Zan: {self.nzan}")))

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
            [(= (. self p1 live) 0) 1]
            [(= (. self p2 live) 0) 2]
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
