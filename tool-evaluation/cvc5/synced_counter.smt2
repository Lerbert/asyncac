(set-logic UFDTNIA)

(set-option :incremental true)
(set-option :produce-unsat-cores true)
(set-option :print-unsat-cores-full true)

; Lists

(declare-datatypes ((List 1))
    ((par (T) ((cons (head T) (tail (List T))) (nil))))
)

; Basic Types

(declare-datatype Event (
    (start)
    (inc)
    (ack)
))

(declare-datatype Port (
    (userCom)
    (xCom)
    (yCom)
))

(declare-datatype Message (
    (msg (portMsg Port) (evtMsg Event))
))

(declare-datatype XCtrl (
    (Stable)
    (WaitAck)
))

(declare-datatype XConf (
    (xConf (ctrl XCtrl) (xCnt Int))
))

(declare-datatype YCtrl (
    (Counting)
))

(declare-datatype YConf (
    (yConf (ctrl YCtrl) (yCnt Int))
))

; Simple

(define-sort MList () (List Message))

(define-fun oel ((x Message)) MList
    (cons x (as nil MList))
)

(define-fun-rec length ((l MList)) Int
    (match l (
        (nil 0)
        ((cons x xs) (+ 1 (length xs)))
    ))
)

(define-fun is_empty ((l MList)) Bool
    (match l (
        (nil true)
        ((cons x xs) false)
    ))
)

(define-fun-rec contains ((x Message) (l MList)) Bool
    (match l (
        (nil false)
        ((cons y ys) (or (= x y) (contains x ys)))
    ))
)

(define-fun-rec enqueue ((x Message) (l MList)) MList
    (match l (
        (nil (oel x))
        ((cons y ys) (cons y (enqueue x ys)))
    ))
)

(declare-datatype Conf (
    (conf (cxConf XConf) (qxConf MList) (cyConf YConf) (qyConf MList))
))

(define-fun x_init ((cx XConf)) Bool
    (and (= (ctrl cx) Stable) (= (xCnt cx) 0))
)

(define-fun y_init ((cy YConf)) Bool
    (and (= (ctrl cy) Counting) (= (yCnt cy) 0))
)

(define-fun init ((g Conf)) Bool
    (and (x_init (cxConf g)) (y_init (cyConf g)) (is_empty (qxConf g)) (is_empty (qyConf g)))
)

(define-fun x_trans ((cx1 XConf) (input Message) (output MList) (cx2 XConf)) Bool
    (or
        (and (= (ctrl cx1) Stable) (= input (msg userCom start)) (= output (oel (msg yCom inc))) (= (xCnt cx2) (+ (xCnt cx1) 1)) (= (ctrl cx2) WaitAck))
        (and (= (ctrl cx1) WaitAck) (= input (msg yCom ack)) (is_empty output) (= (xCnt cx2) (xCnt cx1)) (= (ctrl cx2) Stable))
    )
)

(define-fun y_trans ((cy1 YConf) (input Message) (output MList) (cy2 YConf)) Bool
    (or
        (and (= (ctrl cy1) Counting) (= input (msg xCom inc)) (= output (oel (msg xCom ack))) (= (yCnt cy2) (+ (yCnt cy1) 1)) (= (ctrl cy2) Counting))
    )
)

(define-fun-rec dist ((to_dist MList) (qx1 MList) (qx2 MList) (qy1 MList) (qy2 MList)) Bool
    (match to_dist (
        (nil (and (= qx1 qx2) (= qy1 qy2)))
        ((cons m output) (match (portMsg m) (
            (userCom (dist output qx1 qx2 qy1 qy2))
            (yCom (dist output qx1 qx2 (enqueue (msg xCom (evtMsg m)) qy1) qy2))
            (xCom (dist output (enqueue (msg yCom (evtMsg m)) qx1) qx2 qy1 qy2))
        )))
    ))
)

(declare-datatype ConsumeResult (
    (Fail)
    (X)
    (Y)
    (External)
))

(define-fun consume ((input Message) (qx MList) (qy MList)) ConsumeResult
    (match (portMsg input) (
        (userCom External)
        (yCom (match qx (
            (nil Fail)
            ((cons m ms) (ite (= m input) X Fail))
        )))
        (xCom (match qy (
            (nil Fail)
            ((cons m ms) (ite (= m input) Y Fail))
        )))
    ))
)

(define-fun trans ((g1 Conf) (input Message) (output MList) (g2 Conf)) Bool
    (and 
        (or 
            (and (x_trans (cxConf g1) input output (cxConf g2)) (= (cyConf g1) (cyConf g2)))
            (and (y_trans (cyConf g1) input output (cyConf g2)) (= (cxConf g1) (cxConf g2)))
        )
        (match (consume input (qxConf g1) (qyConf g1)) (
            (Fail false)
            (External (dist output (qxConf g1) (qxConf g2) (qyConf g1) (qyConf g2)))
            (X (dist output (tail (qxConf g1)) (qxConf g2) (qyConf g1) (qyConf g2)))
            (Y (dist output (qxConf g1) (qxConf g2) (tail (qyConf g1)) (qyConf g2)))
        ))
    )
)

(define-fun safe ((g Conf)) Bool
    (=> (= (ctrl (cxConf g)) Stable) (= (xCnt (cxConf g)) (yCnt (cyConf g))))
)

(define-fun invar ((g Conf)) Bool
    (let ((ctrlX (ctrl (cxConf g))) (ctrlY (ctrl (cyConf g))) (qx (qxConf g)) (qy (qyConf g)) (cntX (xCnt (cxConf g))) (cntY (yCnt (cyConf g))))
        (or
            (and (= ctrlX Stable) (= ctrlY Counting) (is_empty qx) (is_empty qy) (= cntX cntY))
            (and (= ctrlX WaitAck) (= ctrlY Counting) (is_empty qx) (= (length qy) 1) (= cntX (+ cntY 1)))
            (and (= ctrlX WaitAck) (= ctrlY Counting) (= (length qx) 1) (is_empty qy) (= cntX cntY))
        )
    )
)

(echo "Checking consistency (unknown or sat should be fine)")
(check-sat)

; (echo "Checking lemmas")
; (push)
;     (assert (exists ((l MList)) (! 
;         (not (ite (is_empty l) (= (length l) 0) (> (length l) 0)))
;         :pattern ((is_empty l))
;         :pattern ((length l))
;     )))
;     (check-sat) ; This gets the solver stuck
; (pop)

; add proved lemmas as asserts

(assert (forall ((l MList)) (! 
    (ite (is_empty l) (= (length l) 0) (> (length l) 0))
    :pattern ((is_empty l))
    :pattern ((length l))
)))

; proof of safe

(push)
    (echo "invar-safe")
    (assert (not (forall ((g Conf))
        (=> (invar g) (safe g))
    )))
    (check-sat)
(pop)

(push)
    (echo "init-invar")
    (assert (not (forall ((g Conf))
        (=> (init g) (invar g))
    )))
    (check-sat)
(pop)

(push)
    (echo "invar-trans")
    (assert (not (forall ((g1 Conf) (input Message) (output MList) (g2 Conf))
        (=> (and (invar g1) (trans g1 input output g2)) (invar g2))
    )))
    (check-sat)
(pop)

(push)
    (echo "trans1")
    (assert (not (forall ((g1 Conf) (g2 Conf))
        (=>
            (match g1 (((conf cx qx cy qy)
                (and
                    (invar g1)
                    (= (ctrl cx) Stable)
                    (= g2 (conf (xConf WaitAck (+ (xCnt cx) 1)) qx cy (enqueue (msg xCom inc) qy)))
            ))))
            (invar g2)
        )
    )))
    (check-sat)
(pop)

(push)
    (echo "trans2")
    (assert (not (forall ((g1 Conf) (g2 Conf))
        (=>
            (match g1 (((conf cx qx cy qy)
                (and
                    (invar g1)
                    (= (ctrl cx) WaitAck)
                    (not (is_empty qx)) (= (head qx) (msg yCom ack))
                    (= g2 (conf (xConf Stable (xCnt cx)) (tail qx) cy qy))
            ))))
            (invar g2)
        )
    )))
    (check-sat)
(pop)

(push)
    (echo "trans3")
    (assert (not (forall ((g1 Conf) (g2 Conf))
        (=>
            (match g1 (((conf cx qx cy qy)
                (and
                    (invar g1)
                    (= (ctrl cy) Counting)
                    (not (is_empty qy)) (= (head qy) (msg xCom inc))
                    (= g2 (conf cx (enqueue (msg yCom ack) qx) (yConf Counting (+ (yCnt cy) 1)) (tail qy)))
            ))))
            (invar g2)
        )
    )))
    (check-sat)
(pop)

; trans examples

(push)
    (echo "trans-ex1")
    (assert (not (forall ((cy YConf)) (trans 
        (conf (xConf Stable 2) (as nil MList) cy (as nil MList))
        (msg userCom start)
        (oel (msg yCom inc))
        (conf (xConf WaitAck 3) (as nil MList) cy (oel (msg xCom inc)))
    ))))
    (check-sat)
(pop)

(push)
    (echo "trans-ex2")
    (assert (not (forall ((cy YConf)) (trans 
        (conf (xConf WaitAck 2) (oel (msg yCom ack)) cy (as nil MList))
        (msg yCom ack)
        (as nil MList)
        (conf (xConf Stable 2) (as nil MList) cy (as nil MList))
    ))))
    (check-sat)
(pop)

(push)
    (echo "trans-ex3")
    (assert (not (forall ((cy YConf)) (not (trans 
        (conf (xConf WaitAck 2) (oel (msg yCom inc)) cy (as nil MList))
        (msg yCom inc)
        (as nil MList)
        (conf (xConf Stable 2) (as nil MList) cy (as nil MList))
    )))))
    (check-sat)
(pop)

(push)
    (echo "trans-ex4")
    (assert (not (forall ((cy YConf)) (not (trans 
        (conf (xConf WaitAck 2) (as nil MList) cy (as nil MList))
        (msg yCom ack)
        (as nil MList)
        (conf (xConf Stable 2) (as nil MList) cy (as nil MList))
    )))))
    (check-sat)
(pop)
