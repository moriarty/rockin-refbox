;---------------------------------------------------------------------------
;  conveyor-belt-tests.clp - RoboCup-at-Work RefBox CLIPS CBTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass ConveyorBeltTest1 (is-a TestScenario) (role concrete))
(defclass ConveyorBeltTest2 (is-a TestScenario) (role concrete))

(defmessage-handler ConveyorBeltTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*CBT-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [finished-state])
  )

  (modify-instance [task-specification] (task-spec "CBT<C1>"))

  (assert (attention-message (text "CBT Task Set")))
)

(defmessage-handler ConveyorBeltTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; CBT feedback is invalid -> finish the test
)

(defmessage-handler ConveyorBeltTest2 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*CBT-TIME*))
  (make-instance [paused-state] of PausedState
    (phase EXECUTION) (state-machine ?state-machine))
  (make-instance [finished-state] of FinishedState
    (phase EXECUTION) (state-machine ?state-machine))

  (send [stopped-state]    add-transition START           [running-state])
  (send [running-state]    add-transition STOP            [stopped-state])
  (send [running-state]    add-transition PAUSE           [paused-state])
  (send [running-state]    add-transition TIMEOUT         [finished-state])
  (send [running-state]    add-transition FINISH          [finished-state])
  (send [paused-state]     add-transition START           [running-state])
  (send [paused-state]     add-transition STOP            [stopped-state])

  (make-instance ?state-machine of StateMachine
    (current-state [stopped-state])
    (states [stopped-state] [running-state] [paused-state] [finished-state])
  )

  (modify-instance [task-specification] (task-spec "CBT<C2>"))

  (assert (attention-message (text "CBT Task Set")))
)

(defmessage-handler ConveyorBeltTest2 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; CBT feedback is invalid -> finish the test
)



(defrule init-cbt
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [CBT1] of ConveyorBeltTest1 (type CBT) (type-id 1) (description "cbt 1"))
  (make-instance [CBT2] of ConveyorBeltTest2 (type CBT) (type-id 2) (description "cbt 2"))

  (slot-insert$ ?bm registered-scenarios 1 [CBT1])
  (slot-insert$ ?bm registered-scenarios 1 [CBT2])
)
