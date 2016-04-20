;---------------------------------------------------------------------------
;  basic-transportation-tests.clp - RoboCup-at-Work RefBox CLIPS BTTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicTransportationTest1 (is-a TestScenario) (role concrete))
(defclass BasicTransportationTest2 (is-a TestScenario) (role concrete))
(defclass BasicTransportationTest3 (is-a TestScenario) (role concrete))

(defmessage-handler BasicTransportationTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT-TIME*))
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

  (modify-instance [task-specification]
    (task-spec (str-cat
      "BTT<initialsituation("
        "<S5,(R20,M30,S40_40_B)>"
        "<S2,(S40_40_G,M20,R20)>"
        "<S3,(F20_20_B,M20_100,F20_20_G)>"
      ");goalsituation("
        "<C1,line(M20_100,M30,M20)>"
        "<S4,line(F20_20_G,R20,R20)>"
        "<S1,line(S40_40_B,S40_40_G,F20_20_B)>)>"
  )))

  (assert (attention-message (text "BTT Task Spec Set")))
)

(defmessage-handler BasicTransportationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; BTT feedback is invalid -> finish the test
)


(defmessage-handler BasicTransportationTest2 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT-TIME*))
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

  (modify-instance [task-specification]
    (task-spec (str-cat
      "BTT<initialsituation("
        "<S3,(S40_40_B,F20_20_B,F20_20_B,M20)>"
      ");goalsituation("
        "<S1,line(S40_40_B,F20_20_B,F20_20_B,M20)>)>"
  )))

  (assert (attention-message (text "BTT Task Spec Set")))
)

(defmessage-handler BasicTransportationTest2 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; BTT feedback is invalid -> finish the test
)

(defmessage-handler BasicTransportationTest3 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BTT-TIME*))
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
  (modify-instance [task-specification]
    (task-spec (str-cat
      "BTT<initialsituation("
        "<S3,(S40_40_B,F20_20_B,F20_20_B,M20)>"
      ");goalsituation("
        "<S1,line(S40_40_B,F20_20_B,F20_20_B,M20)>)>"
  )))

  (assert (attention-message (text "BTT Task Spec Set")))

)

(defmessage-handler BasicTransportationTest3 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; BTT feedback is invalid -> finish the test
)


(defrule init-btt
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [BTT1] of BasicTransportationTest1 (type BTT) (type-id 1) (description "BTT 1"))
  (make-instance [BTT2] of BasicTransportationTest2 (type BTT) (type-id 2) (description "BTT 2"))
  (make-instance [BTT3] of BasicTransportationTest3 (type BTT) (type-id 3) (description "BTT 3"))

  (slot-insert$ ?bm registered-scenarios 1 [BTT1])
  (slot-insert$ ?bm registered-scenarios 1 [BTT2])
  (slot-insert$ ?bm registered-scenarios 1 [BTT3])
)
