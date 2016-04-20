;---------------------------------------------------------------------------
;  robocup-final-tests.clp - RoboCup-at-Work RefBox CLIPS RFTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass RobocupFinalTest1 (is-a TestScenario) (role concrete))

(defmessage-handler RobocupFinalTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*RFT-TIME*))
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
    (task-spec ?*RFT1*)
    ;(task-spec (str-cat
    ;"RFT<initialsituation("
    ;  "<S5,(R20,M30,S40_40_B)>"
    ;  "<S2,(S40_40_G,M20,R20)>"
    ;  "<S3,(F20_20_B,M20_100,F20_20_G)>"
    ;");"
    ;"goalsituation("
    ;  "<C1,line(M20_100,M30,M20)>"
    ;  "<S4,line(F20_20_G,R20,R20)>"
    ;  "<S1,line(S40_40_B,S40_40_G,F20_20_B)>"
    ; ")>"))
  )
  ;(assert (attention-message (text "RFT Task Set")))
)

(defmessage-handler RobocupFinalTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; RFT feedback is invalid -> finish the test
)


(defrule init-rft
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [RFT1] of RobocupFinalTest1 (type RFT) (type-id 1) (description "Robocup Final Test"))

  (slot-insert$ ?bm registered-scenarios 1 [RFT1])
)
