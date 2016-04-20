;---------------------------------------------------------------------------
;  basic-manipulation-tests.clp - RoboCup-at-Work RefBox CLIPS BMTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass BasicManipulationTest1 (is-a TestScenario) (role concrete))

(defmessage-handler BasicManipulationTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*BMT-TIME*))
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
    (task-spec "BMT<S2,S2,S3,line(M20_100,F20_20_G,F20_20_B,S40_40_B,S40_40_G,R20,M30),EXIT>")
  )
  (assert (attention-message (text "BMT Task Spec Set")))

)

(defmessage-handler BasicManipulationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; BMT feedback is invalid -> finish the test
)


(defrule init-bmt
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [BMT1] of BasicManipulationTest1 (type BMT) (type-id 1) (description "Prepare Assembly Aid Tray"))

  (slot-insert$ ?bm registered-scenarios 1 [BMT1])
)
