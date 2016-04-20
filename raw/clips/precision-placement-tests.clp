;---------------------------------------------------------------------------
;  precision-placement-tests.clp - RoboCup-at-Work RefBox CLIPS PPTs
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass PrecisionPlacementTest1 (is-a TestScenario) (role concrete))

(defmessage-handler PrecisionPlacementTest1 setup (?time ?state-machine)
  (make-instance [stopped-state] of StoppedState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time))
  (make-instance [running-state] of RunningState
    (phase EXECUTION) (state-machine ?state-machine) (time ?time) (max-time ?*PPT-TIME*))
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
    (task-spec ?*PPT1*)
    ;(task-spec "PPT<S3,(S40_40_B,M20,F20_20_B),S4>")
  )
  ;(assert (attention-message (text "PPT Task Spec Set")))
)

(defmessage-handler PrecisionPlacementTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (return FINISH)     ; PPT feedback is invalid -> finish the test
)


(defrule init-ptt
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [PPT1] of PrecisionPlacementTest1 (type PPT) (type-id 1) (description "ppt 1"))

  (slot-insert$ ?bm registered-scenarios 1 [PPT1])
)
