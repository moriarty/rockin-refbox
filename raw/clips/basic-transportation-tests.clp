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
)

(defmessage-handler BasicTransportationTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "assembly_aid_tray_id")
       (pb-has-field ?pb-msg "container_id"))
   then
    (return CONTINUE)   ; BTT feedback is valid -> continue the test
   else
    (return FINISH)     ; BTT feedback is invalid -> finish the test
  )
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
