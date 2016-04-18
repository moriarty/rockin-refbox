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


  (bind ?shelf-locations (create$ [shelf-01] [shelf-02] [shelf-03] [shelf-04]
        [shelf-05] [shelf-06] [shelf-07] [shelf-08] [shelf-09] [shelf-10]
        [shelf-11] [shelf-12]))

  ; Randomize a location for EM-01-01
  (bind ?em-01-01-location (pick-random$ ?shelf-locations))

  ; The location of EM-01-01 should should not be reused
  (bind ?shelf-locations (delete-member$ ?shelf-locations ?em-01-01-location))

  ; Inventory
  (slot-insert$ [inventory] items 1
    ;;;;;;;;;;;;;;;;;;;;
    ; Assembly aid trays
    ;;;;;;;;;;;;;;;;;;;;

    ; Assembly aid tray EM-01-01 at random location
    (make-instance of Item (object-id [em-01-01]) (location-id ?em-01-01-location))

    ;;;;;;;;;;;;;;;
    ; Bearing boxes
    ;;;;;;;;;;;;;;;

    ; 1 bearing box at random location
    (make-instance of Item (object-id [ax-01]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))

    ; 1 bearing box at random location
    (make-instance of Item (object-id [ax-01]) (location-id (pick-random$ ?shelf-locations)) (quantity 1))
  )


  ; Orders
  (slot-insert$ [order-info] orders 1
    ; Deliver 2 items of AX-01 to EM-01-01
    (make-instance of Order (status OFFERED) (object-id [ax-01]) (container-id [em-01-01]) (quantity-requested 2))

    ; Deliver container EM-01-01 to location WS-03
    (make-instance of Order (status OFFERED) (object-id [em-01-01]) (destination-id [workstation-03]))
  )
)

(defmessage-handler PrecisionPlacementTest1 handle-feedback (?pb-msg ?time ?name ?team)
  (if (and
       (pb-has-field ?pb-msg "assembly_aid_tray_id")
       (pb-has-field ?pb-msg "container_id"))
   then
    (return CONTINUE)   ; PPT feedback is valid -> continue the test
   else
    (return FINISH)     ; PPT feedback is invalid -> finish the test
  )
)


(defrule init-ptt
  (init)
  ?bm <- (object (is-a Test))
  =>
  (make-instance [PPT1] of PrecisionPlacementTest1 (type PPT) (type-id 1) (description "ppt 1"))

  (slot-insert$ ?bm registered-scenarios 1 [PPT1])
)
