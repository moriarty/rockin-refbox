;---------------------------------------------------------------------------
;  test.clp - RoboCup-at-Work RefBox CLIPS test
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass TestScenario (is-a USER)
  ; NONE: No test running
  ; BNT:  Basic Navigation Test
  ; BMT:  Basic Manipulation Test
  ; BTT:  Basic Transportation Test
  ; PPT:  Percision Placement Test
  ; CBT:  Conveyor Belt Test
  ; RFT:  Robocup Final Test
  (slot type (type SYMBOL) (allowed-values NONE BNT BMT BTT PPT CBT RFT) (default NONE))
  (slot type-id (type INTEGER) (default 0))
  (slot description (type STRING) (default ""))
)

(defmessage-handler TestScenario create-msg ()
  "Create a ProtoBuf message for a test scenario description"

  (bind ?pb-test-scenario (pb-create "raw_msgs.TestScenario"))
 
  (pb-set-field ?pb-test-scenario "type" ?self:type)
  (pb-set-field ?pb-test-scenario "type_id" ?self:type-id)
  (pb-set-field ?pb-test-scenario "description" ?self:description)

  (return ?pb-test-scenario)
)

(defmessage-handler TestScenario setup (?time ?state-machine)
)

(defmessage-handler TestScenario handle-feedback (?pb-msg ?time ?name ?team)
  (return CONTINUE)
)


(defclass NoneTestScenario (is-a TestScenario) (role concrete))

(defmessage-handler NoneTestScenario setup (?time ?state-machine)
  (make-instance [init-state] of InitState)

  (make-instance ?state-machine of StateMachine
    (current-state [init-state])
    (states [init-state])
  )
)


(defclass Test (is-a USER)
  (slot current-scenario (type INSTANCE) (allowed-classes TestScenario))
  (slot requested-scenario (type INSTANCE) (allowed-classes TestScenario))
  (multislot registered-scenarios (type INSTANCE) (allowed-classes TestScenario))

  ; time that the test is running
  (slot time (type INSTANCE) (allowed-classes TestTime))

  ; State machine that coordinates the test execution
  (slot state-machine (type INSTANCE) (allowed-classes StateMachine))
)

(defmessage-handler Test switch-scenario ()
  (send ?self put-current-scenario ?self:requested-scenario)


  ; Remove all items from the inventory
  (do-for-all-instances ((?inventory Inventory))
    (foreach ?item (send ?inventory get-items)
      (unmake-instance ?item)
    )
    (slot-delete$ ?inventory items 1 (length$ (send ?inventory get-items)))
  )

  ; Remove all orders from the order info
  (do-for-all-instances ((?order-info OrderInfo))
    (foreach ?order (send ?order-info get-orders)
      (unmake-instance ?order)
    )
    (slot-delete$ ?order-info orders 1 (length$ (send ?order-info get-orders)))
  )


  ; TODO: Remove all states and their transitions


  (send ?self:current-scenario setup ?self:time ?self:state-machine)
)

(defmessage-handler Test request-scenario (?type ?type-id)
  (foreach ?scenario (send ?self get-registered-scenarios)
    (bind ?scenario-type (send ?scenario get-type))
    (bind ?scenario-type-id (send ?scenario get-type-id))

    (if (and (eq ?type ?scenario-type) (eq ?type-id ?scenario-type-id))
     then
      (send ?self put-requested-scenario ?scenario)
      (return)
    )
  )

  (printout t "Requested test scenario " ?type ?type-id " does not exist" crlf)
)

(defmessage-handler Test handle-feedback (?pb-msg ?time ?name ?team)
  (bind ?scenario (send ?self get-current-scenario))
  (return (send ?scenario handle-feedback ?pb-msg ?time ?name ?team))
)


(defrule init-test
  (init)
  =>
  (make-instance [NONE] of NoneTestScenario (type NONE) (type-id 0) (description "No test running"))

  (make-instance [test] of Test
    (time (make-instance of TestTime))
    (registered-scenarios [NONE])
  )

  (send [test] request-scenario NONE 0)
  (send [test] switch-scenario)
)

(defrule test-update
  (time $?now)
  ?bm <- (object (is-a Test))
  =>
  (bind ?state-machine (send ?bm get-state-machine))
  (send ?state-machine update)
)
