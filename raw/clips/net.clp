;---------------------------------------------------------------------------
;  net.clp - RoboCup-at-Work RefBox CLIPS network handling
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deffunction net-create-VersionInfo ()
  (bind ?vi (pb-create "raw_msgs.VersionInfo"))
  (pb-set-field ?vi "version_major" ?*VERSION-MAJOR*)
  (pb-set-field ?vi "version_minor" ?*VERSION-MINOR*)
  (pb-set-field ?vi "version_micro" ?*VERSION-MICRO*)
  (pb-set-field ?vi "version_string"
    (str-cat ?*VERSION-MAJOR* "." ?*VERSION-MINOR* "." ?*VERSION-MICRO*))
  (return ?vi)
)

(deffunction net-init-peer (?cfg-prefix ?group)
  (bind ?peer-id 0)

  (do-for-fact ((?csp confval) (?crp confval) (?ch confval))
    (and (eq ?csp:type UINT) (eq ?csp:path (str-cat ?cfg-prefix "send-port"))
        (eq ?crp:type UINT) (eq ?crp:path (str-cat ?cfg-prefix "recv-port"))
        (eq ?ch:type STRING) (eq ?ch:path (str-cat ?cfg-prefix "host")))
    (printout t "Creating local communication peer for group " ?group
        " (send port " ?csp:value "  recv port " ?crp:value ")" crlf)
    (bind ?peer-id (pb-peer-create-local ?ch:value ?csp:value ?crp:value))
  )
  (if (eq ?peer-id 0)
   then
    (do-for-fact ((?cp confval) (?ch confval))
      (and (eq ?cp:type UINT) (eq ?cp:path (str-cat ?cfg-prefix "port"))
          (eq ?ch:type STRING) (eq ?ch:path (str-cat ?cfg-prefix "host")))
      (printout t "Creating communication peer for group " ?group
          " (port " ?cp:value ")" crlf)
      (bind ?peer-id (pb-peer-create ?ch:value ?cp:value))
    )
  )

  (if (neq ?peer-id 0)
   then
    (assert (network-peer (group ?group) (id ?peer-id)))
   else
    (printout warn "No network configuration found for " ?group " at " ?cfg-prefix crlf)
  )
)

(defrule net-init-public
  (init)
  (config-loaded)
  (not (network-peer (group "PUBLIC")))
  =>
  (net-init-peer "/llsfrb/comm/public-peer/" "PUBLIC")
)

(defrule net-init-peers
  (init)
  (config-loaded)
  (known-team (name ?team))
  (not (network-peer (group ?team)))
  =>
  (net-init-peer (str-cat "/llsfrb/comm/" ?team "-peer/") ?team)
)

(defrule net-read-known-teams
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/game/teams") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Teams: " ?lv crlf)
  (foreach ?team ?lv
    (assert (known-team (name ?team)))
  )
)

(defrule net-client-connected
  ?cf <- (protobuf-server-client-connected ?client-id ?host ?port)
  =>
  (retract ?cf)
  (assert (network-client (id ?client-id) (host ?host) (port ?port)))
  (printout t "Client " ?client-id " connected from " ?host ":" ?port crlf)

  ; Send version information right away
  (bind ?vi (net-create-VersionInfo))
  (pb-send ?client-id ?vi)
  (pb-destroy ?vi)
)

(defrule net-client-disconnected
  ?cf <- (protobuf-server-client-disconnected ?client-id)
  ?nf <- (network-client (id ?client-id) (host ?host))
  =>
  (retract ?cf ?nf)
  (printout t "Client " ?client-id " ( " ?host ") disconnected" crlf)
)

(defrule net-send-beacon
  (time $?now)
  ?f <- (signal (type beacon) (time $?t&:(timeout ?now ?t ?*BEACON-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (if (debug 3) then (printout t "Sending beacon" crlf))
  (bind ?beacon (pb-create "raw_msgs.BeaconSignal"))
  (bind ?beacon-time (pb-field-value ?beacon "time"))
  (pb-set-field ?beacon-time "sec" (nth$ 1 ?now))
  (pb-set-field ?beacon-time "nsec" (integer (* (nth$ 2 ?now) 1000)))
  (pb-set-field ?beacon "time" ?beacon-time) ; destroys ?beacon-time!
  (pb-set-field ?beacon "seq" ?seq)
  (pb-set-field ?beacon "team_name" "RoboCup-at-Work")
  (pb-set-field ?beacon "peer_name" "RefBox")
  (pb-broadcast ?peer-id-public ?beacon)
  (pb-destroy ?beacon)
)

(defrule net-recv-beacon-known
  ?mf <- (protobuf-msg (type "raw_msgs.BeaconSignal") (ptr ?p) (rcvd-at $?rcvd-at)
           (rcvd-from ?from-host ?from-port) (rcvd-via ?via))
  ?rf <- (robot (host ?from-host) (port ?from-port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout debug "Received beacon from known " ?from-host ":" ?from-port crlf)
  (bind ?team (pb-field-value ?p "team_name"))
  (bind ?name (pb-field-value ?p "peer_name"))
  (bind ?time (pb-field-value ?p "time"))

  (modify ?rf (last-seen ?rcvd-at) (warning-sent FALSE))
)

(defrule net-recv-beacon-unknown
  ?mf <- (protobuf-msg (type "raw_msgs.BeaconSignal") (ptr ?p) (rcvd-at $?rcvd-at)
           (rcvd-from ?from-host ?from-port) (rcvd-via ?via))
  (not (robot (host ?from-host) (port ?from-port)))
  ?sf <- (signal (type version-info))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (modify ?sf (count 0) (time 0 0))
  (printout debug "Received initial beacon from " ?from-host ":" ?from-port crlf)
  (bind ?team (pb-field-value ?p "team_name"))
  (bind ?name (pb-field-value ?p "peer_name"))
  (bind ?timef (pb-field-value ?p "time"))
  (bind ?time (create$ (pb-field-value ?timef "sec") (integer (/ (pb-field-value ?timef "nsec") 1000))))
  (bind ?peer-time-diff (abs (time-diff-sec ?rcvd-at ?time)))
  (if (> ?peer-time-diff ?*PEER-TIME-DIFFERENCE-WARNING*)
   then
    (printout warn "Robot " ?name " of " ?team
        " has a large time offset (" ?peer-time-diff " sec)" crlf)
    (assert (attention-message (text (str-cat "Robot " ?name " of " ?team
                " has a large time offset ("
                ?peer-time-diff " sec)"))))
  )
  (do-for-fact ((?other robot)) (eq ?other:host ?from-host)
    (printout warn "Received two BeaconSignals from host " ?from-host
        " (" ?other:team "/" ?other:name "@" ?other:port " vs "
        ?team "/" ?from-host "@" ?from-port ")" crlf)
    (assert (attention-message (text (str-cat "Received two BeaconSignals form host "
                ?from-host " (" ?other:team "/" ?other:name
                "@" ?other:port " vs " ?team "/" ?from-host
                "@" ?from-port ")"))))
  )

  (if (and (eq ?team "RoboCup-at-Work") (eq ?name "RefBox"))
   then
    (printout warn "Detected another RefBox at " ?from-host ":" ?from-port crlf)
    (assert (attention-message (text (str-cat "Detected another RefBox at "
                ?from-host ":" ?from-port))))
  )
  (assert (robot (team ?team) (name ?name) (host ?from-host) (port ?from-port)
    (last-seen ?rcvd-at)))
)

(defrule send-attmsg
  ?af <- (attention-message (text ?text) (team ?team) (time ?time-to-show))
  =>
  (retract ?af)
  (bind ?attmsg (pb-create "raw_msgs.AttentionMessage"))
  (pb-set-field ?attmsg "message" (str-cat ?text))
  (if (neq (str-compare ?team "") 0) then (pb-set-field ?attmsg "team" ?team))
  (if (> ?time-to-show 0) then
    (pb-set-field ?attmsg "time_to_show" ?time-to-show))

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?attmsg))
  (pb-destroy ?attmsg)
)

(defrule net-recv-SetTestScenario
  ?mf <- (protobuf-msg (type "raw_msgs.SetTestScenario") (ptr ?p) (rcvd-via STREAM))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Get the scenario type (NONE, TBM) and type id from the message
  (bind ?pb-scenario (pb-field-value ?p "scenario"))
  (bind ?pb-scenario-type (pb-field-value ?pb-scenario "type"))
  (bind ?pb-scenario-type-id (pb-field-value ?pb-scenario "type_id"))

  (send [test] request-scenario ?pb-scenario-type ?pb-scenario-type-id)
)

(defrule net-recv-SetTestScenario-illegal
  ?mf <- (protobuf-msg (type "raw_msgs.SetTestScenario") (ptr ?p)
           (rcvd-via BROADCAST) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout warn "Illegal SetTestScenario message received from host " ?host crlf)
)

(defrule net-recv-SetTestTransitionEvent
  ?mf <- (protobuf-msg (type "raw_msgs.SetTestTransitionEvent") (ptr ?p)
           (rcvd-via STREAM) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (bind ?pb-event (pb-field-value ?p "event"))

  (if (eq ?pb-event RESET) then
    (send [test] switch-scenario)
  else
    (printout t "RECV TRANSITION EVENT: " ?pb-event crlf)
    (bind ?state-machine (send [test] get-state-machine))
    (send ?state-machine process-event ?pb-event)
  )
)

(defrule net-recv-SetTestTransitionEvent-illegal
  ?mf <- (protobuf-msg (type "raw_msgs.SetTestTransitionEvent") (ptr ?p)
           (rcvd-via BROADCAST) (rcvd-from ?host ?port))
  =>
  (retract ?mf) ; message will be destroyed after rule completes
  (printout warn "Illegal SetTestTransitionEvent message received from host " ?host crlf)
)

(deffunction net-create-TestState ()
  (bind ?teststate (pb-create "raw_msgs.TestState"))
  (bind ?teststate-time (pb-field-value ?teststate "test_time"))

  ; Set the test time (in seconds)
  (if (eq (type ?teststate-time) EXTERNAL-ADDRESS) then
    (bind ?bt (send [test] get-time))
    (bind ?time (time-from-sec (send ?bt get-timer)))
    (pb-set-field ?teststate-time "sec" (nth$ 1 ?time))
    (pb-set-field ?teststate-time "nsec" (integer (* (nth$ 2 ?time) 1000)))
    (pb-set-field ?teststate "test_time" ?teststate-time) ; destroys ?teststate-time!
  )

  ; Add the current scenario (e.g. TBM1 or TBM3) of the test
  (bind ?current-scenario (send [test] get-current-scenario))
  (bind ?pb-test-scenario (send ?current-scenario create-msg))
  (pb-set-field ?teststate "scenario" ?pb-test-scenario)

  ; Add all known teams
  (do-for-all-facts ((?team known-team)) TRUE
    (pb-add-list ?teststate "known_teams" ?team:name)
  )

  ; Set the test state (e.g. PAUSED or RUNNING) based on the state machine
  (bind ?state-machine (send [test] get-state-machine))
  (bind ?current-state (send ?state-machine get-current-state))
  (bind ?robot-state (send ?current-state to-robot-state))
  (pb-set-field ?teststate "state" ?robot-state)

  (bind ?robot-phase (send ?current-state to-robot-phase))
  (pb-set-field ?teststate "phase" ?robot-phase)

  (return ?teststate)
)

(defrule net-send-TestState
  (time $?now)
  ?f <- (signal (type test-state) (time $?t&:(timeout ?now ?t ?*TESTSTATE-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (bind ?test-state (net-create-TestState))

  (pb-broadcast ?peer-id-public ?test-state)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?test-state)
  )
  (pb-destroy ?test-state)
)

(deffunction net-create-TestInfo (?info)
  (bind ?bi (pb-create "raw_msgs.TestInfo"))

  (pb-set-field ?bi "object" (fact-slot-value ?info object))

  (return ?bi)
)

(defrule net-send-TestInfo
  (time $?now)
  ?f <- (signal (type test-info) (time $?t&:(timeout ?now ?t ?*TESTINFO-PERIOD*)) (seq ?seq))
  ;(test-info ?info)
  ?info <- (test-info)
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (bind ?test-info (net-create-TestInfo ?info))

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?test-info)
  )

  (pb-destroy ?test-info)
)

(deffunction net-create-RobotInfo ()
  (bind ?ri (pb-create "raw_msgs.RobotInfo"))

  (bind ?robots (find-all-facts ((?r robot)) TRUE))
  (bind ?robots (sort robot-order ?robots))

  (foreach ?robot ?robots
    (bind ?r (pb-create "raw_msgs.Robot"))
    (bind ?r-time (pb-field-value ?r "last_seen"))
    (if (eq (type ?r-time) EXTERNAL-ADDRESS) then
      (pb-set-field ?r-time "sec" (nth$ 1 (fact-slot-value ?robot last-seen)))
      (pb-set-field ?r-time "nsec" (integer (* (nth$ 2 (fact-slot-value ?robot last-seen)) 1000)))
      (pb-set-field ?r "last_seen" ?r-time) ; destroys ?r-time!
    )

    (pb-set-field ?r "name" (fact-slot-value ?robot name))
    (pb-set-field ?r "team" (fact-slot-value ?robot team))
    (pb-set-field ?r "host" (fact-slot-value ?robot host))
    (pb-set-field ?r "is_logging" (fact-slot-value ?robot is-logging))

    (pb-add-list ?ri "robots" ?r) ; destroys ?r
  )

  (return ?ri)
)

(defrule net-send-RobotInfo
  (time $?now)
  ?f <- (signal (type robot-info) (time $?t&:(timeout ?now ?t ?*ROBOTINFO-PERIOD*)) (seq ?seq))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))
  (bind ?ri (net-create-RobotInfo))


  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?ri)
  )

  (pb-destroy ?ri)
)

(defrule net-send-Inventory
  (time $?now)
  ?f <- (signal (type inventory) (time $?t&:(timeout ?now ?t ?*INVENTORY-PERIOD*)) (seq ?seq))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?f (time ?now) (seq (+ ?seq 1)))

  (bind ?item-count (length (send [inventory] get-items)))
  (if (>= ?item-count 23)
   then
    (printout t "Too many items in the inventory! Please reset the test" crlf)
    (assert (attention-message (text "Too many items in the inventory! Please reset the test")))
    (return)
  )

  (bind ?pb-inventory (send [inventory] create-msg))
  (pb-broadcast ?peer-id-public ?pb-inventory)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?pb-inventory)
  )

  (pb-destroy ?pb-inventory)
)

(defrule net-send-OrderInfo
  (time $?now)
  ?sf <- (signal (type order-info) (seq ?seq) (count ?count)
     (time $?t&:(timeout ?now ?t (if (> ?count ?*BC-ORDERINFO-BURST-COUNT*)
                 then ?*BC-ORDERINFO-PERIOD*
                 else ?*BC-ORDERINFO-BURST-PERIOD*))))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?sf (time ?now) (seq (+ ?seq 1)) (count (+ ?count 1)))

  (bind ?oi (send [order-info] create-msg))
  (pb-broadcast ?peer-id-public ?oi)

  (do-for-all-facts ((?client network-client)) TRUE
    (pb-send ?client:id ?oi)
  )

  (pb-destroy ?oi)
)

(defrule net-recv-OrderAcceptance
  ?mf <- (protobuf-msg (type "raw_msgs.OrderAcceptance") (ptr ?p) (rcvd-at $?rcvd-at)
           (rcvd-from ?from-host ?from-port) (rcvd-via ?via))
  ?rf <- (robot (name ?name) (host ?from-host) (port ?from-port) (team ?team))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (bind ?pb-order-ids (pb-field-list ?p "id"))

  (foreach ?id ?pb-order-ids
    (do-for-all-instances ((?order Order)) (eq ?order:id ?id)
      (bind ?processing-team (send ?order get-processing-team))

      ; Check if any team is already processing the order
      (if (eq (length$ ?processing-team) 0) then
        (printout t ?name "/" ?team " accepts order " ?id crlf)
        (slot-insert$ ?order processing-team 1 ?team)
      else
        (bind ?processing-team (nth$ 1 ?processing-team))

        ; Check if the requesting team is processing the order
        (if (neq (str-compare ?processing-team ?team) 0) then
          (printout t ?name "/" ?team " tries to accept order " ?id ", but the order is already processed by " ?processing-team crlf)
        )
      )
    )
  )
)

(defrule net-send-VersionInfo
  (time $?now)
  ?sf <- (signal (type version-info) (seq ?seq)
     (count ?count&:(< ?count ?*BC-VERSIONINFO-COUNT*))
     (time $?t&:(timeout ?now ?t ?*BC-VERSIONINFO-PERIOD*)))
  (network-peer (group "PUBLIC") (id ?peer-id-public))
  =>
  (modify ?sf (time ?now) (seq (+ ?seq 1)) (count (+ ?count 1)))
  (bind ?vi (net-create-VersionInfo))
  (pb-broadcast ?peer-id-public ?vi)
  (pb-destroy ?vi)
)

(deffunction print-RobotStatus (?name ?team ?p)
  (bind ?msg (str-cat "Status (" ?name "/" ?team "): "))
  (bind ?has-data FALSE)

  (if (pb-has-field ?p "functionality") then
    (bind ?pb-functionality (pb-field-value ?p "functionality"))
    (bind ?msg (str-cat ?msg ?pb-functionality))
    (bind ?has-data TRUE)
  )

  (if (pb-has-field ?p "capability") then
    (bind ?pb-capability (pb-field-value ?p "capability"))
    (bind ?msg (str-cat ?msg " [" ?pb-capability "]"))
    (bind ?has-data TRUE)
  )

  (if (pb-has-field ?p "meta_data") then
    (bind ?pb-meta-data (pb-field-value ?p "meta_data"))
    (bind ?msg (str-cat ?msg ": " ?pb-meta-data))
    (bind ?has-data TRUE)
  )

  (if (eq ?has-data TRUE) then
    (printout t ?msg crlf)
    (assert (attention-message (text ?msg)))
  )
)

(defrule net-recv-RobotStatusReport
  ?mf <- (protobuf-msg (type "raw_msgs.RobotStatusReport") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  (robot (name ?name) (team ?team) (host ?host))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (bind ?pb-report (pb-field-list ?p "status"))
  (foreach ?pb-status ?pb-report
    (print-RobotStatus ?name ?team ?pb-status)
  )
)

(defrule net-recv-LoggingStatus
  ?mf <- (protobuf-msg (type "raw_msgs.LoggingStatus") (ptr ?p)
         (rcvd-via ?via) (rcvd-from ?host ?port))
  ?robot <- (robot (host ?host))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  (bind ?is-logging (pb-field-value ?p "is_logging"))
  (if (eq ?is-logging 1) then
    (modify ?robot (is-logging TRUE))
  else
    (modify ?robot (is-logging FALSE))
  )
)

(defrule net-recv-TestFeedback
  ?mf <- (protobuf-msg (type "raw_msgs.TestFeedback") (ptr ?p)
         (rcvd-at $?rcvd-at) (rcvd-from ?host ?port) (client-type PEER))
  (robot (name ?name) (team ?team) (host ?host))
  =>
  (retract ?mf) ; message will be destroyed after rule completes

  ; Identify which phase the peer wants to terminate
  (bind ?phase-to-terminate (pb-field-value ?p "phase_to_terminate"))

  ; Identify the currently active phase
  (bind ?state-machine (send [test] get-state-machine))
  (bind ?current-state (send ?state-machine get-current-state))
  (bind ?current-phase (send ?current-state to-robot-phase))

  ; Exit if the phase to terminate is not the same as the currently active phase
  (if (neq ?phase-to-terminate ?current-phase) then
    (if (debug 3) then (printout t "Ignoring BechmarkFeedback from robot "
        ?name "/" ?team " because the specified phase is incorrect" crlf))
    (return)
  )


  ; Forward the feedback to the test's feedback handler
  (bind ?command (send [test] handle-feedback ?p ?rcvd-at ?name ?team))

  ; Simply return if the test should continue
  (if (eq ?command CONTINUE) then (return))

  ; Switch the state if the test should finish
  (bind ?state-pre (send ?state-machine get-current-state))
  (send ?state-machine process-event FINISH)
  (bind ?state-post (send ?state-machine get-current-state))

  (if (neq ?state-pre ?state-post) then
    (printout t "SENDING TO CLIENTS" crlf)
    ; Forward the feedback to all clients
    (do-for-all-facts ((?client network-client)) TRUE
      (pb-send ?client:id ?p)
    )
  )
)
