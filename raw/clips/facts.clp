;---------------------------------------------------------------------------
;  facts.clp - RoboCup-at-Work RefBox CLIPS - facts specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(deftemplate known-team
  (slot name (type STRING))
)

(deftemplate robot
  (slot team (type STRING))
  (slot name (type STRING))
  (slot host (type STRING))
  (slot port (type INTEGER))
  (multislot last-seen (type INTEGER) (cardinality 2 2))
  (slot warning-sent (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
  (slot is-logging (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
)

(deftemplate signal
  (slot type)
  (multislot time (type INTEGER) (cardinality 2 2) (default (create$ 0 0)))
  (slot seq (type INTEGER) (default 1))
  (slot count (type INTEGER) (default 1))
)

(deftemplate network-client
  (slot id (type INTEGER))
  (slot host (type STRING))
  (slot port (type INTEGER))
)

(deftemplate network-peer
  (slot group (type STRING))
  (slot id (type INTEGER))
)

(deftemplate attention-message
  (slot team (type STRING) (default ""))
  (slot text (type STRING))
  (slot time (type INTEGER) (default 5))
)

(deftemplate triggered-conveyor-belt
  (slot cycle (type INTEGER) (default 0))
)

(deftemplate test-info
  (slot object (type STRING) (default ""))
)

(deftemplate test-feedback
  (slot source (type SYMBOL))
  (multislot time (type INTEGER) (cardinality 2 2) (default (create$ 0 0)))
  (slot type (type SYMBOL) (allowed-values SELECTED STARTED TIMEOUT))
)


(deffacts startup
  (signal (type version-info)            (time (create$ 0 0)) (seq 1))
  (signal (type beacon)                  (time (create$ 0 0)) (seq 1))
  (signal (type test-state)              (time (create$ 0 0)) (seq 1))
  (signal (type test-info)               (time (create$ 0 0)) (seq 1))
  (signal (type task-specification)      (time (create$ 0 0)) (seq 1))
  (signal (type robot-info)              (time (create$ 0 0)) (seq 1))
  (signal (type order-info)              (time (create$ 0 0)) (seq 1))
  (signal (type inventory)               (time (create$ 0 0)) (seq 1))
  (signal (type conveyor-belt)           (time (create$ 0 0)) (seq 1))
  (signal (type triggered-conveyor-belt) (time (create$ 0 0)) (seq 1))

  (triggered-conveyor-belt)
)
