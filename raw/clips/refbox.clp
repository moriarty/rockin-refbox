;---------------------------------------------------------------------------
;  refbox.clp - RoboCup-at-Work RefBox CLIPS main file
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(load* (resolve-file location.clp))
(load* (resolve-file object-identifier.clp))
(load* (resolve-file item.clp))
(load* (resolve-file inventory.clp))
(load* (resolve-file order.clp))
(load* (resolve-file order-info.clp))
(load* (resolve-file test-time.clp))
(load* (resolve-file state-machine.clp))
(load* (resolve-file test.clp))
(load* (resolve-file basic-navigation-tests.clp))
(load* (resolve-file basic-manipulation-tests.clp))
(load* (resolve-file basic-transportation-tests.clp))
(load* (resolve-file precision-placement-tests.clp))
(load* (resolve-file conveyor-belt-tests.clp))
(load* (resolve-file robocup-final-tests.clp))
(load* (resolve-file net.clp))
(load* (resolve-file robots.clp))

(defrule load-conveyor-belt
  (init)
  (have-feature ConveyorBelt)
  =>
  (printout t "Enabling Conveyor Belt" crlf)
  (load* (resolve-file device-conveyor-belt.clp))
)
