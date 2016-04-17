;---------------------------------------------------------------------------
;  globals.clp - RoboCup-at-Work RefBox global CLIPS variables
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defglobal
  ; network sending periods; seconds
  ?*BEACON-PERIOD* = 1.0
  ?*TESTSTATE-PERIOD* = 0.1
  ?*TESTINFO-PERIOD* = 0.1
  ?*ROBOTINFO-PERIOD* = 0.25
  ?*INVENTORY-PERIOD* = 1.0
  ?*CONVEYOR-BELT-PERIOD* = 0.1
  ?*BC-ORDERINFO-PERIOD* = 2.0
  ?*BC-ORDERINFO-BURST-PERIOD* = 0.5

  ; This value is set by the rule config-timer-interval from config.yaml
  ?*TIMER-INTERVAL* = 0.0

  ; Time (sec) after which to warn about a robot lost
  ?*PEER-LOST-TIMEOUT* = 5
  ?*PEER-REMOVE-TIMEOUT* = 1080
  ?*PEER-TIME-DIFFERENCE-WARNING* = 3.0

  ; number of burst updates before falling back to slower updates
  ?*BC-ORDERINFO-BURST-COUNT* = 10

  ; How often and in what period should the version information
  ; be send over the network when a new peer is detected?
  ?*BC-VERSIONINFO-PERIOD* = 0.5
  ?*BC-VERSIONINFO-COUNT* = 10

  ; Test times; seconds
  ?*BNT-TIME*               =  600
  ?*BMT-TIME*               =  600
  ?*BTT-TIME*               =  600
  ?*PPT-TIME*               =  600
  ?*CBT-TIME*               =  600
  ?*RFT-TIME*               =  600

  ; Test repetitions
  ?*BNT-COUNT*    =  1
  ?*BMT-COUNT*    =  1
  ?*BTT-COUNT*    =  1
  ?*PPT-COUNT*    =  1
  ?*CBT-COUNT*    =  1
  ?*RFT-COUNT*    =  1
)
