;---------------------------------------------------------------------------
;  init.clp - RoboCup-at-Work RefBox CLIPS initialization file
;
;  Copyright  2013  Tim Niemueller [www.niemueller.de]
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defglobal
  ?*CLIPS_DIRS* = (get-clips-dirs)
  ?*DEBUG* = 2  ;debug levels: 0 ~ none, 1 ~ minimal, 2 ~ more, 3 ~ maximum
  ?*CONFIG_PREFIXES* = (create$ "/llsfrb")
  ?*START-TIME* = (now)
  ?*BNT1* = ""
  ?*BMT1* = ""
  ?*BTT1* = ""
  ?*BTT2* = ""
  ?*BTT3* = ""
  ?*PPT1* = ""
  ?*CBT1* = ""
  ?*CBT2* = ""
  ?*RFT1* = ""

)

(deffunction resolve-file (?file)
  (foreach ?d ?*CLIPS_DIRS*
     (bind ?fn (str-cat ?d ?file))
     (if (open ?fn file-clips-tmp)
      then
       (close file-clips-tmp)
       (return ?fn)
     )
  )
  (return FALSE)
)

(load* (resolve-file utils.clp))
(load* (resolve-file time.clp))
(load* (resolve-file config.clp))
(load* (resolve-file protobuf.clp))

(load* (resolve-file globals.clp))
(load* (resolve-file priorities.clp))
(load* (resolve-file facts.clp))

(defrule load-config
  (init)
  =>
  (foreach ?p ?*CONFIG_PREFIXES*
    (load-config ?p)
  )
  (assert (config-loaded))
)

(defrule load-refbox
  (init)
  (confval (path "/llsfrb/clips/main") (type STRING) (value ?v))
  =>
  (printout t "Loading refbox main file '" ?v "'" crlf)
  (batch* (resolve-file (str-cat ?v ".clp")))
)

(defrule enable-debug
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value ?v))
  =>
  (if (eq ?v true) then
    (printout t "CLIPS debugging enabled, watching facts and rules" crlf)
    (watch facts)
    (watch rules)
  )
)

(defrule debug-level
  (init)
  (confval (path "/llsfrb/clips/debug-level") (type UINT) (value ?v))
  =>
  (printout t "Setting debug level to " ?v " (was " ?*DEBUG* ")" crlf)
  (bind ?*DEBUG* ?v)
)

(defrule load-tests
  (init)
  (confval (path "/llsfrb/raw/tests/BNT/1") (type STRING) (value ?bnt1))
  (confval (path "/llsfrb/raw/tests/BMT/1") (type STRING) (value ?bmt1))
  (confval (path "/llsfrb/raw/tests/BTT/1") (type STRING) (value ?btt1))
  (confval (path "/llsfrb/raw/tests/BTT/2") (type STRING) (value ?btt2))
  (confval (path "/llsfrb/raw/tests/BTT/3") (type STRING) (value ?btt3))
  (confval (path "/llsfrb/raw/tests/PPT/1") (type STRING) (value ?ppt1))
  (confval (path "/llsfrb/raw/tests/CBT/1") (type STRING) (value ?cbt1))
  (confval (path "/llsfrb/raw/tests/CBT/2") (type STRING) (value ?cbt2))
  (confval (path "/llsfrb/raw/tests/RFT/1") (type STRING) (value ?rft1))
  =>
  (printout t "LOADING TASK SPECIFICATIONS FROM CONFIG FILE" crlf)
  (bind ?*BNT1* ?bnt1)
  (bind ?*BMT1* ?bmt1)
  (bind ?*BTT1* ?btt1)
  (bind ?*BTT2* ?btt2)
  (bind ?*BTT3* ?btt3)
  (bind ?*PPT1* ?ppt1)
  (bind ?*CBT1* ?cbt1)
  (bind ?*CBT2* ?cbt2)
  (bind ?*RFT1* ?rft1)
)

(defrule silence-debug-facts
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value true))
  (confval (path "/llsfrb/clips/unwatch-facts") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Disabling watching of the following facts: " ?lv crlf)
  (foreach ?v ?lv (unwatch facts (sym-cat ?v)))
)

(defrule silence-debug-rules
  (declare (salience -1000))
  (init)
  (confval (path "/llsfrb/clips/debug") (type BOOL) (value true))
  (confval (path "/llsfrb/clips/unwatch-rules") (type STRING) (is-list TRUE) (list-value $?lv))
  =>
  (printout t "Disabling watching of the following rules: " ?lv crlf)
  (foreach ?v ?lv (unwatch rules (sym-cat ?v)))
)

(defrule load-mongodb
  (init)
  (have-feature MongoDB)
  =>
  (printout t "Enabling MongoDB logging" crlf)
  (load* (resolve-file mongodb.clp))
)

(defrule config-timer-interval
  (confval (path "/llsfrb/clips/timer-interval") (type ?t) (value ?v))
  =>
  (bind ?*TIMER-INTERVAL* (/ ?v 1000.))
)

(defrule announce-loading-done
  (declare (salience ?*PRIORITY_LAST*))
  (init)
  =>
  (printout t "RefBox loaded and ready to run" crlf)
)

(defrule announce-finalize
  (declare (salience ?*PRIORITY_LAST*))
  (finalize)
  =>
  (printout t "===  Shutting down  ===" crlf)
)

(reset)
(seed (integer (time)))
