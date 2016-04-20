;---------------------------------------------------------------------------
;  task-specification.clp - RoboCup-at-Work RefBox CLIPS - task specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass TaskSpecification (is-a USER) (role concrete)
  (slot task-spec (type STRING) (default ""))
)

(defmessage-handler TaskSpecification create-msg ()
  "Create a ProtoBuf message of an TaskSpecification"

  ; Instantiate a new item for the protobuf message
  (bind ?pb-taskspec (pb-create "raw_msgs.TaskSpecification"))

  (pb-set-field ?pb-taskspec "task_spec" ?self:task-spec )
  ;(printout t "create-msg" (dynamic-get task-spec) "??" crlf)
  (return ?pb-taskspec)
)

(defrule init-task-specification
  ;;; salience must be high to avoid:
  ;
  ; C: [INSFUN2] No such instance task-specification in function modify-instance.
  ; [PRCCODE4] Execution halted during the actions of message-handler switch-scenario primary in class Test
  ;
  (declare (salience ?*PRIORITY_HIGH*))
  (init)
  =>
  (make-instance [task-specification] of TaskSpecification (task-spec "<None>"))
  (printout t "task-specification-initialized" crlf)
)

