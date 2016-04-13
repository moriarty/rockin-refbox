;---------------------------------------------------------------------------
;  order-info.clp - RoboCup-at-Work RefBox CLIPS - order info specification
;
;  Licensed under BSD license, cf. LICENSE file
;---------------------------------------------------------------------------

(defclass OrderInfo (is-a USER) (role concrete)
  ; The order info represents all currently existing orders in the system

  (multislot orders (type INSTANCE) (allowed-classes Order))
)

(defmessage-handler OrderInfo create-msg ()
  "Create a ProtoBuf message of an order info"

  (bind ?oi (pb-create "raw_msgs.OrderInfo"))

  (foreach ?order ?self:orders
    (pb-add-list ?oi "orders" (send ?order create-msg))
  )

  (return ?oi)
)


(defrule init-order-info
  (init)
  =>
  (make-instance [order-info] of OrderInfo)
)
