//
// Created by feanor on 12/20/15.
//

#pragma once

#include <protobuf_comm/mqtt/mqtt_client.h>
#include <protobuf_comm/message_register.h>

namespace protobuf_comm {
#if 0 /* just to make Emacs auto-indent happy */
    }
#endif

class MqttSubscriber : public MqttClient
{
public:
    /** Constructor for Subscriber */
    MqttSubscriber(std::string host, int port, bool cleanSession = true);

    void subscribe(std::string team, std::string msg_type, int qos = 1);

    /** Boost signal for on subscribe */
    typedef
    boost::signals2::signal<void(int, int, const int*)>
            signal_on_subscribe;

    /** Boost signal for an error during receiving a message. */
    typedef
    boost::signals2::signal<void (int &, std::string)>
            signal_recv_error_type;

    /** Boost signal for specific received message */
    typedef
    boost::signals2::signal<void(std::string, uint16_t, uint16_t,
                                 std::shared_ptr<google::protobuf::Message>)>
            signal_received_type;

    /** Signal that is invoked when the on subscribe.
    * @return signal
    */
    signal_on_subscribe & signal_subscribed()
    { return sig_sub_; }

    /** Signal that is invoked when a message has been received.
    * @return signal
    */
    signal_received_type &  signal_received()
    { return sig_rcvd_; }

private:

    MessageRegister *message_register_;
    bool             own_message_register_;

    signal_on_subscribe         sig_sub_;
    signal_received_type        sig_rcvd_;
    std::string                 msg_type_;
    std::string                 topic_;

    void on_message(const struct mosquitto_message *message);
    void on_subscribe(int mid, int qos_count, const int *granted_qos);

};
}
