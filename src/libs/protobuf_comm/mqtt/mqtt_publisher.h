//
// Created by feanor on 12/20/15.
//
#pragma once
#include <protobuf_comm/mqtt/mqtt_client.h>

namespace protobuf_comm {
#if 0 /* just to make Emacs auto-indent happy */
    }
#endif


class MqttPublisher: public MqttClient
{
    public:
        /* Constructor for Publisher */
        MqttPublisher(std::string host, int port, bool cleanSession = true);
        virtual ~MqttPublisher();

        /** Boost signal for on publish */
        typedef
        boost::signals2::signal<void(int)>
            signal_on_publish;

        /** Boost signal for an error during sending a message. */
        typedef
        boost::signals2::signal<void (std::string)>
             signal_send_error_type;


        /** Signal that is invoked when the on publish.
        * @return signal
        */
        signal_on_publish & signal_published()
        { return sig_pub_; }

        /** Signal that is invoked when sending a message failed.
        * @return signal
        */
        signal_send_error_type &  signal_send_error()
        { return sig_send_error_; }

        void publish(std::string topic, std::shared_ptr <google::protobuf::Message> &m);

        void publish(std::string topic, google::protobuf::Message &m);

    private:

        signal_on_publish        sig_pub_;
        signal_send_error_type   sig_send_error_;
        void on_publish(int rc);

};
}
