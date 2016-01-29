//
// Created by feanor on 12/4/15.
//

#pragma once

#include "mosquittopp.h"

#include <protobuf_comm/message_register.h>

#include <google/protobuf/message.h>
#include <google/protobuf/descriptor.h>
#include <boost/asio.hpp>
#include <boost/signals2.hpp>

#include <string>

namespace protobuf_comm {
#if 0 /* just to make Emacs auto-indent happy */
    }
#endif

class MqttClient : public mosqpp::mosquittopp
{
   public:
    MqttClient();
    MqttClient(std::string clientID, std::string host, int port, bool cleanSession = true);
    ~MqttClient();

    std::string getClientID();
    std::string getHost();
    int getPort();
    
    void reinitialise(bool cleanSession = true);
    void reinitialise(std::string clientID, bool cleanSession = true);
    void connect();
    void connect(std::string bindAddress);
    void reconnect();
    void disconnect();

    /** Boost signal for on connect */
    typedef
    boost::signals2::signal<void(int)>
            signal_on_connect;
    /** Boost signal for on disconnect */
    typedef
    boost::signals2::signal<void(int)>
            signal_on_disconnect;


    /** Signal that is invoked when the connection has been established.
    * @return signal
    */
    signal_on_connect & signal_connected()
    { return sig_connected_; }

    /** Signal that is invoked when the connection is closed.
    * @return signal
    */
    signal_on_disconnect & signal_disconnected()
    { return sig_disconnected_; }

    
    private:
     std::string clientID_;
     std::string host_;
     int    port_;
     int    keepAlive_;
     bool   bConnected_;
     bool   bAutoReconnect_;

     signal_on_connect        sig_connected_;
     signal_on_disconnect     sig_disconnected_;
     //signal_error_type        sig_error_;

     void on_connect(int rc);
     void on_disconnect(int rc);
     virtual void on_publish(int rc) {return;};
     virtual void on_subscribe(int rc){return;};
     virtual void on_message(const struct mosquitto_message *message){return;};
     void on_log(int level, const char *str);
     void on_error();
     void check_error(int ret);

     void run_mqtt();
};
} // end namespace protobuf_comm