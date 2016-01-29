//
// Created by feanor on 12/4/15.
//

#include "mqtt_client.h"
#include <google/protobuf/descriptor_database.h>
#include <google/protobuf/dynamic_message.h>
#include <unistd.h>


using namespace std;
using namespace mosqpp;

namespace protobuf_comm {
#if 0 /* just to make Emacs auto-indent happy */
    }
#endif

MqttClient::MqttClient(): mosquittopp()
{
    lib_init();
    clientID_             = "mqttClient_" + std::to_string(getpid());
    host_                 = "localhost";
    port_                 = 1883;
    keepAlive_            = 60;
    bConnected_           = false;
    bAutoReconnect_       = true;
}

MqttClient::MqttClient(string clientID, string host, int port, bool cleanSession) :
        mosquittopp(clientID.c_str(), cleanSession)
{
    lib_init();
    clientID_             = clientID;
    host_                 = host;
    port_                 = port;
    keepAlive_            = 60;
    bConnected_           = false;
    bAutoReconnect_       = true;

    cout<<"ClientId: " << clientID_ <<endl;
}

MqttClient::~MqttClient()
{
    lib_cleanup();
}

void MqttClient::connect()
{
    mosquittopp::connect(host_.c_str(), port_, keepAlive_);
    run_mqtt();
}

void MqttClient::connect(string bindAddress)
{
    mosquittopp::connect(host_.c_str(), port_, keepAlive_, bindAddress.c_str());
}

void MqttClient::reinitialise(bool cleanSession)
{
    this->reinitialise(clientID_);
}

void MqttClient::reinitialise(string clientID, bool cleanSession)
{
    clientID_ = clientID;
    mosquittopp::reinitialise(clientID_.c_str(), cleanSession);
}
void MqttClient::reconnect()
{
    mosquittopp::reconnect();
}

void MqttClient::disconnect()
{
    mosquittopp::disconnect();
}

void MqttClient::on_connect(int rc)
{
    if (MOSQ_ERR_SUCCESS == rc) {
        bConnected_ = true;
        sig_connected_(rc);
    }
}

void MqttClient::on_disconnect(int rc) {
    if (MOSQ_ERR_SUCCESS == rc) {
        bConnected_ = false;
        sig_disconnected_(rc);
    }
}

void MqttClient::on_log(int level, const char *str)
{

}

void MqttClient::on_error()
{

}

void MqttClient::check_error(int ret) {
   // std::cout << mosqpp::strerror(ret);
}

void MqttClient::run_mqtt()
{
    loop_start();
}
}
