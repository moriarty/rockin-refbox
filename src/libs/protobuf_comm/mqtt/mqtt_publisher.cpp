//
// Created by feanor on 12/20/15.
//

#include <protobuf_comm/mqtt/mqtt_publisher.h>

using namespace std;
using namespace mosqpp;

namespace protobuf_comm {
#if 0 /* just to make Emacs auto-indent happy */
    }
#endif


MqttPublisher::MqttPublisher(string host, int port, bool cleanSession)
        : MqttClient("pub_to_"+ host + "_" +std::to_string(getpid()), host, port, cleanSession)
{

}

void MqttPublisher::publish(string topic, std::shared_ptr <google::protobuf::Message> &m)
{
    this->publish(topic,*m);
}

void MqttPublisher::publish(string topic, google::protobuf::Message &m)
{
    string data;
    if(m.SerializeToString(&data)){
        mosquittopp::publish(NULL, topic.c_str(), data.size(), data.c_str(), 0, true);
    }else{
        cout << "serialize fails" <<endl;
    }
}

void MqttPublisher::on_publish(int rc)
{
    sig_pub_(rc);
}
}