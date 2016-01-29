/*  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the authors nor the names of its contributors
 *   may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <protobuf_comm/mqtt/mqtt_subscriber.h>
#include <protobuf_comm/message_register.h>
#include <msgs/AttentionMessage.pb.h>


using namespace protobuf_comm;

class ExampleMqttSubscriber
{
public:
    ExampleMqttSubscriber(std::string host, unsigned short port)
            : host_(host), port_(port)
    {
       client_ = new MqttSubscriber(host_,port_);

       client_->signal_connected().connect(
                boost::bind(&ExampleMqttSubscriber::client_connected, this, _1));
       client_->signal_disconnected().connect(
                boost::bind(&ExampleMqttSubscriber::client_disconnected, this, _1));
       client_->signal_subscribed().connect(
		boost::bind(&ExampleMqttSubscriber::client_subscribed, this, _1, _2, _3)); 
       client_->signal_received().connect(
                boost::bind(&ExampleMqttSubscriber::client_msg, this, _1, _2, _3, _4));

       client_->connect();
    }

    ~ExampleMqttSubscriber()
    {
        delete client_;
    }

    void subscribe(std::string team){
        //std::shared_ptr<atwork_msgs::AttentionMessage> a;
        client_->subscribe("test", "atwork_msgs.AttentionMessage");
    }

private:
    void client_connected(int rc)
    {
        std::cout << "Client connected with rc: " << rc << std::endl;
    }

    void client_disconnected(int rc)
    {
        std::cout << "Client Disconnected with rc: " << rc << std::endl;
        usleep(100000);
        client_->reinitialise(false);
    }
    
    void client_subscribed(int mid, int qos_count, const int *granted_qos){
	std::cout << "Client subscribed with mid: " << mid 
		  << " with qos: "<<qos_count <<std::endl;
    }

    void client_msg(std::string topic, uint16_t comp_id, uint16_t msg_type,
                    std::shared_ptr<google::protobuf::Message> msg)
    {
        std::shared_ptr<atwork_msgs::AttentionMessage> a;
        if ((a = std::dynamic_pointer_cast<atwork_msgs::AttentionMessage>(msg))) {
            std::cout << "Attention message received from Topic: "<< topic << std::endl;
        }
    }


private:
    MqttSubscriber *client_;
    std::string host_;
    unsigned short port_;
};


int main(int argc, char **argv)
{
    ExampleMqttSubscriber client("test.mosquitto.org", 1883);
    std::cout << "Connected" << std::endl;
    client.subscribe("test");


    while (true) {
        usleep(100000);
    }
}

