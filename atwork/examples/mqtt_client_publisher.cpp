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

#include <protobuf_comm/mqtt/mqtt_publisher.h>
#include <msgs/AttentionMessage.pb.h>

using namespace protobuf_comm;

class ExampleMqttPublisher
{
public:
    ExampleMqttPublisher(std::string host, unsigned short port)
            : host_(host), port_(port)
    {
       client_ = new MqttPublisher(host_,port_);

       client_->signal_connected().connect(
                boost::bind(&ExampleMqttPublisher::client_connected, this, _1));
       client_->signal_disconnected().connect(
                boost::bind(&ExampleMqttPublisher::client_disconnected, this, _1));
       client_->signal_published().connect(
		boost::bind(&ExampleMqttPublisher::client_published, this, _1)); 
       client_->connect();
    }

    void client_publish_msg(std::string team){
        std::string topic = "";

	atwork_msgs::AttentionMessage am; 	
	am.set_message("test");
        //am.set_time_to_show(1);
        am.set_team("SuperTeam");
	
        topic = team + "/" + am.GetDescriptor()->full_name();
        std::cout <<"Topic Name: "<<topic <<std::endl;
        client_->publish(topic, am);
    }


    ~ExampleMqttPublisher()
    {
        delete client_;
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
    
    void client_published(int mid){
	std::cout << "Client published with mid: " << mid << std::endl; 
    }

private:
    MqttPublisher *client_;
    std::string host_;
    unsigned short port_;
};

int main(int argc, char **argv)
{
    ExampleMqttPublisher client("test.mosquitto.org", 1883);
    atwork_msgs::AttentionMessage am;
    client.client_publish_msg("test");
    while (true) {

        usleep(100000);
    }
}

