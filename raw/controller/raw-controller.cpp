#include <chrono>
#include <iomanip>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <config/yaml.h>
#include <utils/system/argparser.h>

#include <protobuf_comm/client.h>
#include <msgs/TestState.pb.h>
#include <msgs/TestFeedback.pb.h>
#include <msgs/ConveyorBelt.pb.h>

#include <gtkmm.h>
#include <pangomm.h>
#include <glibmm.h>



protobuf_comm::ProtobufStreamClient client;
std::string host;
int port;
Glib::RefPtr<Gtk::Builder> builder;
std::chrono::time_point<std::chrono::system_clock> last_gui_update;
boost::mutex mutex;
std::shared_ptr<raw_msgs::TestState> test_state;



void handle_message(uint16_t comp_id, uint16_t msg_type,
      std::shared_ptr<google::protobuf::Message> msg)
{
  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (std::dynamic_pointer_cast<raw_msgs::TestState>(msg)) {
    test_state = std::dynamic_pointer_cast<raw_msgs::TestState>(msg);
  }
}



bool idle_handler() {
  if ((std::chrono::system_clock::now() - last_gui_update) < std::chrono::milliseconds(100)) {
    usleep(10000);
    return true;
  }
  last_gui_update = std::chrono::system_clock::now();


  // Prevent simultaneous access to the refbox state from gui and network
  boost::mutex::scoped_lock lock(mutex);

  if (test_state) {
    Gtk::Button *button_start = 0;
    Gtk::Button *button_pause = 0;
    Gtk::Button *button_stop = 0;
    builder->get_widget("button_start", button_start);
    builder->get_widget("button_pause", button_pause);
    builder->get_widget("button_stop", button_stop);

    switch (test_state->state()) {
      case raw_msgs::TestState::STOPPED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
      break;

      case raw_msgs::TestState::RUNNING:
        button_start->set_sensitive(false);
        button_pause->set_sensitive(true);
        button_stop->set_sensitive(true);
      break;

      case raw_msgs::TestState::PAUSED:
        button_start->set_sensitive(true);
        button_pause->set_sensitive(false);
        button_stop->set_sensitive(false);
      break;

      case raw_msgs::TestState::FINISHED:
        if (test_state->phase() == raw_msgs::TestState::EXECUTION) {
            button_start->set_sensitive(false);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
        } else {
            button_start->set_sensitive(true);
            button_pause->set_sensitive(false);
            button_stop->set_sensitive(false);
        }
      break;
    }
  }

  return true;
}


void handle_disconnect(const boost::system::error_code &error)
{
  usleep(100000);
  client.async_connect(host.c_str(), port);
}


void on_start_click()
{
  if (!client.connected()) return;

  raw_msgs::SetTestTransitionEvent cmd_event;
  cmd_event.set_event(raw_msgs::SetTestTransitionEvent::START);
  client.send(cmd_event);
}


void on_pause_click()
{
  if (!client.connected()) return;

  raw_msgs::SetTestTransitionEvent cmd_event;
  cmd_event.set_event(raw_msgs::SetTestTransitionEvent::PAUSE);
  client.send(cmd_event);
}


void on_stop_click()
{
  if (!client.connected()) return;

  raw_msgs::SetTestTransitionEvent cmd_event;
  cmd_event.set_event(raw_msgs::SetTestTransitionEvent::STOP);
  client.send(cmd_event);
}


void on_reset_click()
{
  if (!client.connected()) return;

  Gtk::ComboBoxText *combobox_test = 0;
  builder->get_widget("combobox_test", combobox_test);
  std::string test = combobox_test->get_active_text();

  raw_msgs::SetTestScenario cmd_scenario;
  if        (test == "BNT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::BNT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "BMT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::BMT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "BTT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "BTT2") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (test == "BTT3") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::BTT);
    cmd_scenario.mutable_scenario()->set_type_id(3);
  } else if (test == "PPT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::PPT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "CBT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::CBT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "CBT2") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::CBT);
    cmd_scenario.mutable_scenario()->set_type_id(2);
  } else if (test == "RFT1") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::RFT);
    cmd_scenario.mutable_scenario()->set_type_id(1);
  } else if (test == "None") {
    cmd_scenario.mutable_scenario()->set_type(raw_msgs::TestScenario::NONE);
    cmd_scenario.mutable_scenario()->set_type_id(0);
  }
  client.send(cmd_scenario);


  raw_msgs::SetTestTransitionEvent cmd_event;
  cmd_event.set_event(raw_msgs::SetTestTransitionEvent::RESET);
  client.send(cmd_event);
}


void on_cb_start_click()
{
  if (!client.connected()) return;

  raw_msgs::ConveyorBeltCommand msg;
  msg.set_command(raw_msgs::START);
  client.send(msg);
}


void on_cb_stop_click()
{
  if (!client.connected()) return;

  raw_msgs::ConveyorBeltCommand msg;
  msg.set_command(raw_msgs::STOP);
  client.send(msg);
}


int main(int argc, char **argv)
{
  llsfrb::YamlConfiguration config(CONFDIR);
  config.load("config.yaml");

  protobuf_comm::MessageRegister &message_register = client.message_register();
  message_register.add_message_type<raw_msgs::TestState>();


  Glib::RefPtr<Gtk::Application> app = Gtk::Application::create(argc, argv, "org.raw.controller");
  builder = Gtk::Builder::create_from_file(std::string(SRCDIR) + std::string("/raw_controller.glade"));

  Gtk::Window *window = 0;
  builder->get_widget("window1", window);
  window->set_title("RoboCup@Work RefboxController");
  window->show_all();

  Gtk::Button *button_start = 0;
  Gtk::Button *button_pause = 0;
  Gtk::Button *button_stop = 0;
  Gtk::Button *button_reset = 0;
  Gtk::Button *button_cb_start = 0;
  Gtk::Button *button_cb_stop = 0;
  builder->get_widget("button_start", button_start);
  builder->get_widget("button_pause", button_pause);
  builder->get_widget("button_stop", button_stop);
  builder->get_widget("button_reset", button_reset);
  builder->get_widget("button_cb_start", button_cb_start);
  builder->get_widget("button_cb_stop", button_cb_stop);

  Glib::signal_idle().connect(sigc::ptr_fun(&idle_handler));
  button_start->signal_clicked().connect(sigc::ptr_fun(&on_start_click));
  button_pause->signal_clicked().connect(sigc::ptr_fun(&on_pause_click));
  button_stop->signal_clicked().connect(sigc::ptr_fun(&on_stop_click));
  button_reset->signal_clicked().connect(sigc::ptr_fun(&on_reset_click));
  button_cb_start->signal_clicked().connect(sigc::ptr_fun(&on_cb_start_click));
  button_cb_stop->signal_clicked().connect(sigc::ptr_fun(&on_cb_stop_click));

  client.signal_received().connect(handle_message);
  client.signal_disconnected().connect(handle_disconnect);
  host = config.get_string("/llsfrb/shell/refbox-host");
  port = config.get_uint("/llsfrb/shell/refbox-port");
  client.async_connect(host.c_str(), port);

  return app->run(*window);
}
