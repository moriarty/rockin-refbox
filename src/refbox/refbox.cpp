
/***************************************************************************
 *  refbox.cpp - LLSF RefBox main program
 *
 *  Created: Thu Feb 07 11:04:17 2013
 *  Copyright  2013  Tim Niemueller [www.niemueller.de]
 ****************************************************************************/

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

#include "refbox.h"
#include "clips_logger.h"

#include <core/threading/mutex.h>
#include <core/version.h>
#include <config/yaml.h>
#include <protobuf_clips/communicator.h>
#include <protobuf_comm/asio/peer.h>
#include <logging/multi.h>
#include <logging/file.h>
#include <logging/network.h>
#include <logging/console.h>
#include <aspect/manager.h>
#include <baseapp/thread_manager.h>
#include <plugin/manager.h>

#include <boost/bind.hpp>
#include <boost/format.hpp>
#if BOOST_ASIO_VERSION < 100601
#  include <csignal>
#endif
#ifdef HAVE_MONGODB
#  include <mongo/client/dbclient.h>
#  include <mongodb_log/mongodb_log_logger.h>
#  include <mongodb_log/mongodb_log_protobuf.h>
#endif
#ifdef HAVE_AVAHI
#  include <netcomm/dns-sd/avahi_thread.h>
#  include <netcomm/utils/resolver.h>
#endif

using namespace protobuf_comm;
using namespace protobuf_clips;

namespace llsfrb {
#if 0 /* just to make Emacs auto-indent happy */
}
#endif

#if BOOST_ASIO_VERSION < 100601
LLSFRefBox *g_refbox = NULL;
static void handle_signal(int signum)
{
  if (g_refbox) {
    g_refbox->handle_signal(boost::system::errc::make_error_code(boost::system::errc::success),
			    signum);
  }
}
#endif

/** @class LLSFRefBox "refbox.h"
 * LLSF referee box main application.
 * @author Tim Niemueller
 */ 

/** Constructor.
 * @param argc number of arguments passed
 * @param argv array of arguments
 */
LLSFRefBox::LLSFRefBox(int argc, char **argv)
  : clips_mutex_(fawkes::Mutex::RECURSIVE), timer_(io_service_)
{
  pb_comm_ = NULL;

  config_ = new YamlConfiguration(CONFDIR);
  config_->load("config.yaml");

  try {
    cfg_timer_interval_ = config_->get_uint("/llsfrb/clips/timer-interval");
  } catch (fawkes::Exception &e) {
    delete config_;
    throw;
  }

  log_level_ = Logger::LL_INFO;
  try {
    std::string ll = config_->get_string("/llsfrb/log/level");
    if (ll == "debug") {
      log_level_ = Logger::LL_DEBUG;
    } else if (ll == "info") {
      log_level_ = Logger::LL_INFO;
    } else if (ll == "warn") {
      log_level_ = Logger::LL_WARN;
    } else if (ll == "error") {
      log_level_ = Logger::LL_ERROR;
    }
  } catch (fawkes::Exception &e) {} // ignored, use default

  MultiLogger *mlogger = new MultiLogger();
  mlogger->add_logger(new ConsoleLogger(log_level_));
  try {
    std::string logfile = config_->get_string("/llsfrb/log/general");
    mlogger->add_logger(new FileLogger(logfile.c_str(), log_level_));
  } catch (fawkes::Exception &e) {} // ignored, use default
  logger_ = mlogger;

  try {
    cfg_clips_dir_ = config_->get_string("/llsfrb/clips/dir");
    cfg_clips_dir_ = modify_directory_path(cfg_clips_dir_);
  } catch (fawkes::Exception &e) {
    // when the configuration is not set, default to a hard-coded directory
    cfg_clips_dir_ = std::string(SRCDIR) + "/clips/";
  };
  logger_->log_info("RefBox", "Using CLIPS rules from: %s", cfg_clips_dir_.c_str());

  clips_ = new CLIPS::Environment();
  setup_protobuf_comm();
  setup_clips();

  mlogger->add_logger(new NetworkLogger(pb_comm_->server(), log_level_));

  // Set up the plugin infrastructure
  aspect_manager_ = new fawkes::AspectManager();
  thread_manager_ = new fawkes::ThreadManager(aspect_manager_, aspect_manager_);
  plugin_manager_ = new fawkes::PluginManager(thread_manager_, config_,
      "/llsfrb/meta_plugins/", fawkes::Module::MODULE_FLAGS_DEFAULT, false);

  aspect_manager_->register_default_inifins(config_, logger_, clips_,
      &clips_mutex_, pb_comm_);

  std::string plugins = "";
  try {
    plugins = config_->get_string("llsfrb/plugins");
  } catch (fawkes::Exception &e) {
    // no plugins configuration parameter specified, do nothing
  }
  plugin_manager_->load(plugins.c_str());

 #ifdef HAVE_MONGODB
  cfg_mongodb_enabled_ = false;
  try {
    cfg_mongodb_enabled_ = config_->get_bool("/llsfrb/mongodb/enable");
  } catch (fawkes:: Exception &e) {} // ignore, use default

  if (cfg_mongodb_enabled_) {
    cfg_mongodb_hostport_     = config_->get_string("/llsfrb/mongodb/hostport");
    std::string mdb_text_log  = config_->get_string("/llsfrb/mongodb/collections/text-log");
    std::string mdb_clips_log = config_->get_string("/llsfrb/mongodb/collections/clips-log");
    std::string mdb_protobuf  = config_->get_string("/llsfrb/mongodb/collections/protobuf");
    mlogger->add_logger(new MongoDBLogLogger(cfg_mongodb_hostport_, mdb_text_log));

    clips_logger_->add_logger(new MongoDBLogLogger(cfg_mongodb_hostport_, mdb_clips_log));

    mongodb_protobuf_ = new MongoDBLogProtobuf(cfg_mongodb_hostport_, mdb_protobuf);

    
    mongo::DBClientConnection *conn =
      new mongo::DBClientConnection(/* auto reconnect */ true);
    mongodb_ = conn;
    std::string errmsg;
    if (! conn->connect(cfg_mongodb_hostport_, errmsg)) {
      throw fawkes::Exception("Could not connect to MongoDB at %s: %s",
			      cfg_mongodb_hostport_.c_str(), errmsg.c_str());
    }

    setup_clips_mongodb();

    pb_comm_->server()->signal_received()
      .connect(boost::bind(&LLSFRefBox::handle_server_client_msg, this, _1, _2, _3, _4));
    pb_comm_->server()->signal_receive_failed()
      .connect(boost::bind(&LLSFRefBox::handle_server_client_fail, this, _1, _2, _3, _4));

    const std::map<long int, protobuf_comm::ProtobufBroadcastPeer *> &peers =
      pb_comm_->peers();
    for (auto p : peers) {
      p.second->signal_received()
	.connect(boost::bind(&LLSFRefBox::handle_peer_msg, this, _1, _2, _3, _4));
    }

    pb_comm_->signal_server_sent()
      .connect(boost::bind(&LLSFRefBox::handle_server_sent_msg, this, _1, _2));
    pb_comm_->signal_client_sent()
      .connect(boost::bind(&LLSFRefBox::handle_client_sent_msg, this, _1, _2, _3));
    pb_comm_->signal_peer_sent()
      .connect(boost::bind(&LLSFRefBox::handle_peer_sent_msg, this, _2));

  }
#endif

  start_clips();

#ifdef HAVE_AVAHI
  unsigned int refbox_port = config_->get_uint("/llsfrb/comm/server-port");
  avahi_thread_ = new fawkes::AvahiThread();
  avahi_thread_->start();
  nnresolver_   = new fawkes::NetworkNameResolver(avahi_thread_);
  fawkes::NetworkService *refbox_service =
    new fawkes::NetworkService(nnresolver_, "RefBox on %h", "_refbox._tcp", refbox_port);
  avahi_thread_->publish_service(refbox_service);
  delete refbox_service;
#endif

}

/** Destructor. */
LLSFRefBox::~LLSFRefBox()
{
  timer_.cancel();

#ifdef HAVE_AVAHI
  avahi_thread_->cancel();
  avahi_thread_->join();
  delete avahi_thread_;
  delete nnresolver_;
#endif

  //std::lock_guard<std::recursive_mutex> lock(clips_mutex_);
  {
    fawkes::MutexLocker lock(&clips_mutex_);
    clips_->assert_fact("(finalize)");
    clips_->refresh_agenda();
    clips_->run();

    finalize_clips_logger(clips_->cobj());
  }

  delete pb_comm_;
  delete config_;
  delete clips_;
  delete logger_;
  delete clips_logger_;

  // Delete all global objects allocated by libprotobuf
  google::protobuf::ShutdownProtobufLibrary();
}


void
LLSFRefBox::setup_protobuf_comm()
{
  try {
    std::vector<std::string> proto_dirs;
    try {
      proto_dirs = config_->get_strings("/llsfrb/comm/protobuf-dirs");
      if (proto_dirs.size() > 0) {
	for (size_t i = 0; i < proto_dirs.size(); ++i) {
	  proto_dirs[i] = modify_directory_path(proto_dirs[i]);
	  //logger_->log_warn("RefBox", "DIR: %s", proto_dirs[i].c_str());
	}
      }
    } catch (fawkes::Exception &e) {} // ignore, use default

    if (proto_dirs.empty()) {
      pb_comm_ = new ClipsProtobufCommunicator(clips_, clips_mutex_);
    } else {
      pb_comm_ = new ClipsProtobufCommunicator(clips_, clips_mutex_, proto_dirs);
    }

    pb_comm_->enable_server(config_->get_uint("/llsfrb/comm/server-port"));

    MessageRegister &mr_server = pb_comm_->message_register();
    if (! mr_server.load_failures().empty()) {
      MessageRegister::LoadFailMap::const_iterator e = mr_server.load_failures().begin();
      std::string errstr = e->first + " (" + e->second + ")";
      for (++e; e != mr_server.load_failures().end(); ++e) {
	errstr += std::string(", ") + e->first + " (" + e->second + ")";
      }
      logger_->log_warn("RefBox", "Failed to load some message types: %s", errstr.c_str());
    }

  } catch (std::runtime_error &e) {
    delete config_;
    delete pb_comm_;
    throw;
  }
}

void
LLSFRefBox::setup_clips()
{
  fawkes::MutexLocker lock(&clips_mutex_);

  logger_->log_info("RefBox", "Creating CLIPS environment");
  MultiLogger *mlogger = new MultiLogger();
  mlogger->add_logger(new ConsoleLogger(log_level_));
  try {
    std::string logfile = config_->get_string("/llsfrb/log/clips");
    mlogger->add_logger(new FileLogger(logfile.c_str(), Logger::LL_DEBUG));
  } catch (fawkes::Exception &e) {} // ignored, use default

  clips_logger_ = mlogger;

  init_clips_logger(clips_->cobj(), logger_, clips_logger_);

  std::string defglobal_ver =
    boost::str(boost::format("(defglobal\n"
			     "  ?*VERSION-MAJOR* = %u\n"
			     "  ?*VERSION-MINOR* = %u\n"
			     "  ?*VERSION-MICRO* = %u\n"
			     ")")
	       % FAWKES_VERSION_MAJOR
	       % FAWKES_VERSION_MINOR
	       % FAWKES_VERSION_MICRO);

  clips_->build(defglobal_ver);

  clips_->add_function("get-clips-dirs", sigc::slot<CLIPS::Values>(sigc::mem_fun(*this, &LLSFRefBox::clips_get_clips_dirs)));
  clips_->add_function("now", sigc::slot<CLIPS::Values>(sigc::mem_fun(*this, &LLSFRefBox::clips_now)));
  clips_->add_function("load-config", sigc::slot<void, std::string>(sigc::mem_fun(*this, &LLSFRefBox::clips_load_config)));

  clips_->signal_periodic().connect(sigc::mem_fun(*this, &LLSFRefBox::handle_clips_periodic));

}

void
LLSFRefBox::start_clips()
{
  fawkes::MutexLocker lock(&clips_mutex_);

  if (!clips_->batch_evaluate(cfg_clips_dir_ + "init.clp")) {
    logger_->log_warn("RefBox", "Failed to initialize CLIPS environment, batch file failed.");
    throw fawkes::Exception("Failed to initialize CLIPS environment, batch file failed.");
  }  

  clips_->assert_fact("(init)");
  clips_->refresh_agenda();
  clips_->run();
}

void
LLSFRefBox::handle_clips_periodic()
{
  std::queue<int> to_erase;
  std::map<long int, CLIPS::Fact::pointer>::iterator f;

  for (f = clips_msg_facts_.begin(); f != clips_msg_facts_.end(); ++f) {
    if (f->second->refcount() == 1) {
      //logger_->log_info("RefBox", "Fact %li can be erased", f->second->index());
      to_erase.push(f->first);
    }
  }
  while (! to_erase.empty()) {
    long int index = to_erase.front();
    CLIPS::Fact::pointer &f = clips_msg_facts_[index];
    CLIPS::Value v = f->slot_value("ptr")[0];
    void *ptr = v.as_address();
    delete static_cast<std::shared_ptr<google::protobuf::Message> *>(ptr);
    clips_msg_facts_.erase(index);
    to_erase.pop();
  }
}


CLIPS::Values
LLSFRefBox::clips_now()
{
  CLIPS::Values rv;
  struct timeval tv;
  gettimeofday(&tv, 0);
  rv.push_back(tv.tv_sec);
  rv.push_back(tv.tv_usec);
  return rv;
}


CLIPS::Values
LLSFRefBox::clips_get_clips_dirs()
{
  CLIPS::Values rv;
  rv.push_back(cfg_clips_dir_);
  return rv;
}

void
LLSFRefBox::clips_load_config(std::string cfg_prefix)
{
  std::auto_ptr<Configuration::ValueIterator> v(config_->search(cfg_prefix.c_str()));
  while (v->next()) {
    std::string type = "";
    std::string value = v->get_as_string();

    if      (v->is_uint())   type = "UINT";
    else if (v->is_int())    type = "INT";
    else if (v->is_float())  type = "FLOAT";
    else if (v->is_bool())   type = "BOOL";
    else if (v->is_string()) {
      type = "STRING";
      if (! v->is_list()) {
	value = std::string("\"") + value + "\"";
      }
    } else {
      logger_->log_warn("RefBox", "Config value at '%s' of unknown type '%s'",
	     v->path(), v->type());
    }

    if (v->is_list()) {
      //logger_->log_info("RefBox", "(confval (path \"%s\") (type %s) (is-list TRUE) (list-value %s))",
      //       v->path(), type.c_str(), value.c_str());
      clips_->assert_fact_f("(confval (path \"%s\") (type %s) (is-list TRUE) (list-value %s))",
			    v->path(), type.c_str(), value.c_str());
    } else {
      //logger_->log_info("RefBox", "(confval (path \"%s\") (type %s) (value %s))",
      //       v->path(), type.c_str(), value.c_str());
      clips_->assert_fact_f("(confval (path \"%s\") (type %s) (value %s))",
			    v->path(), type.c_str(), value.c_str());
    }
  }
}


#ifdef HAVE_MONGODB

/** Handle message that came from a client.
 * @param client client ID
 * @param component_id component the message was addressed to
 * @param msg_type type of the message
 * @param msg the message
 */
void
LLSFRefBox::handle_server_client_msg(ProtobufStreamServer::ClientID client,
				     uint16_t component_id, uint16_t msg_type,
				     std::shared_ptr<google::protobuf::Message> msg)
{
  mongo::BSONObjBuilder meta;
  meta.append("direction", "inbound");
  meta.append("via", "server");
  meta.append("component_id", component_id);
  meta.append("msg_type", msg_type);
  meta.append("client_id", client);
  mongo::BSONObj meta_obj(meta.obj());
  mongodb_protobuf_->write(*msg, meta_obj);
}

/** Handle message that came from a client.
 * @param client client ID
 * @param component_id component the message was addressed to
 * @param msg_type type of the message
 * @param msg the message
 */
void
LLSFRefBox::handle_peer_msg(boost::asio::ip::udp::endpoint &endpoint,
			    uint16_t component_id, uint16_t msg_type,
			    std::shared_ptr<google::protobuf::Message> msg)
{
  mongo::BSONObjBuilder meta;
  meta.append("direction", "inbound");
  meta.append("via", "peer");
  meta.append("endpoint-host", endpoint.address().to_string());
  meta.append("endpoint-port", endpoint.port());
  meta.append("component_id", component_id);
  meta.append("msg_type", msg_type);
  mongo::BSONObj meta_obj(meta.obj());
  mongodb_protobuf_->write(*msg, meta_obj);
}

/** Handle server reception failure
 * @param client client ID
 * @param component_id component the message was addressed to
 * @param msg_type type of the message
 * @param msg the message string
 */
void
LLSFRefBox::handle_server_client_fail(ProtobufStreamServer::ClientID client,
				      uint16_t component_id, uint16_t msg_type,
				      std::string msg)
{
}


void
LLSFRefBox::add_comp_type(google::protobuf::Message &m, mongo::BSONObjBuilder *b)
{
  const google::protobuf::Descriptor *desc = m.GetDescriptor();
  const google::protobuf::EnumDescriptor *enumdesc = desc->FindEnumTypeByName("CompType");
  if (! enumdesc) return;
  const google::protobuf::EnumValueDescriptor *compdesc =
    enumdesc->FindValueByName("COMP_ID");
  const google::protobuf::EnumValueDescriptor *msgtdesc =
    enumdesc->FindValueByName("MSG_TYPE");
  if (! compdesc || ! msgtdesc)  return;
  int comp_id = compdesc->number();
  int msg_type = msgtdesc->number();
  b->append("component_id", comp_id);
  b->append("msg_type", msg_type);
}

/** Handle message that was sent to a server client.
 * @param client client ID
 * @param msg the message
 */
void
LLSFRefBox::handle_server_sent_msg(ProtobufStreamServer::ClientID client,
				   std::shared_ptr<google::protobuf::Message> msg)
{
  mongo::BSONObjBuilder meta;
  meta.append("direction", "outbound");
  meta.append("via", "server");
  meta.append("client_id", client);
  add_comp_type(*msg, &meta);
  mongo::BSONObj meta_obj(meta.obj());
  mongodb_protobuf_->write(*msg, meta_obj);
}

/** Handle message that was sent with a client.
 * @param host host of the endpoint sent to
 * @param port port of the endpoint sent to
 * @param msg the message
 */
void
LLSFRefBox::handle_client_sent_msg(std::string host, unsigned short int port,
				   std::shared_ptr<google::protobuf::Message> msg)
{
  mongo::BSONObjBuilder meta;
  meta.append("direction", "outbound");
  meta.append("via", "client");
  meta.append("host", host);
  meta.append("port", port);
  add_comp_type(*msg, &meta);
  mongo::BSONObj meta_obj(meta.obj());
  mongodb_protobuf_->write(*msg, meta_obj);
}

/** Setup MongoDB related CLIPS functions. */
void
LLSFRefBox::setup_clips_mongodb()
{
  fawkes::MutexLocker lock(&clips_mutex_);

  clips_->add_function("bson-create", sigc::slot<CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_create)));
  clips_->add_function("bson-parse", sigc::slot<CLIPS::Value, std::string>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_parse)));
  clips_->add_function("bson-destroy", sigc::slot<void, void *>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_destroy)));
  clips_->add_function("bson-append", sigc::slot<void, void *, std::string, CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_append)));
  clips_->add_function("bson-append-array", sigc::slot<void, void *, std::string, CLIPS::Values>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_append_array)));
  clips_->add_function("bson-array-start", sigc::slot<CLIPS::Value, void *, std::string>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_array_start)));
  clips_->add_function("bson-array-finish", sigc::slot<void, void *>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_array_finish)));
  clips_->add_function("bson-array-append", sigc::slot<void, void *, CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_array_append)));

  clips_->add_function("bson-append-time", sigc::slot<void, void *, std::string, CLIPS::Values>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_append_time)));
  clips_->add_function("bson-tostring", sigc::slot<std::string, void *>(sigc::mem_fun(*this, &LLSFRefBox::clips_bson_tostring)));
  clips_->add_function("mongodb-insert", sigc::slot<void, std::string, void *>(sigc::mem_fun(*this, &LLSFRefBox::clips_mongodb_insert)));
  clips_->add_function("mongodb-upsert", sigc::slot<void, std::string, void *, CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_mongodb_upsert)));
  clips_->add_function("mongodb-update", sigc::slot<void, std::string, void *, CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_mongodb_update)));
  clips_->add_function("mongodb-replace", sigc::slot<void, std::string, void *, CLIPS::Value>(sigc::mem_fun(*this, &LLSFRefBox::clips_mongodb_replace)));

  clips_->build("(deffacts have-feature-mongodb (have-feature MongoDB))");
}

/** Handle message that was sent to a server client.
 * @param client client ID
 * @param msg the message
 */
void
LLSFRefBox::handle_peer_sent_msg(std::shared_ptr<google::protobuf::Message> msg)
{
  mongo::BSONObjBuilder meta;
  meta.append("direction", "outbound");
  meta.append("via", "peer");
  add_comp_type(*msg, &meta);
  mongo::BSONObj meta_obj(meta.obj());
  mongodb_protobuf_->write(*msg, meta_obj);
}


CLIPS::Value
LLSFRefBox::clips_bson_create()
{
  mongo::BSONObjBuilder *b = new mongo::BSONObjBuilder();
  return CLIPS::Value(b);
}

CLIPS::Value
LLSFRefBox::clips_bson_parse(std::string document)
{
  mongo::BSONObjBuilder *b = new mongo::BSONObjBuilder();
  try {
    b->appendElements(mongo::fromjson(document));
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_error("MongoDB", "Parsing JSON doc failed: %s\n%s",
		       e.what(), document.c_str());
  }
  return CLIPS::Value(b);
}

void
LLSFRefBox::clips_bson_destroy(void *bson)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
  delete b;
}

std::string
LLSFRefBox::clips_bson_tostring(void *bson)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
  return b->asTempObj().jsonString(mongo::Strict, true);
}

void
LLSFRefBox::clips_bson_append(void *bson, std::string field_name, CLIPS::Value value)
{
  try {
    mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
    switch (value.type()) {
    case CLIPS::TYPE_FLOAT:
      b->append(field_name, value.as_float());
      break;

    case CLIPS::TYPE_INTEGER:
      b->append(field_name, value.as_integer());
      break;

    case CLIPS::TYPE_SYMBOL:
    case CLIPS::TYPE_STRING:
    case CLIPS::TYPE_INSTANCE_NAME:
      b->append(field_name, value.as_string());
      break;

    case CLIPS::TYPE_EXTERNAL_ADDRESS:
      {
	mongo::BSONObjBuilder *subb = static_cast<mongo::BSONObjBuilder *>(value.as_address());
	b->append(field_name, subb->asTempObj());
      }
      break;

    default:
      logger_->log_warn("RefBox", "Tried to add unknown type to BSON field %s",
			field_name.c_str());
      break;
    }
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_error("MongoDB", "Failed to append array value to field %s: %s",
		       field_name.c_str(), e.what());
  }
}


void
LLSFRefBox::clips_bson_append_array(void *bson,
				    std::string field_name, CLIPS::Values values)
{
  try {
    mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
    mongo::BSONArrayBuilder ab(b->subarrayStart(field_name));

    for (auto value : values) {
      switch (value.type()) {
      case CLIPS::TYPE_FLOAT:
	ab.append(value.as_float());
	break;

      case CLIPS::TYPE_INTEGER:
	ab.append(value.as_integer());
	break;
      
      case CLIPS::TYPE_SYMBOL:
      case CLIPS::TYPE_STRING:
      case CLIPS::TYPE_INSTANCE_NAME:
	ab.append(value.as_string());
	break;

      case CLIPS::TYPE_EXTERNAL_ADDRESS:
	{
	  mongo::BSONObjBuilder *subb =
	    static_cast<mongo::BSONObjBuilder *>(value.as_address());
	  ab.append(subb->asTempObj());
	}
	break;
      
      default:
	logger_->log_warn("MongoDB", "Tried to add unknown type to BSON array field %s",
			  field_name.c_str());
	break;
      }
    }
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_error("MongoDB", "Failed to append array value to field %s: %s",
		       field_name.c_str(), e.what());
  }
}

CLIPS::Value
LLSFRefBox::clips_bson_array_start(void *bson, std::string field_name)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
  mongo::BufBuilder &bb = b->subarrayStart(field_name);
  mongo::BSONArrayBuilder *arrb = new mongo::BSONArrayBuilder(bb);
  return CLIPS::Value(arrb);
}


void
LLSFRefBox::clips_bson_array_finish(void *barr)
{
  mongo::BSONArrayBuilder *ab = static_cast<mongo::BSONArrayBuilder *>(barr);
  delete ab;
}

void
LLSFRefBox::clips_bson_array_append(void *barr, CLIPS::Value value)
{
  try {
    mongo::BSONArrayBuilder *ab = static_cast<mongo::BSONArrayBuilder *>(barr);
    switch (value.type()) {
    case CLIPS::TYPE_FLOAT:
      ab->append(value.as_float());
      break;

    case CLIPS::TYPE_INTEGER:
      ab->append(value.as_integer());
      break;

    case CLIPS::TYPE_SYMBOL:
    case CLIPS::TYPE_STRING:
    case CLIPS::TYPE_INSTANCE_NAME:
      ab->append(value.as_string());
      break;

    case CLIPS::TYPE_EXTERNAL_ADDRESS:
      {
	mongo::BSONObjBuilder *subb = static_cast<mongo::BSONObjBuilder *>(value.as_address());
	ab->append(subb->asTempObj());
      }
      break;

    default:
      logger_->log_warn("RefBox", "Tried to add unknown type to BSON array");
      break;
    }
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_error("MongoDB", "Failed to append to array: %s", e.what());
  }
}


void
LLSFRefBox::clips_bson_append_time(void *bson, std::string field_name, CLIPS::Values time)
{
  if (time.size() != 2) {
    logger_->log_warn("MongoDB", "Invalid time, %zu instead of 2 entries", time.size());
    return;
  }
  if (time[0].type() != CLIPS::TYPE_INTEGER || time[1].type() != CLIPS::TYPE_INTEGER) {
    logger_->log_warn("MongoDB", "Invalid time, type mismatch");
    return;
  }

  try {
    mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
    struct timeval now = { time[0].as_integer(), time[1].as_integer()};
    mongo::Date_t nowd = now.tv_sec * 1000 + now.tv_usec / 1000;
    b->appendDate(field_name, nowd);
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_error("MongoDB", "Failed to append time value to field %s: %s",
		       field_name.c_str(), e.what());
  }
}

void
LLSFRefBox::clips_mongodb_insert(std::string collection, void *bson)
{
  if (! cfg_mongodb_enabled_) {
    logger_->log_warn("MongoDB", "Insert requested while MongoDB disabled");
    return;
  }

  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);

  try {
    mongodb_->insert(collection, b->obj());
  } catch (mongo::DBException &e) {
    logger_->log_warn("MongoDB", "Insert failed: %s", e.what());
  }
}


void
LLSFRefBox::mongodb_update(std::string &collection, mongo::BSONObj obj,
			   CLIPS::Value &query, bool upsert)
{
  if (! cfg_mongodb_enabled_) {
    logger_->log_warn("MongoDB", "Update requested while MongoDB disabled");
    return;
  }

  try {
    mongo::BSONObj query_obj;
    if (query.type() == CLIPS::TYPE_STRING) {
      query_obj = mongo::fromjson(query.as_string());
    } else if (query.type() == CLIPS::TYPE_EXTERNAL_ADDRESS) {
      mongo::BSONObjBuilder *qb = static_cast<mongo::BSONObjBuilder *>(query.as_address());
      query_obj = qb->asTempObj();
    } else {
      logger_->log_warn("MongoDB", "Invalid query, must be string or BSON document");
      return;
    }

    mongodb_->update(collection, query_obj, obj, upsert);
#if MONGODB_VERSION_MAJOR == 2 && MONGODB_VERSION_MINOR == 0
  } catch (bson::assertion &e) {
#else
  } catch (mongo::AssertionException &e) {
#endif
    logger_->log_warn("MongoDB", "Compiling query failed: %s", e.what());
  } catch (mongo::DBException &e) {
    logger_->log_warn("MongoDB", "Insert failed: %s", e.what());
  }
}


void
LLSFRefBox::clips_mongodb_upsert(std::string collection, void *bson, CLIPS::Value query)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
  mongodb_update(collection, b->asTempObj(), query, true);
}

void
LLSFRefBox::clips_mongodb_update(std::string collection, void *bson, CLIPS::Value query)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);

  mongo::BSONObjBuilder update_doc;
  update_doc.append("$set", b->asTempObj());

  mongodb_update(collection, update_doc.obj(), query, false);
}

void
LLSFRefBox::clips_mongodb_replace(std::string collection, void *bson, CLIPS::Value query)
{
  mongo::BSONObjBuilder *b = static_cast<mongo::BSONObjBuilder *>(bson);
  mongodb_update(collection, b->asTempObj(), query, false);
}


#endif


/** Start the timer for another run. */
void
LLSFRefBox::start_timer()
{
  timer_last_ = boost::posix_time::microsec_clock::local_time();
  timer_.expires_from_now(boost::posix_time::milliseconds(cfg_timer_interval_));
  timer_.async_wait(boost::bind(&LLSFRefBox::handle_timer, this,
				boost::asio::placeholders::error));
}

/** Handle timer event.
 * @param error error code
 */
void
LLSFRefBox::handle_timer(const boost::system::error_code& error)
{
  if (! error) {
    /*
    boost::posix_time::ptime now = boost::posix_time::microsec_clock::local_time();
    long ms = (now - timer_last_).total_milliseconds();
    timer_last_ = now;
    */

    {
      //std::lock_guard<std::recursive_mutex> lock(clips_mutex_);
      fawkes::MutexLocker lock(&clips_mutex_);

      clips_->assert_fact("(time (now))");
      clips_->refresh_agenda();
      clips_->run();
    }

    timer_.expires_at(timer_.expires_at()
		      + boost::posix_time::milliseconds(cfg_timer_interval_));
    timer_.async_wait(boost::bind(&LLSFRefBox::handle_timer, this,
				  boost::asio::placeholders::error));
  }
}


/** Handle operating system signal.
 * @param error error code
 * @param signum signal number
 */
void
LLSFRefBox::handle_signal(const boost::system::error_code& error, int signum)
{
  timer_.cancel();
  io_service_.stop();
}



/** Run the application.
 * @return return code, 0 if no error, error code otherwise
 */
int
LLSFRefBox::run()
{
#if BOOST_ASIO_VERSION >= 100601
  // Construct a signal set registered for process termination.
  boost::asio::signal_set signals(io_service_, SIGINT, SIGTERM);

  // Start an asynchronous wait for one of the signals to occur.
  signals.async_wait(boost::bind(&LLSFRefBox::handle_signal, this,
				 boost::asio::placeholders::error,
				 boost::asio::placeholders::signal_number));
#else
  g_refbox = this;
  signal(SIGINT, llsfrb::handle_signal);
#endif

  start_timer();
  io_service_.run();
  return 0;
}


/** Modify the path of a directory.
 * The method replace the variables @BASEDIR@, @RESDIR@, @CONFDIR@ by the
 * associated locations which have been defined at compile time. Additionally,
 * if missing, a slash is appended.
 *
 * @param dir The raw input directory.
 * @return The parsed directory.
 */
std::string
LLSFRefBox::modify_directory_path(std::string dir)
{
  std::string::size_type pos;

  if ((pos = dir.find("@BASEDIR@")) != std::string::npos) {
    dir.replace(pos, 9, BASEDIR);
  }
  if ((pos = dir.find("@RESDIR@")) != std::string::npos) {
    dir.replace(pos, 8, RESDIR);
  }
  if ((pos = dir.find("@CONFDIR@")) != std::string::npos) {
    dir.replace(pos, 9, CONFDIR);
  }

  if (!dir.empty() && dir[dir.size()-1] != '/') {
    dir += "/";
  }

  return dir;
}

} // end of namespace llsfrb
