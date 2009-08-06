// Low-level interface to FastCGI.
// Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal
 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
 
// You should have received a copy of the GNU General Public License along
// with this program; If not, see <http://www.gnu.org/licenses/>.
  
// Please contact Vijay Mathew Pandyalakal if you need additional 
// information or have any questions.
// (Electronic mail: vijay.the.schemer@gmail.com)

/*
 * The FastCGI Classes.
 * Copyright (c) 2001-2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include "spark.h"
#include "spark_fcgi.h"
using namespace spark_fcgi;
#include "spark_socket.h"
using namespace spark_socket;

#include <cstring>
#include <cassert>
#include <sys/socket.h>
#include <fcntl.h>

static const char* MODULE_NAME = "#%spark-fcgi";

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);
static FCGIRequest* _scheme_object_to_fcgi_request(Scheme_Object* obj);
static std::string _last_error;

spark::Status_code
spark_fcgi::initialize(Scheme_Env* env)
{
  Scheme_Object* module_symbol = NULL;
  Scheme_Env* new_env = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, module_symbol);
  MZ_GC_VAR_IN_REG(1, new_env);
  MZ_GC_REG();

  module_symbol = scheme_intern_symbol(MODULE_NAME);
  assert_scheme_object(module_symbol, "scheme_intern_symbol");
  new_env = scheme_primitive_module(module_symbol, env);
  assert_scheme_object(new_env, "scheme_primitive_module");
  spark::Status_code status = spark::SUCCESS;
  if ((status = _add_constants(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  if ((status = _add_procedures(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  scheme_finish_primitive_module(new_env);
  scheme_protect_primitive_provide(new_env, 0);

  MZ_GC_UNREG();
  return spark::SUCCESS;
}

// exported function signatures
namespace spark_fcgi
{
  static Scheme_Object* recv(int, Scheme_Object**);
  static Scheme_Object* send(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_id(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_role(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_keep_connection(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_aborted(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_param_keys(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_param_value(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_stdin_stream(int, Scheme_Object**);
  static Scheme_Object* fcgi_request_data_stream(int, Scheme_Object**);
  static Scheme_Object* close(int, Scheme_Object**);
  static Scheme_Object* delete_fcgi_request(int, Scheme_Object**);
  static Scheme_Object* get_last_error(int, Scheme_Object**);
} // namespace spark_fcgi

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("FCGI-REQUEST-COMPLETE", FCGIRequest::REQUEST_COMPLETE),
    Constant("FCGI-CANT-MPX-CONN", FCGIRequest::CANT_MPX_CONN),
    Constant("FCGI-OVERLOADED", FCGIRequest::OVERLOADED),
    Constant("FCGI-UNKNOWN-ROLE", FCGIRequest::UNKNOWN_ROLE),
    Constant("FCGI-RESPONDER", FCGIRequest::RESPONDER),
    Constant("FCGI-FILTER", FCGIRequest::FILTER),
    Constant("FCGI-AUTHORIZER", FCGIRequest::AUTHORIZER),
    Constant()
  };  
  return spark::add_constants(env, constants, "spark-fcgi");
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fcgi::recv, "recv", 1),
    new Procedure(spark_fcgi::send, "send", 2, 5),
    new Procedure(spark_fcgi::fcgi_request_id, "fcgi-request-id", 1),
    new Procedure(spark_fcgi::fcgi_request_role, "fcgi-request-role", 1),
    new Procedure(spark_fcgi::fcgi_request_keep_connection, "fcgi-request-keep-connection", 1),
    new Procedure(spark_fcgi::fcgi_request_aborted, "fcgi-request-aborted", 1),
    new Procedure(spark_fcgi::fcgi_request_param_keys, "fcgi-request-param-keys", 1),
    new Procedure(spark_fcgi::fcgi_request_param_value, "fcgi-request-param-value", 2),
    new Procedure(spark_fcgi::fcgi_request_stdin_stream, "fcgi-request-stdin-stream", 1),
    new Procedure(spark_fcgi::fcgi_request_data_stream, "fcgi-request-data-stream", 1),
    new Procedure(spark_fcgi::close, "close", 1),
    new Procedure(spark_fcgi::delete_fcgi_request, "delete-fcgi-request", 1),
    new Procedure(spark_fcgi::get_last_error, "get-last-error", 0),
    0
  };
  
  return spark::add_procedures(env, procedures, "spark-fcgi");
}

class ConnectionHandler :  public FCGIProtocolDriver::OutputCallback
{
public:
  ConnectionHandler(Socket sock) : _sock(sock) { }
private:
  virtual void operator() (void const* buf, size_t count)
  {
    char* buffer = reinterpret_cast<char*>(const_cast<void*>(buf));
    int rc = ::send(_sock, buffer, count, 0);
    while (rc < count)
      {
	count -= rc;
	if (count <= 0)
	  break;
	buffer += rc;
	rc = ::send(_sock, buffer, count, 0);
      }
    if (rc == -1)
      throw fcgi_io_callback_error("Failed to write response.");
  }
private:
  Socket _sock;
}; // class ConnectionHandler :  public FCGIProtocolDriver::OutputCallback

class RequestHandler : public FCGIRequest::handler
{
public:
  RequestHandler(const std::string& data,
		 FCGIRequest::protocol_status_t status,
		 bool error = false,
		 const std::string& content_type = "text/html")
    : _data(data), _status(status), 
      _error(error), _content_type(content_type)
  { }

private:
  virtual void operator()(FCGIRequest* req)
  {
    if (!req->stdin_eof)
      return;
    
    int ret = 0;
    if (_error)
      {	
	req->write(_data.c_str(), _data.length(), FCGIRequest::STDERR);
	ret = 1;
      }
    else
      {
	std::ostringstream os;
	os << "Content-type: " << _content_type << "\r\n"
	   << "\r\n"
	   << _data
	   << std::endl;
	req->write(os.str().c_str(), os.str().size());
      }
    req->end_request(ret, _status);    
  }
private:
  std::string _data;
  FCGIRequest::protocol_status_t _status;
  std::string _content_type;
  bool _error;
}; // class RequestHandler : public FCGIRequest::handler

#define FCGI_REQUEST_TAG 300

// Exported fastcgi API

Scheme_Object*
spark_fcgi::recv(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  if (fcntl(sock, F_SETFL, O_NONBLOCK) == -1)
    scheme_signal_error("Failed to set socket to non-blocking mode.");
  char* data = 0;
  const int buff_size = 1024 * 4;
  char* buffer = new char[buff_size + 1];
  int r = ::recv(sock, buffer, buff_size, 0);
  int buff_count = 0;
  while (r > 0)
    {
      int old_i = buff_count;
      buff_count += r;
      data = (char*)realloc(data, buff_count);
      int j = 0;
      for (int i=old_i; i<r; ++i)
	data[i] = buffer[j++];
      delete[] buffer;
      buffer = new char[buff_size + 1];
      r = ::recv(sock, buffer, buff_size, 0);
    }
  delete[] buffer;
  try
    {
      ConnectionHandler* handler = new ConnectionHandler(sock);
      FCGIProtocolDriver* driver = new FCGIProtocolDriver(handler);
      driver->process_input(data, buff_count); 
      FCGIRequest* request = driver->get_request();
      if (request)
	{
	  request->driver_ptr = driver;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(FCGI_REQUEST_TAG);
	    _ret_ = scheme_make_cptr(request, tag);
	    MZ_GC_UNREG();
	  }
	}
    }
  catch (fcgi_error& e)
    {
      _last_error = e.what();
      _ret_ = scheme_make_symbol("error");
    }
  catch (...)
    {
      _last_error = "unknown error";
      _ret_ = scheme_make_symbol("error");
    }
  if (data)
    free(data);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fcgi::send(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (request == 0)
    {
      scheme_wrong_type("send", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  std::string data;
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      data = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      scheme_wrong_type("send", "string", 1, argc, argv);
      DEFAULT_RET_FINISH;
    }

  FCGIRequest::protocol_status_t status = FCGIRequest::REQUEST_COMPLETE;
  if (argc >= 3)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[2], i))
	{
	  status = static_cast<FCGIRequest::protocol_status_t>(i);
	}
    }

  bool err = false;
  if (argc >= 4)
    {
      err = (argv[3] == scheme_true) ? true : false;
    }

  std::string content_type = "text/html";
  if (argc >= 5)
    {
      if (SCHEME_CHAR_STRINGP(argv[4]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
	  content_type = SCHEME_BYTE_STR_VAL(str);
	}
      else
	{
	  scheme_wrong_type("send", "string", 4, argc, argv);
	  DEFAULT_RET_FINISH;
	}
    }

  request->handler_cb = new RequestHandler(data, status, 
					   err, content_type);
  try 
    {
      request->handler_cb->operator()(request);
      _ret_ = scheme_true;
    }
  catch (fcgi_error& e)
    {
      _last_error = e.what();
      _ret_ = scheme_make_symbol("error");
    }
  catch (...)
    {
      _last_error = "unknown error";
      _ret_ = scheme_make_symbol("error");
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_id(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-id", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = scheme_make_integer(request->id);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_role(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-role", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  switch (request->role)
    {
    case FCGIRequest::RESPONDER:
      _ret_ = scheme_make_symbol("responder");
      break;
    case FCGIRequest::AUTHORIZER:
      _ret_ = scheme_make_symbol("authorizer");
      break;
    case FCGIRequest::FILTER:
      _ret_ = scheme_make_symbol("filter");
      break;
    default:
      _ret_ = scheme_make_symbol("unknown");
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_keep_connection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-keep_connection", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = request->keep_connection ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_aborted(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-aborted", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = request->aborted ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_param_keys(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-param-keys", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  size_t count = request->params.size();
  if (count > 0)
    {
      Scheme_Object** elems = new Scheme_Object*[count];
      spark::String_map::const_iterator iter = request->params.begin();
      spark::String_map::const_iterator iter_end = request->params.end();
      int i = 0;
      while (iter != iter_end)
	{
	  Scheme_Object* obj = NULL;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, obj);
	  MZ_GC_REG();
	  obj = scheme_make_utf8_string(iter->first.c_str());
	  elems[i++] = obj;
	  iter++;
	  MZ_GC_UNREG();
	}
      _ret_ = scheme_build_list(count, elems);      
      delete[] elems;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_param_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-param-value", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  std::string key;
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      key = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      scheme_wrong_type("fcgi-request-param-value", "string", 1, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = scheme_make_utf8_string(request->params[key].c_str());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_stdin_stream(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-stdin-stream", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = scheme_make_utf8_string(request->stdin_stream.c_str());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::fcgi_request_data_stream(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (!request)
    {
      scheme_wrong_type("fcgi-request-data-stream", "FCGIRequest", 0, argc, argv);
      DEFAULT_RET_FINISH;
    }

  _ret_ = scheme_make_utf8_string(request->data_stream.c_str());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fcgi::close(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::delete_fcgi_request(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  FCGIRequest* request = _scheme_object_to_fcgi_request(argv[0]);
  if (request)
    {
      delete request->driver_ptr;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fcgi::get_last_error(int, Scheme_Object**)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_utf8_string(_last_error.c_str());

  DEFAULT_RET_FINISH;
}

FCGIRequest*
_scheme_object_to_fcgi_request(Scheme_Object* obj)
{
  if (!SCHEME_CPTRP(obj))
    return 0;

  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(obj);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    return 0;
  if (i != FCGI_REQUEST_TAG)
    return 0;
  void* p = SCHEME_CPTR_VAL(obj);
  if (!p)
    return 0;
  return reinterpret_cast<FCGIRequest*>(p);
}

// FastCGI API by Peter Simons

enum message_type_t
  { TYPE_BEGIN_REQUEST     =  1
    , TYPE_ABORT_REQUEST     =  2
    , TYPE_END_REQUEST       =  3
    , TYPE_PARAMS            =  4
    , TYPE_STDIN             =  5
    , TYPE_STDOUT            =  6
    , TYPE_STDERR            =  7
    , TYPE_DATA              =  8
    , TYPE_GET_VALUES        =  9
    , TYPE_GET_VALUES_RESULT = 10
    , TYPE_UNKNOWN           = 11
  };

struct Header
{
  uint8_t version;
  uint8_t type;
  uint8_t requestIdB1;
  uint8_t requestIdB0;
  uint8_t contentLengthB1;
  uint8_t contentLengthB0;
  uint8_t paddingLength;
  uint8_t reserved;

  Header()
  {
    memset(this, 0, sizeof(*this));
  }

  Header(message_type_t t, uint16_t id, uint16_t len)
    : version(1), type(t)
    , requestIdB1(id >> 8)
    , requestIdB0(id & 0xff)
    , contentLengthB1(len >> 8)
    , contentLengthB0(len & 0xff)
    , paddingLength(0), reserved(0)
  {
  }
};

struct BeginRequest
{
  uint8_t roleB1;
  uint8_t roleB0;
  uint8_t flags;
  uint8_t reserved[5];
};

static uint8_t const FLAG_KEEP_CONN = 1;

struct EndRequestMsg : public Header
{
  uint8_t appStatusB3;
  uint8_t appStatusB2;
  uint8_t appStatusB1;
  uint8_t appStatusB0;
  uint8_t protocolStatus;
  uint8_t reserved[3];

  EndRequestMsg()
  {
    memset(this, 0, sizeof(*this));
  }

  EndRequestMsg(uint16_t id, uint32_t appStatus, FCGIRequest::protocol_status_t protStatus)
    : Header(TYPE_END_REQUEST, id, sizeof(EndRequestMsg)-sizeof(Header))
    , appStatusB3((appStatus >> 24) & 0xff)
    , appStatusB2((appStatus >> 16) & 0xff)
    , appStatusB1((appStatus >>  8) & 0xff)
    , appStatusB0((appStatus >>  0) & 0xff)
    , protocolStatus(protStatus)
  {
    memset(this->reserved, 0, sizeof(this->reserved));
  }
};

struct UnknownTypeMsg : public Header
{
  uint8_t type;
  uint8_t reserved[7];

  UnknownTypeMsg()
  {
    memset(this, 0, sizeof(*this));
  }

  UnknownTypeMsg(uint8_t unknown_type)
    : Header(TYPE_UNKNOWN, 0, sizeof(UnknownTypeMsg) - sizeof(Header))
    , type(unknown_type)
  {
    memset(this->reserved, 0, sizeof(this->reserved));
  }
};

void FCGIProtocolDriver::process_unknown(uint8_t type)
{
  UnknownTypeMsg msg(type);
  output_cb->operator()(&msg, sizeof(UnknownTypeMsg));
}

void FCGIProtocolDriver::process_begin_request(uint16_t id, uint8_t const * buf, uint16_t)
{
  // Check whether we have an open request with that id already and
  // if, throw an exception.

  if (reqmap.find(id) != reqmap.end())
    {
      char tmp[256];
      sprintf(tmp, "FCGIProtocolDriver received duplicate BEGIN_REQUEST id %u.", id);
      throw duplicate_begin_request(tmp);
    }

  // Create a new request instance and store it away. The user may
  // get it after we've read all parameters.

  BeginRequest const * br = reinterpret_cast<BeginRequest const *>(buf);
  reqmap[id] = new FCGIRequest(*this, id,
			       FCGIRequest::role_t((br->roleB1 << 8) + br->roleB0),
			       (br->flags & FLAG_KEEP_CONN) == 1);
}

void FCGIProtocolDriver::process_abort_request(uint16_t id, uint8_t const *, uint16_t)
{
  // Find request instance for this id. Ignore message if non
  // exists, set ignore flag otherwise.

  reqmap_t::iterator req = reqmap.find(id);
  if (req == reqmap.end())
    std::cerr << "FCGIProtocolDriver received ABORT_REQUEST for non-existing id " << id << ". Ignoring."
              << std::endl;
  else
    {
      req->second->aborted = true;
      if (req->second->handler_cb) // Notify the handler associated with this request.
	(*req->second->handler_cb)(req->second);
    }
}

void FCGIProtocolDriver::process_params(uint16_t id, uint8_t const * buf, uint16_t len)
{
  // Find request instance for this id. Ignore message if non
  // exists.

  reqmap_t::iterator req = reqmap.find(id);
  if (req == reqmap.end())
    {
      std::cerr << "FCGIProtocolDriver received PARAMS for non-existing id " << id << ". Ignoring."
		<< std::endl;
      return;
    }

  // Is this the last message to come? Then queue the request for
  // the user.

  if (len == 0)
    {
      new_request_queue.push(id);
      return;
    }

  // Process message.

  uint8_t const * const  bufend(buf + len);
  uint32_t               name_len;
  uint32_t               data_len;
  while(buf != bufend)
    {
      if (*buf >> 7 == 0)
	name_len = *(buf++);
      else
	{
	  name_len = ((buf[0] & 0x7F) << 24) + (buf[1] << 16) + (buf[2] << 8) + buf[3];
	  buf += 4;
	}
      if (*buf >> 7 == 0)
	data_len = *(buf++);
      else
	{
	  data_len = ((buf[0] & 0x7F) << 24) + (buf[1] << 16) + (buf[2] << 8) + buf[3];
	  buf += 4;
	}
      assert(buf + name_len + data_len <= bufend);
      std::string const name(reinterpret_cast<char const *>(buf), name_len);
      buf += name_len;
      std::string const data(reinterpret_cast<char const *>(buf), data_len);
      buf += data_len;
#ifdef DEBUG_FASTCGI
      std::cerr << "request #" << id << ": FCGIProtocolDriver received PARAM '" << name << "' = '" << data << "'"
		<< std::endl;
#endif
      req->second->params[name] = data;
    }
}

void FCGIProtocolDriver::process_stdin(uint16_t id, uint8_t const * buf, uint16_t len)
{
  // Find request instance for this id. Ignore message if non
  // exists.

  reqmap_t::iterator req = reqmap.find(id);
  if (req == reqmap.end())
    {
      std::cerr << "FCGIProtocolDriver received STDIN for non-existing id " << id << ". Ignoring."
		<< std::endl;
      return;
    }

  // Is this the last message to come? Then set the eof flag.
  // Otherwise, add the data to the buffer in the request structure.

  if (len == 0)
    req->second->stdin_eof = true;
  else
    req->second->stdin_stream.append((char const *)buf, len);

  // Notify the handler associated with this request.

  if (req->second->handler_cb)
    (*req->second->handler_cb)(req->second);
}

FCGIProtocolDriver::FCGIProtocolDriver(OutputCallback* cb) : output_cb(cb)
{
}

FCGIProtocolDriver::~FCGIProtocolDriver()
{
  for(reqmap_t::iterator i = reqmap.begin(); i != reqmap.end(); ++i)
    {
      delete i->second;
    }
  delete output_cb;
}

void FCGIProtocolDriver::process_input(void const * buf, size_t count)
{
  // Copy data to our own buffer.

  InputBuffer.insert( InputBuffer.end()
		      , static_cast<uint8_t const *>(buf)
		      , static_cast<uint8_t const *>(buf) + count
		      );

  // If there is enough data in the input buffer to contain a
  // header, interpret it.

  while(InputBuffer.size() >= sizeof(Header))
    {
      Header const * hp = reinterpret_cast<Header const *>(&InputBuffer[0]);

      // Check whether our peer speaks the correct protocol version.

      if (hp->version != 1)
	{
	  char buf[256];
	  sprintf(buf, "FCGIProtocolDriver cannot handle protocol version %u.", hp->version);
	  throw unsupported_fcgi_version(buf);
	}

      // Check whether we have the whole message that follows the
      // headers in our buffer already. If not, we can't process it
      // yet.

      uint16_t msg_len = (hp->contentLengthB1 << 8) + hp->contentLengthB0;
      uint16_t msg_id  = (hp->requestIdB1 << 8) + hp->requestIdB0;

      if (InputBuffer.size() < sizeof(Header)+msg_len+hp->paddingLength)
	return;

      // Process the message. In case an exception arrives here,
      // terminate the request.

      try
	{
#ifdef DEBUG_FASTCGI
	  std::cerr << "Received message: id = " << msg_id << ", "
		    << "body len = " << msg_len << ", "
		    << "type = " << (int)hp->type << std::endl;
#endif
	  switch (hp->type)
	    {
	    case TYPE_BEGIN_REQUEST:
	      process_begin_request(msg_id, &InputBuffer[0]+sizeof(Header), msg_len);
	      break;

	    case TYPE_ABORT_REQUEST:
	      process_abort_request(msg_id, &InputBuffer[0]+sizeof(Header), msg_len);
	      break;

	    case TYPE_PARAMS:
	      process_params(msg_id, &InputBuffer[0]+sizeof(Header), msg_len);
	      break;

	    case TYPE_STDIN:
	      process_stdin(msg_id, &InputBuffer[0]+sizeof(Header), msg_len);
	      break;

	    case TYPE_END_REQUEST:
	    case TYPE_STDOUT:
	    case TYPE_STDERR:
	    case TYPE_DATA:
	    case TYPE_GET_VALUES:
	    case TYPE_GET_VALUES_RESULT:
	    case TYPE_UNKNOWN:
	    default:
	      process_unknown(hp->type);
	    }
	}
      catch(fcgi_io_callback_error const &)
	{
	  throw;
	}
      catch(std::exception const & e)
	{
	  std::cerr << "Caught exception while processing request #" << msg_id << ": " << e.what() << std::endl;
	  terminate_request(msg_id);
	}
      catch(...)
	{
	  std::cerr << "Caught unknown exception while processing request #" << msg_id << "." << std::endl;
	  terminate_request(msg_id);
	}

      // Remove the message from our buffer and contine processing
      // if there if something left.

      InputBuffer.erase( InputBuffer.begin()
			 , InputBuffer.begin()+sizeof(Header)+msg_len+hp->paddingLength
			 );
    }
}

FCGIRequest* FCGIProtocolDriver::get_request()
{
  if (new_request_queue.empty())
    return 0;

  FCGIRequest* r = reqmap[new_request_queue.front()];
  new_request_queue.pop();
  return r;
}

bool FCGIProtocolDriver::have_active_requests()
{
  if (new_request_queue.empty() && reqmap.empty())
    return false;
  else
    return true;
}

void FCGIProtocolDriver::terminate_request(uint16_t id)
{
  reqmap_t::iterator req;
  req = reqmap.find(id);
  if (req != reqmap.end())
    {
      delete req->second;
      reqmap.erase(req);
    }
}

// Pure virtual destructors must also exist somewhere.

FCGIProtocolDriver::OutputCallback::~OutputCallback()
{
}

FCGIRequest::FCGIRequest(FCGIProtocolDriver& driver_, uint16_t id_, role_t role_, bool kc)
  : id(id_), role(role_), keep_connection(kc), aborted(false), stdin_eof(false)
  , data_eof(false), handler_cb(0), driver(driver_)
{
}

FCGIRequest::~FCGIRequest()
{
  if (handler_cb)
    delete handler_cb;
}

void FCGIRequest::write(const std::string& buf, ostream_type_t stream)
{
  write(buf.data(), buf.size(), stream);
}

void FCGIRequest::write(char const * buf, size_t count, ostream_type_t stream)
{
  if (count > 0xffff)
    throw std::out_of_range("Can't send messages of that size.");
  else if (count == 0)
    return;

  // Construct message.

  Header h(stream == STDOUT ? TYPE_STDOUT : TYPE_STDERR, id, count);
  driver.output_cb->operator()(&h, sizeof(Header));
  driver.output_cb->operator()(buf, count);
}

void FCGIRequest::end_request(uint32_t appStatus, FCGIRequest::protocol_status_t protStatus)
{
  // Terminate the stdout and stderr stream, and send the
  // end-request message.

  uint8_t buf[64];
  uint8_t * p = buf;

  new(p) Header(TYPE_STDOUT, id, 0);
  p += sizeof(Header);
  new(p) Header(TYPE_STDERR, id, 0);
  p += sizeof(Header);
  new(p) EndRequestMsg(id, appStatus, protStatus);
  p += sizeof(EndRequestMsg);
  driver.output_cb->operator()(buf, p - buf);
  driver.terminate_request(id);
}


