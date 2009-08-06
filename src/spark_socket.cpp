// MzScheme inetrface to system-level socket API.
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

#include "spark.h"
#include "spark_socket.h"
using namespace spark_socket;

#include <vector>

/*#ifdef UNIX*/
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/poll.h>
#include <netinet/in.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/if.h>
/*#endif*/

static const char* MODULE_NAME = "#%spark-socket";

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);
static Scheme_Object* _socket_to_scheme_object(Socket sock);
static Scheme_Object* _make_address_info(const sockaddr_in& addr);
static bool _unpack_to_sockaddr_in(Scheme_Object* addr_info, 
				   const char* calling_func,
				   sockaddr_in& ret);
static Scheme_Object* _hostent_to_scheme_object(const hostent* addr);

spark::Status_code
spark_socket::initialize(Scheme_Env* env)
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

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("NONBLOCK", O_NONBLOCK),
    Constant("ASYNC", O_ASYNC),
    Constant("POLLIN", POLLIN),
    Constant("POLLOUT", POLLOUT),
    Constant("POLLPRI", POLLPRI),
    Constant("POLLERR", POLLERR),
    Constant("POLLHUP", POLLHUP),
    Constant("POLLNVAL", POLLNVAL),
    Constant("MSG-OOB", MSG_OOB),
    Constant("MSG-PEEK", MSG_PEEK),
    Constant("MSG-WAITALL", MSG_WAITALL),
    Constant("SO-KEEPALIVE", SO_KEEPALIVE),
    Constant("SO-OOBINLINE", SO_OOBINLINE),
    Constant("SO-RCVLOWAT", SO_RCVLOWAT),
    Constant("SO-SNDLOWAT", SO_SNDLOWAT),
    Constant("SO-BSDCOMPAT", SO_BSDCOMPAT),
    Constant("SO-PASSCRED", SO_PASSCRED),
    Constant("SO-DEBUG", SO_DEBUG),
    Constant("SO-REUSEADDR", SO_REUSEADDR),
    Constant("SO-DONTROUTE", SO_DONTROUTE),
    Constant("SO-BROADCAST", SO_BROADCAST),
    Constant("SO-SNDBUF", SO_SNDBUF),
    Constant("SO-RCVBUF", SO_RCVBUF),
    Constant("SO-PRIORITY", SO_PRIORITY),
    Constant("SO-TIMESTAMP", SO_TIMESTAMP),
    Constant("SO-ERROR", SO_ERROR),
    Constant("SO-ACCEPTCONN", SO_ACCEPTCONN),
    Constant("SO-TYPE", SO_TYPE),
    Constant("SO-BINDTODEVICE", SO_BINDTODEVICE),
    Constant("SO-LINGER", SO_LINGER),
    Constant("SOL-SOCKET", SOL_SOCKET),
    Constant("SHUT-RD", SHUT_RD),
    Constant("SHUT-WR", SHUT_WR),
    Constant("SHUT-RDWR", SHUT_RDWR),
    Constant("PF-UNSPEC", PF_UNSPEC),
    Constant("PF-LOCAL", PF_LOCAL),
    Constant("PF-UNIX", PF_UNIX),
    Constant("PF-FILE", PF_FILE),
    Constant("PF-INET", PF_INET),
    Constant("PF-AX25", PF_AX25),
    Constant("PF-IPX", PF_IPX),
    Constant("PF-APPLETALK", PF_APPLETALK),
    Constant("PF-NETROM", PF_NETROM),
    Constant("PF-BRIDGE", PF_BRIDGE),
    Constant("PF-ATMPVC", PF_ATMPVC),
    Constant("PF-X25", PF_X25),
    Constant("PF-INET6", PF_INET6),
    Constant("PF-ROSE", PF_ROSE),
    Constant("PF-DECNET", PF_DECnet),
    Constant("PF-NETBEUI", PF_NETBEUI),
    Constant("PF-SECURITY", PF_SECURITY),
    Constant("PF-KEY", PF_KEY),
    Constant("PF-NETLINK", PF_NETLINK),
    Constant("PF-ROUTE", PF_ROUTE),
    Constant("PF-PACKET", PF_PACKET),
    Constant("PF-ASH", PF_ASH),
    Constant("PF-ECONET", PF_ECONET),
    Constant("PF-ATMSVC", PF_ATMSVC),
    Constant("PF-SNA", PF_SNA),
    Constant("PF-IRDA", PF_IRDA),
    Constant("PF-PPPOX", PF_PPPOX),
    Constant("PF-WANPIPE", PF_WANPIPE),
    Constant("PF-BLUETOOTH", PF_BLUETOOTH),
    Constant("PF-MAX", PF_MAX),
    Constant("AF-UNSPEC", AF_UNSPEC),
    Constant("AF-LOCAL", AF_LOCAL),
    Constant("AF-UNIX", AF_UNIX),
    Constant("AF-FILE", AF_FILE),
    Constant("AF-INET", AF_INET),
    Constant("AF-AX25", AF_AX25),
    Constant("AF-IPX", AF_IPX),
    Constant("AF-APPLETALK", AF_APPLETALK),
    Constant("AF-NETROM", AF_NETROM),
    Constant("AF-BRIDGE", AF_BRIDGE),
    Constant("AF-ATMPVC", AF_ATMPVC),
    Constant("AF-X25", AF_X25),
    Constant("AF-INET6", AF_INET6),
    Constant("AF-ROSE", AF_ROSE),
    Constant("AF-DECNET", AF_DECnet),
    Constant("AF-NETBEUI", AF_NETBEUI),
    Constant("AF-SECURITY", AF_SECURITY),
    Constant("AF-KEY", AF_KEY),
    Constant("AF-NETLINK", AF_NETLINK),
    Constant("AF-ROUTE", AF_ROUTE),
    Constant("AF-PACKET", AF_PACKET),
    Constant("AF-ASH", AF_ASH),
    Constant("AF-ECONET", AF_ECONET),
    Constant("AF-ATMSVC", AF_ATMSVC),
    Constant("AF-SNA", AF_SNA),
    Constant("AF-IRDA", AF_IRDA),
    Constant("AF-PPPOX", AF_PPPOX),
    Constant("AF-WANPIPE", AF_WANPIPE),
    Constant("AF-BLUETOOTH", AF_BLUETOOTH),
    Constant("AF-MAX", AF_MAX),
    Constant("SOL-RAW", SOL_RAW),
    Constant("SOL-DECNET", SOL_DECNET),
    Constant("SOL-X25", SOL_X25),
    Constant("SOL-PACKET", SOL_PACKET),
    Constant("SOL-ATM", SOL_ATM),
    Constant("SOL-AAL", SOL_AAL),
    Constant("SOL-IRDA", SOL_IRDA),
    Constant("SOCK-STREAM", SOCK_STREAM),
    Constant("SOCK-DGRAM", SOCK_DGRAM),
    Constant("SOCK-RAW", SOCK_RAW),
    Constant("SOCK-RDM", SOCK_RDM),
    Constant("SOCK-SEQPACKET", SOCK_SEQPACKET),
    Constant("SOCK-PACKET", SOCK_PACKET),
    Constant("INADDR-ANY", INADDR_ANY),
    Constant("MSG-OOB", MSG_OOB),
    Constant("MSG-DONTROUTE", MSG_DONTROUTE),
    Constant("MSG-DONTWAIT", MSG_DONTWAIT),
    Constant("MSG-NOSIGNAL", MSG_NOSIGNAL),
    Constant("MSG-PEEK", MSG_PEEK),
    Constant("MSG-WAITALL", MSG_WAITALL),
    Constant("F-SETFL", F_SETFL),
    Constant("O-NONBLOCK", O_NONBLOCK),
    Constant("O-ASYNC", O_ASYNC),
    Constant("IPPROTO-IP", IPPROTO_IP),
    Constant("IPPROTO-HOPOPTS", IPPROTO_HOPOPTS),
    Constant("IPPROTO-ICMP", IPPROTO_ICMP),
    Constant("IPPROTO-IGMP", IPPROTO_IGMP),
    Constant("IPPROTO-IPIP", IPPROTO_IPIP),
    Constant("IPPROTO-TCP", IPPROTO_TCP),
    Constant("IPPROTO-EGP", IPPROTO_EGP),
    Constant("IPPROTO-PUP", IPPROTO_PUP),
    Constant("IPPROTO-UDP", IPPROTO_UDP),
    Constant("IPPROTO-IDP", IPPROTO_IDP),
    Constant("IPPROTO-TP", IPPROTO_TP),
    Constant("IPPROTO-IPV6", IPPROTO_IPV6),
    Constant("IPPROTO-ROUTING", IPPROTO_ROUTING),
    Constant("IPPROTO-FRAGMENT", IPPROTO_FRAGMENT),
    Constant("IPPROTO-RSVP", IPPROTO_RSVP),
    Constant("IPPROTO-GRE", IPPROTO_GRE), 
    Constant("IPPROTO-ESP", IPPROTO_ESP), 
    Constant("IPPROTO-AH", IPPROTO_AH),
    Constant("IPPROTO-ICMPV6", IPPROTO_ICMPV6),
    Constant("IPPROTO-NONE", IPPROTO_NONE),
    Constant("IPPROTO-DSTOPTS", IPPROTO_DSTOPTS),
    Constant("IPPROTO-MTP", IPPROTO_MTP),
    Constant("IPPROTO-ENCAP", IPPROTO_ENCAP),
    Constant("IPPROTO-PIM", IPPROTO_PIM), 
    Constant("IPPROTO-COMP", IPPROTO_COMP),
    Constant("IPPROTO-SCTP", IPPROTO_SCTP),
    Constant("IPPROTO-RAW", IPPROTO_RAW),
    Constant()
  };  
  return spark::add_constants(env, constants, "spark-socket");
}

// exported function signatures
namespace spark_socket
{
  static Scheme_Object* socket(int, Scheme_Object**);
  static Scheme_Object* accept(int, Scheme_Object**);
  static Scheme_Object* bind(int, Scheme_Object**);
  static Scheme_Object* connect(int, Scheme_Object**);
  static Scheme_Object* gethostname(int, Scheme_Object**);
  static Scheme_Object* gethostbyname(int, Scheme_Object**);
  static Scheme_Object* getpeername(int, Scheme_Object**);
  static Scheme_Object* set_nonblocking(int, Scheme_Object**);
  static Scheme_Object* set_async_io(int, Scheme_Object**);
  static Scheme_Object* get_nonblocking(int, Scheme_Object**);
  static Scheme_Object* get_async_io(int, Scheme_Object**);
  static Scheme_Object* spark_htonl(int, Scheme_Object**);
  static Scheme_Object* spark_htons(int, Scheme_Object**);
  static Scheme_Object* spark_ntohl(int, Scheme_Object**);
  static Scheme_Object* spark_ntohs(int, Scheme_Object**);
  static Scheme_Object* listen(int, Scheme_Object**);
  static Scheme_Object* poll(int, Scheme_Object**);
  static Scheme_Object* recv(int, Scheme_Object**);
  static Scheme_Object* recv_line(int, Scheme_Object**);
  static Scheme_Object* recvfrom(int, Scheme_Object**);
  static Scheme_Object* send(int, Scheme_Object**);
  static Scheme_Object* sendto(int, Scheme_Object**);
  static Scheme_Object* setsockopt(int, Scheme_Object**);
  static Scheme_Object* getsockopt(int, Scheme_Object**);
  static Scheme_Object* shutdown(int, Scheme_Object**);
  static Scheme_Object* close(int, Scheme_Object**);
  static Scheme_Object* equals(int, Scheme_Object**);
} // namespace spark_socket

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_socket::accept, "accept", 1),
    new Procedure(spark_socket::bind, "bind", 2),
    new Procedure(spark_socket::connect, "connect", 2),
    new Procedure(spark_socket::close, "close", 1),
    new Procedure(spark_socket::gethostname, "gethostname", 0),
    new Procedure(spark_socket::gethostbyname, "gethostbyname", 1),
    new Procedure(spark_socket::getpeername, "getpeername", 1),
    new Procedure(spark_socket::set_nonblocking, "set-non-blocking", 2),
    new Procedure(spark_socket::set_async_io, "set-async-io", 2),
    new Procedure(spark_socket::get_nonblocking, "get-non-blocking", 1),
    new Procedure(spark_socket::get_async_io, "get-async-io", 1),
    new Procedure(spark_socket::spark_htonl, "htonl", 1),
    new Procedure(spark_socket::spark_htons, "htons", 1),
    new Procedure(spark_socket::spark_ntohl, "ntohl", 1),
    new Procedure(spark_socket::spark_ntohs, "ntohs", 1),
    new Procedure(spark_socket::listen, "listen", 2),
    new Procedure(spark_socket::poll, "poll", 2),
    new Procedure(spark_socket::recv, "recv", 4),
    new Procedure(spark_socket::recv_line, "recv-line", 3),
    new Procedure(spark_socket::recvfrom, "recvfrom", 4),
    new Procedure(spark_socket::send, "send", 3),
    new Procedure(spark_socket::sendto, "sendto", 4),
    new Procedure(spark_socket::setsockopt, "setsockopt", 4),
    new Procedure(spark_socket::getsockopt, "getsockopt", 3),
    new Procedure(spark_socket::socket, "socket", 0, 3),
    new Procedure(spark_socket::shutdown, "shutdown", 2),
    new Procedure(spark_socket::equals, "equals?", 2),
    0
  };
  
  return spark::add_procedures(env, procedures, "spark-socket");
}

// Exported socket API

// Where ever you see the word address-info, take it as
// a scheme list in the format:
// (address-family port ip-address)

// Accept an incoming connection on a listening socket.
// This function takes one argument, which should be a valid
// listening socket.
// Returns a list in the following format:
// (new-socket address-info)
// address-info is the address of the site connecting to you.
// The return value will be NULL on failure.
Scheme_Object*
spark_socket::accept(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  sockaddr_in remote_addr;
  socklen_t remote_addr_len = sizeof(remote_addr);
  Socket remote_sock = ::accept(sock, 
				reinterpret_cast<sockaddr*>(&remote_addr), 
				&remote_addr_len);
  if (remote_sock == -1)
    {
      DEFAULT_RET_FINISH;
    }

  // Prepare the return value as a list
  {
    Scheme_Object* remote_sock_obj = NULL;
    Scheme_Object* remote_addr_info = NULL;

    MZ_GC_DECL_REG(2);
    MZ_GC_VAR_IN_REG(0, remote_sock);
    MZ_GC_VAR_IN_REG(1, remote_addr_info);
    MZ_GC_REG();

    remote_sock_obj = _socket_to_scheme_object(remote_sock);
    remote_addr_info = _make_address_info(remote_addr);
    Scheme_Object** list_elems = new Scheme_Object*[2];
    list_elems[0] = remote_sock_obj;
    list_elems[1] = remote_addr_info;
    _ret_ = scheme_build_list(2, list_elems);

    MZ_GC_UNREG();
    delete[] list_elems;
  }
  DEFAULT_RET_FINISH;
}

// Associate a socket with an IP address and port number
// Takes two arguemts, a valid socket and an address-info.
// The ip address field of the second argument should be NULL
// to use INADDR_ANY.
// Returns #t on success, NULL on error.
Scheme_Object* 
spark_socket::bind(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  sockaddr_in addr;
  if (_unpack_to_sockaddr_in(argv[1], "bind", addr))
    {
      if (::bind(sock, 
		 reinterpret_cast<sockaddr*>(&addr), 
		 sizeof(addr)) == 0)
	_ret_ = scheme_true;  
    }
  
  DEFAULT_RET_FINISH;
}

// Connects the socket to a server.
// Takes two arguemts, a valid socket and an address-info.
Scheme_Object* 
spark_socket::connect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  sockaddr_in addr;
  if (_unpack_to_sockaddr_in(argv[1], "connect", addr))
    {
      if (::connect(sock, 
		    reinterpret_cast<sockaddr*>(&addr), 
		    sizeof(addr)) == 0 || errno == EINPROGRESS)
	_ret_ = scheme_true;  
    }

  DEFAULT_RET_FINISH;
}

// Returns the name of the system.
Scheme_Object*
spark_socket::gethostname(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  char hostname[255];
  if (::gethostname(hostname, sizeof(hostname)) == 0)
    _ret_ = scheme_make_utf8_string(hostname);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::gethostbyname(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  hostent* addr = ::gethostbyname(s.c_str());
  if (addr)
    _ret_ = _hostent_to_scheme_object(addr);
  DEFAULT_RET_FINISH;
}

// Return address info about the remote side of the connection.
Scheme_Object* 
spark_socket::getpeername(int, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sockaddr_in addr;
  socklen_t len = sizeof(addr);
  Socket sock = scheme_object_to_socket(argv[0]);
  if (::getpeername(sock, reinterpret_cast<sockaddr*>(&addr), 
		    &len) == 0)
    _ret_ = _make_address_info(addr);
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::set_nonblocking(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  using spark::Utils;
  Socket sock = scheme_object_to_socket(argv[0]);
  Scheme_Object* cmd = argv[1];

  int cntl = ::fcntl(sock, F_GETFL, 0);
  if (cmd == scheme_true)
    cntl |= O_NONBLOCK;
  else
    cntl &= (~O_NONBLOCK); 
  if (::fcntl(sock, F_SETFL, cntl) == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::get_nonblocking(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  using spark::Utils;
  Socket sock = scheme_object_to_socket(argv[0]);

  int cntl = ::fcntl(sock, F_GETFL, 0);
  if ((cntl & O_NONBLOCK) == O_NONBLOCK)
    _ret_ = scheme_true;
  else
    _ret_ = scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::set_async_io(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  using spark::Utils;
  Socket sock = scheme_object_to_socket(argv[0]);
  Scheme_Object* cmd = argv[1];

  int cntl = ::fcntl(sock, F_GETFL, 0);
  if (cmd == scheme_true)
    cntl |= O_ASYNC;
  else
    cntl &= (~O_ASYNC); 
  if (::fcntl(sock, F_SETFL, cntl) == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::get_async_io(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  using spark::Utils;
  Socket sock = scheme_object_to_socket(argv[0]);

  int cntl = ::fcntl(sock, F_GETFL, 0);
  if ((cntl & O_ASYNC) == O_ASYNC)
    _ret_ = scheme_true;
  else
    _ret_ = scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::spark_htonl(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int v = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], v))
    scheme_wrong_type("htonl", "int", 0, argc, argv);
  
  uint32_t hostlong = static_cast<uint32_t>(v);
  _ret_ = scheme_make_integer(::htonl(hostlong));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::spark_htons(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int v = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], v))
    scheme_wrong_type("htons", "int", 0, argc, argv);
  
  uint16_t hostshort = static_cast<uint32_t>(v);
  _ret_ = scheme_make_integer(::htons(hostshort));
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::spark_ntohl(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int v = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], v))
    scheme_wrong_type("ntohl", "int", 0, argc, argv);
  
  uint32_t netlong = static_cast<uint32_t>(v);
  _ret_ = scheme_make_integer(::ntohl(netlong));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::spark_ntohs(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int v = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], v))
    scheme_wrong_type("ntohs", "int", 0, argc, argv);
  
  uint16_t netshort = static_cast<uint32_t>(v);
  _ret_ = scheme_make_integer(::ntohs(netshort));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::listen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int backlog = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], backlog))
    scheme_wrong_type("listen", "int", 1, argc, argv);

  if (::listen(sock, backlog) == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::poll(int, Scheme_Object**)
{
  DEFAULT_RET_INIT;

  // TODO: Implement

  DEFAULT_RET_FINISH;
}

// Receive data on a socket.
// Arguments:
// 1. socket descriptor
// 2. number of bytes to receive
// 3. flags. (a list of integers or null)
// 4. boolean. #t if the return value should be
// a list of bytes or #f if the return value should be
// a string. 
// Returns the received bytes as a string or
// null on error.
Scheme_Object* 
spark_socket::recv(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int size = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], size))
    scheme_wrong_type("recv", "int", 1, argc, argv);
  int flags = spark::Utils::flag_from_list(argv[2]);

  char* buffer = new char[size + 1];
  int read = 0;
  if ((read = ::recv(sock, buffer, size, flags)) >= 0)
    {
      if (argv[3] == scheme_true)
	_ret_ = spark::Utils::make_bytes_list(buffer, read);
      else
	{
	  buffer[read] = 0;
	  _ret_ = scheme_make_utf8_string(buffer);
	}
    }
  else if (errno == EWOULDBLOCK)
    {
      _ret_ = scheme_make_utf8_string("");
    }

  delete[] buffer;
      
  DEFAULT_RET_FINISH;
}

// Receive one line of data from the socket.
// Line terminator is \r\n, which is not returned.
// Arguments:
// 1. socket descriptor
// 2. maximum number of bytes to receive or -1
// 3. flags. (a list of integers or null)
// Returns the received bytes as a string or
// null on error.
Scheme_Object* 
spark_socket::recv_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int max_size = 0;
  if (argv[1] != scheme_null)
    {
      if (!spark::Utils::int_from_scheme_long(argv[1], max_size))
	scheme_wrong_type("recv-line", "int", 1, argc, argv);
    }
  int flags = spark::Utils::flag_from_list(argv[2]);

  std::string s = "";
  const int size = 1;
  char* buffer = new char[size];
  int read = 0;
  int total_read = 0;
  while ((read = ::recv(sock, reinterpret_cast<void*>(buffer),
		     size, flags)) == 1)
    {
      if (buffer[0] == 13) 
	{
	  if ((read = ::recv(sock, reinterpret_cast<void*>(buffer),
		     size, flags)) == 1)
	    {
	      if (buffer[0] == 10)
		{
		  break;
		}
	    }
	}
      ++total_read;
      if (max_size > 0)
	{
	  if (total_read > max_size)
	    {
	      break;
	    }
	}
      s += buffer[0];
    }
  if (total_read <= 0)
    {
      if (errno == EWOULDBLOCK)
	_ret_ = scheme_make_utf8_string("");
      else
	_ret_ = scheme_null;
    }
  else
    _ret_ = scheme_make_utf8_string(s.c_str());
  delete[] buffer;
      
  DEFAULT_RET_FINISH;
}

// Receive data on a UDP socket.
// Arguments:
// 1. socket descriptor
// 2. number of bytes to receive
// 3. flags. (a list of integers or null)
// 4. from address-info
// 5. #t to return a list of bytes or #f to return a string
// Returns the received bytes as a string or
// null on error.
Scheme_Object* 
spark_socket::recvfrom(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int size = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], size))
    scheme_wrong_type("recvfrom", "int", 1, argc, argv);
  int flags = spark::Utils::flag_from_list(argv[2]);
  sockaddr_in fromaddr;
  if (_unpack_to_sockaddr_in(argv[3], "recvfrom", fromaddr))
    {
      char* buffer = new char[size + 1];
      socklen_t len = static_cast<socklen_t>(sizeof(fromaddr));
      int read = 0;
      if ((read = ::recvfrom(sock, reinterpret_cast<void*>(buffer),
			     size, flags,
			     reinterpret_cast<sockaddr*>(&fromaddr),
			     &len)) >= 0)
	{
	  if (argv[4] == scheme_true)
	    _ret_ = spark::Utils::make_bytes_list(buffer, read);
	  else
	    {
	      buffer[read] = 0;
	      _ret_ = scheme_make_utf8_string(buffer);
	    }
	}
      delete[] buffer;
    }

  DEFAULT_RET_FINISH;
}

// Sends bytes over a socket
// Arguments:
// 1. socket handle
// 2. string to send (can also be a byte string)
// 3. flags
// Returns number of bytes sent, or null on error.
Scheme_Object* 
spark_socket::send(int, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  ssize_t sent = 0;
  int flags = spark::Utils::flag_from_list(argv[2]);
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      std::string buffer;
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      buffer = SCHEME_BYTE_STR_VAL(str);
      char* c_str = const_cast<char*>(buffer.c_str());
      sent = ::send(sock, reinterpret_cast<void*>(c_str),
		    buffer.length(), flags);
    }
  else if (SCHEME_BYTE_STRINGP(argv[1]))
    {
      size_t len = SCHEME_BYTE_STRLEN_VAL(argv[1]);
      char* tmp = SCHEME_BYTE_STR_VAL(argv[1]);
      if (!tmp)
	{
	  DEFAULT_RET_FINISH;
	}
      sent = ::send(sock, reinterpret_cast<void*>(tmp),
		    len, flags);
    }
    
  if (sent >= 0)
    _ret_ = scheme_make_integer(static_cast<long>(sent));
  else if (errno == EWOULDBLOCK)
    _ret_ = scheme_make_utf8_string("");

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_socket::sendto(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  std::string buffer;
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      buffer = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      if (!spark::Utils::string_from_bytes_list(argv[1], buffer))
	{
	  DEFAULT_RET_FINISH;
	}
    }

  int flags = spark::Utils::flag_from_list(argv[2]);

  sockaddr_in toaddr;
  if (_unpack_to_sockaddr_in(argv[3], "sendto", toaddr))
    {
      char* c_str = const_cast<char*>(buffer.c_str());
      ssize_t sent = ::sendto(sock, reinterpret_cast<void*>(c_str),
			      buffer.length(), flags,
			      reinterpret_cast<sockaddr*>(&toaddr),
			      sizeof(toaddr));
      if (sent >= 0)
	_ret_ = scheme_make_integer(static_cast<long>(sent));
    }

  DEFAULT_RET_FINISH;
}

// Set various options for a socket
// Arguments:
// 1. Socket descriptor
// 2. level
// 3. option name
// 4. option value. depending on the option name,
// this can be either an integer, a string or a
// list of two integers (represents a linger struct)
Scheme_Object* 
spark_socket::setsockopt(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int level = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], level))
    scheme_wrong_type("setsockopt", "int", 1, argc, argv);
  int optname = 0;
  if (!spark::Utils::int_from_scheme_long(argv[2], optname))
    scheme_wrong_type("setsockopt", "int", 2, argc, argv);
  int ret = 0;

  switch (optname)
    {
      /* options that take an integer option value. */
    case SO_KEEPALIVE:
    case SO_OOBINLINE:
    case SO_RCVLOWAT:
    case SO_SNDLOWAT:
    case SO_BSDCOMPAT:
    case SO_PASSCRED:
    case SO_DEBUG:
    case SO_REUSEADDR:
    case SO_DONTROUTE:
    case SO_BROADCAST:
    case SO_SNDBUF:
    case SO_RCVBUF:
    case SO_PRIORITY:
    case SO_TIMESTAMP:
      /* following three options are valid with getsockopt only. */
    case SO_ERROR: 
    case SO_ACCEPTCONN:
    case SO_TYPE:
      {
	int optval = 0;
	if (!spark::Utils::int_from_scheme_long(argv[3], optval))
	  scheme_wrong_type("setsockopt", "int", 3, argc, argv);
	ret = ::setsockopt(sock, level, optname, 
			   &optval, sizeof(optval));
	break;
      }
      /* options that take a string value. */
    case SO_BINDTODEVICE:
      {
	Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
	std::string optval = SCHEME_BYTE_STR_VAL(str);
	ret = ::setsockopt(sock, level, optname, 
			   (void*)optval.c_str(), optval.length());
	break;
      }
    case SO_LINGER:      
      {
	/* Value is of type struct linger which consists of two ints.
	   So arg_optval should be a list of two integers.
	*/
	struct linger optval;
	if (scheme_list_length(argv[3]) != 2)
	  scheme_wrong_type("setsockopt", "list-of-2-ints",
			    3, argc, argv);
	if (!spark::Utils::int_from_scheme_long(SCHEME_CAR(argv[3]), 
						optval.l_onoff))
	  scheme_wrong_type("setsockopt", "list-of-2-ints",
			    3, argc, argv);
	if (!spark::Utils::int_from_scheme_long(SCHEME_CAR(SCHEME_CDR(argv[3])), 
						optval.l_linger))
	  scheme_wrong_type("setsockopt", "list-of-2-ints",
			    3, argc, argv);
	ret = ::setsockopt(sock, level, optname, 
			   &optval, sizeof(optval));
	break;
      }
    default:
      ret = -1;
    }
  if (ret == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// Returns the value of a socket option.
// Arguments:
// 1. socket descriptor
// 2. level
// 3. option name
// Returns the option value on success.
// This can be an integer, a string or a list of
// two integers based on the option name.
// On error, returns NULL.
Scheme_Object* 
spark_socket::getsockopt(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int level = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], level))
    scheme_wrong_type("getsockopt", "int", 1, argc, argv);
  int optname = 0;
  if (!spark::Utils::int_from_scheme_long(argv[2], optname))
    scheme_wrong_type("getsockopt", "int", 2, argc, argv);
  int ret = 0;

  switch (optname)
    {
      /* options that return an integer option value. */
    case SO_KEEPALIVE:
    case SO_OOBINLINE:
    case SO_RCVLOWAT:
    case SO_SNDLOWAT:
    case SO_BSDCOMPAT:
    case SO_PASSCRED:
    case SO_DEBUG:
    case SO_REUSEADDR:
    case SO_DONTROUTE:
    case SO_BROADCAST:
    case SO_SNDBUF:
    case SO_RCVBUF:
    case SO_PRIORITY:
    case SO_TIMESTAMP:
    case SO_ERROR: 
    case SO_ACCEPTCONN:
    case SO_TYPE:
      {
	int optval = 0;
	socklen_t sz = sizeof(optval);
	ret = ::getsockopt(sock, level, optname, 
			   &optval, &sz);
	if (ret == 0)
	  _ret_ = scheme_make_integer(optval);
	break;
      }
      /* options that return a string value. */
    case SO_BINDTODEVICE:
      {
	char optval[IFNAMSIZ];
	socklen_t sz = IFNAMSIZ;
	ret = ::getsockopt(sock, level, optname, 
			   (void*)optval, &sz);	
	if (ret == 0)
	  _ret_ = scheme_make_utf8_string(optval);
	break;
      }
    case SO_LINGER:      
      {
	/* Returns a list of two ints */
	struct linger optval;
	optval.l_onoff = 0;
	optval.l_linger = 0;
	socklen_t sz = sizeof(optval);
	ret = ::getsockopt(sock, level, optname, 
			   (void*)&optval, &sz);
	if (ret == 0)
	  {
	    Scheme_Object** elems = new Scheme_Object*[2];
	    elems[0] = NULL; elems[1] = NULL;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, elems[0]);
	    MZ_GC_VAR_IN_REG(1, elems[1]);
	    MZ_GC_REG();
	    elems[0] = scheme_make_integer(optval.l_onoff);
	    elems[1] = scheme_make_integer(optval.l_linger);
	    _ret_ = scheme_build_list(2, elems);
	    MZ_GC_UNREG();
	    delete[] elems;
	  }
	break;
      }
    default:
      ret = -1;
    }

  DEFAULT_RET_FINISH;
}

// Stop further sends and receives on a socket.
// Arguments:
// 1. socket
// 2. describes how to shut down.
Scheme_Object* 
spark_socket::shutdown(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket sock = scheme_object_to_socket(argv[0]);
  int how = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], how))
    scheme_wrong_type("shutdown", "int", 1, argc, argv);
  if (::shutdown(sock, how) == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// Allocates a socket decsriptor.
// Returns NULL on failure.
Scheme_Object* 
spark_socket::socket(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  using spark::Utils;
  int domain = PF_INET;
  int type = SOCK_STREAM;
  int protocol = 0;

  if (argc >= 1)
    {
      if (!Utils::int_from_scheme_long(argv[0], domain))
	scheme_wrong_type("socket", "int", 0, argc, argv);
    }
  if (argc >= 2)
    {
      if (!Utils::int_from_scheme_long(argv[1], type))
	scheme_wrong_type("socket", "int", 1, argc, argv);
    }
  if (argc == 3)
    {
      if (!Utils::int_from_scheme_long(argv[2], protocol))
	scheme_wrong_type("socket", "int", 2, argc, argv);
    }

  Socket sock = ::socket(domain, type, protocol);
  if (sock == -1)
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = _socket_to_scheme_object(sock);

  DEFAULT_RET_FINISH;
}

// Closes a socket descriptor.
// Returns NULL on failure, #t on success.
Scheme_Object*
spark_socket::close(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Socket s = scheme_object_to_socket(argv[0]);
  if (s == -1)
    scheme_wrong_type("close", "int", 0, argc, argv);
  
  int v = ::close(s);
  if (v == 0)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// Compares two sockets.
// returns true if they are same, false otherwise.
Scheme_Object* 
spark_socket::equals(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  _ret_ = scheme_false;

  Socket sock01 = scheme_object_to_socket(argv[0]);
  Socket sock02 = scheme_object_to_socket(argv[1]);
  if (sock01 == sock02)
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// :~

// local utility functions

Scheme_Object*
_socket_to_scheme_object(Socket s)
{
  return scheme_make_integer_value(static_cast<long>(s));
}

Socket
spark_socket::scheme_object_to_socket(Scheme_Object* obj)
{
  Socket ret = -1;
  if (!spark::Utils::int_from_scheme_long(obj, ret))
    return -1;
  return ret;
}

Scheme_Object* 
_make_address_info(const sockaddr_in& addr)
{
  Scheme_Object* family = NULL;
  Scheme_Object* port = NULL;
  Scheme_Object* ip = NULL;

  MZ_GC_DECL_REG(3);
  MZ_GC_VAR_IN_REG(0, family);
  MZ_GC_VAR_IN_REG(1, port);
  MZ_GC_VAR_IN_REG(2, ip);
  MZ_GC_REG();

  family = scheme_make_integer_value(static_cast<long>(addr.sin_family));
  port = scheme_make_integer_value_from_unsigned(static_cast<unsigned long>
						 (addr.sin_port));
  char* str_ip = inet_ntoa(addr.sin_addr);
  ip = scheme_make_utf8_string(str_ip);
  
  Scheme_Object** list_elems = new Scheme_Object*[3];
  list_elems[0] = family;
  list_elems[1] = port;
  list_elems[2] = ip;
  MZ_GC_UNREG();

  Scheme_Object* r = scheme_build_list(3, list_elems);
  delete[] list_elems;
  return r;
}

bool
_unpack_to_sockaddr_in(Scheme_Object* addr_info, 
		       const char* calling_func,
		       sockaddr_in& ret)
{
  using spark::Utils;

  if (scheme_list_length(addr_info) != 3)
    {
      scheme_wrong_type(calling_func, 
			"list-of-3-elements", -1, 
			0, (Scheme_Object**)addr_info);
      return false;
    }
  int tmp = 0;
  Scheme_Object* list = addr_info;
  Scheme_Object* cdr01 = NULL;
  Scheme_Object* cdr02 = NULL;
  Scheme_Object* elem01 = NULL;
  Scheme_Object* elem02 = NULL;
  Scheme_Object* elem03 = NULL;
  MZ_GC_DECL_REG(5);
  MZ_GC_VAR_IN_REG(0, cdr01);
  MZ_GC_VAR_IN_REG(1, cdr02);
  MZ_GC_VAR_IN_REG(2, elem01);
  MZ_GC_VAR_IN_REG(3, elem02);
  MZ_GC_VAR_IN_REG(4, elem03);
  MZ_GC_REG();

  elem01 = SCHEME_CAR(list);
  if (!Utils::int_from_scheme_long(elem01, tmp))
    scheme_wrong_type("_unpack_to_sockaddr_in", "int", -1, 
		      0, (Scheme_Object**)elem01);
  ret.sin_family = static_cast<short>(tmp);
  cdr01 = SCHEME_CDR(list);
  elem02 = SCHEME_CAR(cdr01);
  tmp = 0;
  if (!Utils::int_from_scheme_long(elem02, tmp))
    scheme_wrong_type("_unpack_to_sockaddr_in", "int", -1, 
		      0, (Scheme_Object**)elem02);
  ret.sin_port = htons(tmp);
  cdr02 = SCHEME_CDR(cdr01);
  elem03 = SCHEME_CAR(cdr02);
  if (scheme_eqv(elem03, scheme_null))
    ret.sin_addr.s_addr = INADDR_ANY;
  else
    {
      if (SCHEME_CHAR_STRINGP(elem03))
	{
	  elem03 = scheme_char_string_to_byte_string(elem03);
	  std::string h = SCHEME_BYTE_STR_VAL(elem03);
	  // TODO: replace this with a call to gethostbyname
	  if (!inet_aton(h.c_str(), &ret.sin_addr))
	    return false;
	  
	}
      else
	scheme_wrong_type("_unpack_to_sockaddr_in", "string", -1, 
			  0, (Scheme_Object**)elem03);
    }
  MZ_GC_UNREG();
  return true;
}

Scheme_Object*
_hostent_to_scheme_object(const hostent* addr)
{
  const int NUM_FIELDS = 5;
  Scheme_Object* h_name = NULL;
  Scheme_Object* h_aliases = NULL;
  Scheme_Object* h_addrtype = NULL;
  Scheme_Object* length = NULL;
  Scheme_Object* h_addr_list = NULL;
  
  int i = 0;
  MZ_GC_DECL_REG(NUM_FIELDS);
  MZ_GC_VAR_IN_REG(i++, h_name);
  MZ_GC_VAR_IN_REG(i++, h_aliases);
  MZ_GC_VAR_IN_REG(i++, h_addrtype);
  MZ_GC_VAR_IN_REG(i++, length);
  MZ_GC_VAR_IN_REG(i, h_addr_list);
  MZ_GC_REG();

  if (addr->h_name)
    h_name = scheme_make_utf8_string(addr->h_name);
  else
    h_name = scheme_null;

  // aliases
  {  
    std::vector<Scheme_Object*> scheme_objects;
    i = 0;
    char* alias = addr->h_aliases[i++];
    while (alias)
      {
	Scheme_Object* alias_obj = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, alias_obj);
	MZ_GC_REG();
	alias_obj = scheme_make_utf8_string(alias);
	scheme_objects.push_back(alias_obj);
	alias = addr->h_aliases[i++];
	alias = 0;
	MZ_GC_UNREG();
      }
    const size_t sz = scheme_objects.size();
    if (sz)
      {
	Scheme_Object** tmp = new Scheme_Object*[sz];
	for (size_t i=0; i<sz; ++i)
	  {
	    tmp[i] = scheme_objects[i];
	  }
	h_aliases = scheme_build_list(static_cast<int>(sz),
				      (Scheme_Object**)tmp);
	delete[] tmp;
      }
    else
      h_aliases = scheme_null;
  }
  // :~
  
  h_addrtype = scheme_make_integer(static_cast<long>(addr->h_addrtype));
  length = scheme_make_integer(static_cast<long>(addr->h_length));
  
  {
    std::vector<Scheme_Object*> scheme_objects;
    i = 0;
    while (addr->h_addr_list[i])
      {
	in_addr tmp_addr;
	tmp_addr.s_addr = *(u_long *)addr->h_addr_list[i++]; 
	Scheme_Object* addr_obj = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, addr_obj);
	MZ_GC_REG();
	char* saddr = inet_ntoa(tmp_addr);
	if (saddr)	    
	  addr_obj = scheme_make_utf8_string(saddr);
	else
	  addr_obj = scheme_null;
	scheme_objects.push_back(addr_obj);
	MZ_GC_UNREG();
      }
    const size_t sz = scheme_objects.size();
    if (sz)
      {
	Scheme_Object** tmp = new Scheme_Object*[sz];
	for (size_t i=0; i<sz; ++i)
	  {
	    tmp[i] = scheme_objects[i];
	  }
	h_addr_list = scheme_build_list(static_cast<int>(sz),
					(Scheme_Object**)tmp);
	delete[] tmp;
      }
    else
      h_addr_list = scheme_null;
  }
  
  MZ_GC_UNREG();
  i = 0;
  Scheme_Object** values = new Scheme_Object*[NUM_FIELDS];
  values[i++] = h_name;
  values[i++] = h_aliases;
  values[i++] = h_addrtype;
  values[i++] = length;
  values[i] = h_addr_list;
  Scheme_Object* ret = scheme_build_list(NUM_FIELDS, values);
  delete[] values;
  return ret;
}

bool 
spark_socket::scheme_list_to_sockets(Scheme_Object* list, Sockets& out)
{
  if (list == NULL)
    return false;
  if (list == scheme_null)
    return false;
  Scheme_Object* elem = SCHEME_CAR(list);
  Scheme_Object* rest = SCHEME_CDR(list);
  while (elem)
    {
      out.push_back(scheme_object_to_socket(elem));
      if (rest == NULL)
	break;
      if (rest == scheme_null)
	break;
      elem = SCHEME_CAR(rest);
      rest = SCHEME_CDR(rest);
    }
  return true;
}

Scheme_Object* 
spark_socket::sockets_to_scheme_list(const Sockets& sockets)
{
  Scheme_Object* list = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, list);
  MZ_GC_REG();

  const size_t sz = sockets.size();
  if (sz == 0)
    list = scheme_null;
  else
    {
      Scheme_Object** elems = new Scheme_Object*[sz];
      for (size_t i=0; i<sz; ++i)
	{
	  elems[i] = NULL;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, elems);
	  MZ_GC_REG();
	  elems[i] = scheme_make_integer_value(sockets[i]);
	  MZ_GC_UNREG();
	}
      list = scheme_build_list(sz, elems);
      delete[] elems;
    }
  MZ_GC_UNREG();
  return list;
}

// :~
