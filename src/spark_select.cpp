// MzScheme inetrface to select() API.
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
#include "spark_select.h"
#include "spark_socket.h"

#include <sys/select.h>

static const char* MODULE_NAME = "#%spark-select";

static spark::Status_code _add_procedures(Scheme_Env* env);

spark::Status_code
spark_select::initialize(Scheme_Env* env)
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
namespace spark_select
{
  static Scheme_Object* select(int, Scheme_Object**);
} // namespace spark_select

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_select::select, "select", 4),
    0
  };
  return spark::add_procedures(env, procedures, "spark-sqlite");
}

// Check if sockets descriptors are ready to read/write.
// Arguments:
// 1. list of sockets to check for reads or null.
// 2. list of sockets to check for writes or null.
// 3. list of sockets to check for errors or null.
// 4. timeout. a list that represents a timeval struct.
// list of seconds and microseconds.
// Returns a list of 3 lists:
// ((sockets-read-ready) (sockets-write-ready) (sockets-error)),
// true on timeout or null on error.
Scheme_Object*
spark_select::select(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  using namespace spark;
  using namespace spark_socket;

  if (argv[0] != scheme_null)
    {
      if (!SCHEME_LISTP(argv[0]))
	scheme_wrong_type("select", "list", 0, argc, argv);
    }
  if (argv[1] != scheme_null)
    {
      if (!SCHEME_LISTP(argv[1]))
	scheme_wrong_type("select", "list", 1, argc, argv);

    }
  if (argv[2] != scheme_null)
    {
      if (!SCHEME_LISTP(argv[2]))
	scheme_wrong_type("select", "list", 2, argc, argv);
    }
  if (argv[3] != scheme_null)
    {
      if (!SCHEME_LISTP(argv[3]))
	scheme_wrong_type("select", "list", 3, argc, argv);
    }
  timeval time_out;
  time_out.tv_sec = 0;
  time_out.tv_usec = 0;
  if (argv[3] != scheme_null)
    {
      if (scheme_list_length(argv[3]) != 2)
	scheme_wrong_type("select", "list-of-2-ints", 3, argc, argv);
      Scheme_Object* obj = SCHEME_CAR(argv[3]);
      int val = 0;
      if (!Utils::int_from_scheme_long(obj, val))
	scheme_wrong_type("select", "list-of-2-ints", 3, argc, argv);
      time_out.tv_sec = val;
      obj = SCHEME_CAR(SCHEME_CDR(argv[3]));
      if (!Utils::int_from_scheme_long(obj, val))
	scheme_wrong_type("select", "list-of-2-ints", 3, argc, argv);
      time_out.tv_usec = val;
    }
  Sockets readfds_vec;
  bool has_readfds = scheme_list_to_sockets(argv[0], readfds_vec);
  Sockets writefds_vec;
  bool has_writefds = scheme_list_to_sockets(argv[1], writefds_vec);
  Sockets errorfds_vec;
  bool has_errorfds = scheme_list_to_sockets(argv[2], errorfds_vec);
  
  fd_set readfds;
  FD_ZERO(&readfds);  
  int max = 0;
  if (has_readfds)
    {
      size_t sz = readfds_vec.size();
      for (size_t i=0; i<sz; ++i)
	{
	  Socket s = readfds_vec[i];
	  FD_SET(s, &readfds);
	  int si = static_cast<int>(s);
	  if (si > max)
	    max = si;
	}
    }
  fd_set writefds;
  FD_ZERO(&writefds);
  if (has_writefds)
    {
      size_t sz = writefds_vec.size();
      for (size_t i=0; i<sz; ++i)
	{
	  Socket s = writefds_vec[i];
	  FD_SET(s, &writefds);
	  int si = static_cast<int>(s);
	  if (si > max)
	    max = si;
	}
    }
  fd_set errorfds;
  FD_ZERO(&errorfds);
  if (has_errorfds)
    {
      size_t sz = errorfds_vec.size();
      for (size_t i=0; i<sz; ++i)
	{
	  Socket s = errorfds_vec[i];
	  FD_SET(s, &errorfds);
	  int si = static_cast<int>(s);
	  if (si > max)
	    max = si;
	}
    }
  ++max;
  
  int ret = ::select(max, &readfds, &writefds, 
		     &errorfds, &time_out);
  if (ret == 0) // timeout
    _ret_ = scheme_true;
  else if (ret == -1) // error
    {
      _ret_ = scheme_null;
      DEFAULT_RET_FINISH;
    }
  // else
  {
    Scheme_Object* read_set_list = NULL;
    Scheme_Object* write_set_list = NULL;
    Scheme_Object* error_set_list = NULL;
    Sockets read_set;
    for (int i=0; i<max; ++i)
      {
	if (FD_ISSET(i, &readfds))
	  read_set.push_back(i);
      }
    Sockets write_set;
    for (int i=0; i<max; ++i)
      {
	if (FD_ISSET(i, &writefds))
	  write_set.push_back(i);
      }
    Sockets error_set;
    for (int i=0; i<max; ++i)
      {
	if (FD_ISSET(i, &errorfds))
	  error_set.push_back(i);
      }
    read_set_list = sockets_to_scheme_list(read_set);
    write_set_list = sockets_to_scheme_list(write_set);
    error_set_list = sockets_to_scheme_list(error_set);
    Scheme_Object* elems[3];
    elems[0] = read_set_list;
    elems[1] = write_set_list;
    elems[2] = error_set_list;
    if (read_set.size() > 0 || write_set.size() > 0 
	|| error_set.size() > 0)
      _ret_ = scheme_build_list(3, elems);      
    else if (ret == 0)
      _ret_ = scheme_true;
  }
  DEFAULT_RET_FINISH;
}

