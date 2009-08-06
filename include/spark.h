// General stuff used by all Spark modules.
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
// (Electronic mail: mathew.vijay@gmail.com)

#ifndef _SPARK_H_
#define _SPARK_H_

#define SPARK_MAJOR_VERSION 1
#define SPARK_MINOR_VERSION 0 
#define SPARK_MINOR_MINOR_VERSION 0

// common errors returned by spark to the system

#define SPARK_SUCCESS 0 
#define SPARK_LOAD_ERROR 1 
#define SPARK_INTERPRETER_ERROR 2 

// Uncomment these to use 3m GC

//#ifndef MZ_PRECISE_GC 

//#define MZ_PRECISE_GC 

//#endif // #ifndef MZ_PRECISE_GC 

// :~

#ifdef MZ_PRECISE_GC
#define STACK_BASE &__gc_var_stack__
#else
#define STACK_BASE NULL
#endif

#include "scheme.h"
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <cerrno>

#include <pthread.h>
#include <unistd.h>

#define SPARK_DEBUG

#define SPARK_DIR_SEP '/' 
#define SPARK_LIBPATH_ENVVAR_NAME "SPARK_LIBPATH" 

namespace spark
{
  enum Status_code
    {
      SUCCESS,
      SYSTEM_CALL_FAILED,
      MZ_CALL_FAILED,
      MZ_CALL_RETURNED_NULL,
      NULL_POINTER,
      INVALID_ARG
    }; // enum Status_code

  typedef std::map<std::string, std::string> String_map;
  typedef std::vector<std::string> String_vec;

  struct Envvar
  {
    static std::string& get(const std::string& envvar_name);
    static bool set(const std::string& var, const std::string& val);
  private:
    static void _load();
  private:
     static String_map env_vars;
  }; // struct Envvar

  struct Procedure
  {
    Scheme_Prim* prim;
    std::string name;
    int min_args;
    int max_args;
    Procedure()
    : prim(0), name(""), min_args(0), max_args(0)
    { } 
    Procedure(Scheme_Prim* p, const char* n, int mina)
      : prim(p), name(n), min_args(mina),
	max_args(mina)
    { }
    Procedure(Scheme_Prim* p, const char* n, int mina, int maxa)
      : prim(p), name(n), min_args(mina),
	max_args(maxa)
    { }
  }; // struct Procedure

  struct Constant
  {
    std::string name;
    int value;

    Constant()
      : name(""), value(0)
    { }
    Constant(const char* n, int v)
      : name(n), value(v)
    { }
  }; // struct Constant

  typedef char Char;

  struct Utils
  {
    static bool uchar_from_scheme_long(const Scheme_Object* obj, unsigned char& ret);
    static bool char_from_scheme_long(const Scheme_Object* obj, char& ret);
    static bool short_from_scheme_long(const Scheme_Object* obj, short& ret);
    static bool int_from_scheme_long(const Scheme_Object* obj, int& ret);
    static bool uint_from_scheme_long(const Scheme_Object* obj, unsigned int& ret);
    static bool ushort_from_scheme_long(const Scheme_Object* obj, unsigned short& ret);
    static bool long_from_scheme_long(const Scheme_Object* obj, long& ret);
    static bool double_from_scheme_double(const Scheme_Object* obj, double& ret);
    static bool float_from_scheme_double(const Scheme_Object* obj, float& ret);
    static bool char_from_scheme_char(Scheme_Object* obj, Char& c);
    static int flag_from_list(const Scheme_Object* list);
    static bool string_from_bytes_list(Scheme_Object* list, std::string& out);
    static Scheme_Object* make_bytes_list(const char* buffer, int len);
    static void get_file_extn(const std::string& file_name, std::string& ext);

    static void 
    system_error_str(std::string& ret_errmsg)
    {
      ret_errmsg = strerror(errno);
    }

    static void raise_system_error(const char* func);

    static void
    raise_system_error(const char* func, int ret_val, 
		       int errcode = -1)
    {
      if (ret_val == errcode)
	raise_system_error(func);
    }
    static std::string type(const Scheme_Object* obj);
  };

  Status_code add_constants(Scheme_Env* env, Constant* constants,
			    const std::string& module_name);
  Status_code add_procedures(Scheme_Env* env, Procedure** procedures,
			     const std::string& module_name);

  typedef std::map<int, Scheme_Object*> Int_so_map;
  typedef std::vector<const char*> Charp_vector;

  struct Lock
  {
  public:
    Lock()
    {
      pthread_mutex_lock(&_mutex);
    }
    ~Lock()
    {
      pthread_mutex_unlock(&_mutex);
    }
  private:
    static pthread_mutex_t _mutex;
  }; // struct Lock

  struct Global_strings
  {
  public:
    static const char* contains(const char* s)
    {
      Charp_vector::const_iterator it_curr = _strings.begin();
      Charp_vector::const_iterator it_end = _strings.end();
      while (it_curr != it_end)
	{
	  if (strcmp(*it_curr, s) == 0)
	    return *it_curr;
	  ++it_curr;
	}
      return 0;
    }
    static void add(const char* s)
    {
      Lock lock;
      _strings.push_back(s);
    }
  private:
    static Charp_vector _strings;
  }; // struct Global_strings

  const std::string get_version();

} // namespace spark

#define assert_scheme_object(p, func) {\
    if (p == 0)\
      {\
	std::clog << func << " failed.\n";\
	return spark::MZ_CALL_RETURNED_NULL;\
      }\
  }

#define DEFAULT_RET_INIT \
    Scheme_Object* _ret_ = NULL;\
    MZ_GC_DECL_REG(1);\
    MZ_GC_VAR_IN_REG(0, _ret_);\
    MZ_GC_REG();\
    _ret_ = scheme_null;\

#define DEFAULT_RET_FINISH \
    MZ_GC_UNREG();\
    return _ret_;\

#endif // #ifndef _SPARK_H_
