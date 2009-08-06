// Spark - A programming environment built on top of MzScheme.
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

#include <sstream>
#include <cstdlib>
#include <climits>
#include "spark.h"

using spark::Utils;

pthread_mutex_t spark::Lock::_mutex = PTHREAD_MUTEX_INITIALIZER;
spark::Charp_vector spark::Global_strings::_strings;

bool 
Utils::uchar_from_scheme_long(const Scheme_Object* obj, unsigned char& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<unsigned char>(val);
  return b;
}

bool 
Utils::char_from_scheme_long(const Scheme_Object* obj, char& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<char>(val);
  return b;
}

bool 
Utils::short_from_scheme_long(const Scheme_Object* obj, short& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<short>(val);
  return b;
}

bool 
Utils::int_from_scheme_long(const Scheme_Object* obj, int& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<int>(val);
  return b;
}

bool 
Utils::uint_from_scheme_long(const Scheme_Object* obj, unsigned int& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<unsigned int>(val);
  return b;
}

bool 
Utils::ushort_from_scheme_long(const Scheme_Object* obj, unsigned short& ret)
{
  long val = -1;
  bool b = long_from_scheme_long(obj, val);
  ret = static_cast<unsigned short>(val);
  return b;
}

bool 
Utils::long_from_scheme_long(const Scheme_Object* obj, long& ret)
{
  long val = -1;
  if (!SCHEME_INTP(obj))
    return false;
  if (scheme_get_int_val(const_cast<Scheme_Object*>(obj), &val))
    {
      if (val > INT_MAX)
	val = -1;
    }
  if (val == -1)
    return false;
  else
    ret = val;
  return true;
}

bool
Utils::double_from_scheme_double(const Scheme_Object* obj, double& ret)
{
  if (!SCHEME_REALP(obj))
    return false;
  ret = scheme_real_to_double(const_cast<Scheme_Object*>(obj));
  return true;
}

bool
Utils::float_from_scheme_double(const Scheme_Object* obj, float& ret)
{
  double tmp = 0.0f;
  if (double_from_scheme_double(obj, tmp))
    {
      ret = static_cast<float>(tmp);
      return true;
    }
  return false;
}

bool 
Utils::char_from_scheme_char(Scheme_Object* obj, Char& c)
{
  if (SCHEME_CHARP(obj))
    {
      c = static_cast<Char>(SCHEME_CHAR_VAL(obj));
      return true;
    }
  return false;
}

void
Utils::raise_system_error(const char* func)
{
  std::string err;
  Utils::system_error_str(err);
  std::ostringstream out;
  out << func << " failed. (" << errno << ") " << err;
  scheme_raise_exn(MZEXN_FAIL, out.str().c_str());
}

int 
Utils::flag_from_list(const Scheme_Object* list)
{
  if (!list || list == scheme_null)
    return 0;
  if (!SCHEME_LISTP(list))
    return 0;
  Scheme_Object* elem = SCHEME_CAR(list);
  Scheme_Object* rest = SCHEME_CDR(list);
  int flag = 0;
  while (elem)
    {
      int tmp = 0;
      if (elem != scheme_null)
	{
	  if (int_from_scheme_long(elem, tmp))
	    flag |= tmp;
	}
      if (rest)
	{
	  if (rest == scheme_null)
	    break;
	  elem = SCHEME_CAR(rest);
	  rest = SCHEME_CDR(rest);
	}
      else
	break;
    }
  return flag;      
}

bool
Utils::string_from_bytes_list(Scheme_Object* list, std::string& out)
{
  if (!list || list == scheme_null)
    return false;
  if (!SCHEME_LISTP(list))
    return false;
  Scheme_Object* elem = SCHEME_CAR(list);
  Scheme_Object* rest = SCHEME_CDR(list);
  while (elem)
    {
      int tmp = 0;
      if (elem != scheme_null)
	{
	  if (int_from_scheme_long(elem, tmp))
	    out += static_cast<char>(tmp);
	}
      if (rest)
	{
	  if (rest == scheme_null)
	    break;
	  elem = SCHEME_CAR(rest);
	  rest = SCHEME_CDR(rest);
	}
      else
	break;
    }
  return true;
}

Scheme_Object* 
Utils::make_bytes_list(const char* buffer, int len)
{
  if (!buffer || len <= 0)
    return scheme_null;
  Scheme_Object** list_elems = new Scheme_Object*[len];
  for (int i=0; i<len; ++i)
    {
      Scheme_Object* elem = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, elem);
      MZ_GC_REG();
      elem = scheme_make_integer(static_cast<long>(buffer[i]));
      list_elems[i] = elem;
      MZ_GC_UNREG();
    }
  Scheme_Object* r = scheme_build_list(len, list_elems);
  delete[] list_elems;
  return r;
}

void 
Utils::get_file_extn(const std::string& file_name, 
		     std::string& extn)
{
  size_t len = file_name.length();
  size_t index = file_name.rfind('.', (len - 1));
  if (index == std::string::npos)
    return;
  std::string tmp = file_name.substr((index + 1), len);
  // convert to lower-case
  len = tmp.length();
  for (size_t i=0; i<len; ++i)
    extn += tolower(tmp[i]);
}

spark::Status_code 
spark::add_constants(Scheme_Env* env, spark::Constant* constants,
		     const std::string& module_name)
{
  if (!constants)
    return spark::NULL_POINTER;
  int i = 0;
  std::string msg = module_name;
  msg += "::";
  msg += "_add_constants->scheme_make_integer_value";
  const char* msg_str = msg.c_str();
  while (constants[i].name.length())
    {
      Scheme_Object* constant = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, constant);
      MZ_GC_REG();

      constant = scheme_make_integer_value(constants[i].value);      
      assert_scheme_object(constant, msg_str);
      scheme_add_global(constants[i].name.c_str(), constant, env);      
      ++i;
      
      MZ_GC_UNREG();
    }
  return spark::SUCCESS;
}

spark::Status_code 
spark::add_procedures(Scheme_Env* env, spark::Procedure** procedures,
		      const std::string& module_name)
{
  if (!procedures)
    return spark::NULL_POINTER;
  int i = 0;
  std::string msg = module_name;
  msg += "::";
  msg += "_add_procedures->scheme_make_prim_w_arity";
  const char* msg_str = msg.c_str();
  while (procedures[i])
    {
      Scheme_Object* procedure = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, procedure);
      MZ_GC_REG();
      
      procedure = scheme_make_prim_w_arity(
					   procedures[i]->prim,
					   procedures[i]->name.c_str(),
					   procedures[i]->min_args,
					   procedures[i]->max_args
					   );
      assert_scheme_object(procedure, msg_str);
      scheme_add_global(procedures[i]->name.c_str(), procedure, env);
      ++i;
      
      MZ_GC_UNREG();
    }
  return spark::SUCCESS;
}

std::string 
Utils::type(const Scheme_Object* obj)
{
  Scheme_Type t = SCHEME_TYPE(obj);
  switch(t)
    {
      //case scheme_bool_type: return "scheme_bool_type";
    case scheme_char_type: return "scheme_char_type";
    case scheme_integer_type: return "scheme_integer_type";
    case scheme_double_type: return "scheme_double_type";
    case scheme_float_type: return "scheme_float_type";
    case scheme_bignum_type: return "scheme_bignum_type";
    case scheme_rational_type: return "scheme_rational_type";
    case scheme_complex_type: return "scheme_complex_type";
    case scheme_complex_izi_type: return "scheme_complex_izi_type";
    case scheme_char_string_type: return "scheme_char_string_type";
    case scheme_byte_string_type: return "scheme_byte_string_type";
      //case scheme_path_type: return "scheme_path_type";
    case scheme_symbol_type: return "scheme_symbol_type";
    case scheme_keyword_type: return "scheme_keyword_type";
    case scheme_box_type: return "scheme_box_type";
    case scheme_pair_type: return "scheme_pair_type";
    case scheme_vector_type: return "scheme_vector_type";
    case scheme_structure_type: return "scheme_structure_type";
    case scheme_struct_type_type: return "scheme_struct_type_type";
    case scheme_struct_property_type: return "scheme_struct_property_type";
    default: return "unknown_type";
    }
}

const std::string
spark::get_version()
{
  std::ostringstream out;
  out << SPARK_MAJOR_VERSION << '.'
      << SPARK_MINOR_VERSION << '.'
      << SPARK_MINOR_MINOR_VERSION;
  return out.str();
}

extern char** environ;

static void _get_n_v(const std::string& nv, spark::String_vec& out);

// struct Envvar

spark::String_map spark::Envvar::env_vars;

std::string& 
spark::Envvar::get(const std::string& envvar_name)
{
  std::string& s = env_vars[envvar_name];
  if (s.length() == 0)
    {
      _load();
      s = env_vars[envvar_name];
    }
  return s;
}

bool 
spark::Envvar::set(const std::string& var, const std::string& val)
{
  if (setenv(var.c_str(), val.c_str(), 1) == 0)
    return true;
  return false;
}

void 
spark::Envvar::_load()
{
  int i = 0;
  char* e = environ[i];
  while (e)
    {
      String_vec n_v;
      _get_n_v(e, n_v);
      env_vars[n_v[0]] = n_v[1];
      e = environ[++i];
    }
}

void 
_get_n_v(const std::string& nv, spark::String_vec& out)
{
  size_t idx = nv.find('=');
  if (idx == std::string::npos)
    {
      out.push_back("");
      out.push_back("");
    }
  else
    {
      out.push_back(nv.substr(0, idx));
      out.push_back(nv.substr((idx+1), nv.length()));
    }
}

