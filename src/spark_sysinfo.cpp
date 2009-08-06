// MzScheme inetrface to some common system facilities.
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
#include "spark_sysinfo.h"

static const char* MODULE_NAME = "#%spark-sysinfo";

std::string vm_executable_name = "";
spark::String_vec command_line_arguments;

static spark::Status_code _add_procedures(Scheme_Env* env);

// exported function signatures
namespace spark_sysinfo
{
  static Scheme_Object* last_strerror(int, Scheme_Object**);
  static Scheme_Object* vm_executable(int, Scheme_Object**);
  static Scheme_Object* args(int, Scheme_Object**);
  static Scheme_Object* version(int, Scheme_Object**);
} // namespace spark_socket

spark::Status_code
spark_sysinfo::initialize(Scheme_Env* env)
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

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_sysinfo::last_strerror, "last-strerror", 0),
    new Procedure(spark_sysinfo::vm_executable, "vm-executable", 0),
    new Procedure(spark_sysinfo::args, "argv", 0),
    new Procedure(spark_sysinfo::version, "spark-version", 0),
    0
  };
  return spark::add_procedures(env, procedures, "spark-sysinfo");
}

// Exported system API

Scheme_Object* 
spark_sysinfo::last_strerror(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  char* err = strerror(errno);
  if (err)
    _ret_ = scheme_make_utf8_string(err);
  else
    _ret_ = scheme_null;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sysinfo::vm_executable(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  _ret_ = scheme_make_utf8_string(vm_executable_name.c_str());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sysinfo::args(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  size_t sz = command_line_arguments.size();
  Scheme_Object** elems = new Scheme_Object*[sz];
  for (int i=0; i<sz; ++i)
    {
      Scheme_Object* obj = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, obj);
      MZ_GC_REG();
      obj = scheme_make_utf8_string(command_line_arguments[i].c_str());
      elems[i] = obj;
      MZ_GC_UNREG();
    }
  _ret_ = scheme_build_list(sz, elems);      
  delete[] elems;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sysinfo::version(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  _ret_ = scheme_make_utf8_string(spark::get_version().c_str());

  DEFAULT_RET_FINISH;
}
