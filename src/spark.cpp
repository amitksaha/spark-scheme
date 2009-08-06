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

#include <unistd.h>
#include <iostream>
#include <fstream>
#include "spark.h"
#include "spark_modules.h"

struct Commandline_options
  {
    std::string file_to_load;
    std::string s_expr;
    bool print_help;
    bool load_uncompiled;
    bool print_version;
    bool print_license;
    bool print_credits;
    bool print_copyright;

    Commandline_options()
      : file_to_load(""),
	s_expr(""),
	print_help(false),
	load_uncompiled(false),
	print_version(false),
	print_license(false),
	print_credits(false),
	print_copyright(false)
    {
    }      
  };

static Commandline_options  _commandline_options;
static int _script_file_index = 0;
static bool _load_spark(Scheme_Env* scheme_env);
static bool _parse_commandline(int argc, char** argv);
static void _print_usage();
static void _print_version();
static void _print_welcome();
static void _print_credits();
static void _print_copyright();
static void _print_license();
static void _initialize_commandline_arguments(int argc, char** argv);

static spark::String_map _init_options;
static void _init_spark();

extern std::string vm_executable_name;
extern spark::String_vec command_line_arguments;

int 
main(int argc, char** argv)
{
  // _init_spark();
  
  // std::string spark_home = _init_options[SPARK_HOME_ENVVAR_NAME];
  std::string spark_home = spark::Envvar::get(SPARK_LIBPATH_ENVVAR_NAME);
  if (spark_home.length() == 0)
    {
      spark_home = "/usr/lib/spark";
      if (!spark::Envvar::set(SPARK_LIBPATH_ENVVAR_NAME, spark_home)) 
	{
	  std::cout << "Failed to set environment variable.\n";
	  return SPARK_LOAD_ERROR;
	}
    }
  /*
    if (spark_home.length() == 0)
    {
    std::cout << "Failed to load Scheme extentions. "
    << SPARK_HOME_ENVVAR_NAME 
    << " environment variable not set"
    << std::endl;
      return SPARK_LOAD_ERROR;
      }
  */
  
  if (!_parse_commandline(argc, argv))
    {
      std::cout << "Try the option -h to get usage instructions.\n";
      return SPARK_SUCCESS;
    }
  if (_commandline_options.print_help)
    {
      _print_usage();
      return SPARK_SUCCESS;
    }
  if (_commandline_options.print_version)
    {
      _print_version();
      return SPARK_SUCCESS;
    }
  if (_commandline_options.print_credits)
    {
      _print_credits();
      return SPARK_SUCCESS;
    }
  if (_commandline_options.print_copyright)
    {
      _print_copyright();
      return SPARK_SUCCESS;
    }
  if (_commandline_options.print_license)
    {
      _print_license();
      return SPARK_SUCCESS;
    }

  std::string spark_libs = "/scheme/spark-init-c.ss";
  if (_commandline_options.load_uncompiled)
    spark_libs = "/scheme/spark-init.ss";

  vm_executable_name = argv[0];
  _initialize_commandline_arguments(argc, argv);

  Scheme_Env* scheme_env = NULL;
  Scheme_Object* curout  = NULL;
  Scheme_Object* v = NULL;
  Scheme_Config* config  = NULL;
  mz_jmp_buf * volatile save = NULL, fresh;
  MZ_GC_DECL_REG(5);
  MZ_GC_VAR_IN_REG(0, scheme_env);
  MZ_GC_VAR_IN_REG(1, curout);
  MZ_GC_VAR_IN_REG(2, save);
  MZ_GC_VAR_IN_REG(3, config);
  MZ_GC_VAR_IN_REG(4, v);
  scheme_set_stack_base(STACK_BASE, 1);
  MZ_GC_REG();

  scheme_env = scheme_basic_env();
  config = scheme_current_config();
  curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

  // load and register spark C++ libraries
  if (!_load_spark(scheme_env))
    return SPARK_LOAD_ERROR;
  // :~

  // load spark scheme extentions.
  {
    spark_home += spark_libs;
    v = scheme_load(spark_home.c_str());
    if (v == NULL)
      {
	std::cout << "Failed to load Scheme extentions from " 
		  << spark_home 
		  << std::endl;
	return SPARK_LOAD_ERROR;
      }
  }
  // :~

  bool has_file = _commandline_options.file_to_load.length() > 0 ? true : false;
  bool has_expr = _commandline_options.s_expr.length() > 0 ? true : false;

  if (!has_file && !has_expr)
    {
      save = scheme_current_thread->error_buf;
      scheme_current_thread->error_buf = &fresh;
      if (scheme_setjmp(scheme_error_buf)) 
	{
	  scheme_current_thread->error_buf = save;
	  return SPARK_INTERPRETER_ERROR;
	} 
      _print_welcome();
      v = scheme_make_character('\n');
      scheme_display(v, curout);
      // read-eval-print loop, implicitly uses the initial Scheme_Env:
      v = scheme_builtin_value("read-eval-print-loop");
      scheme_apply(v, 0, NULL);
      scheme_current_thread->error_buf = save;
    }
  else
    {
      save = scheme_current_thread->error_buf;
      scheme_current_thread->error_buf = &fresh;
      if (scheme_setjmp(scheme_error_buf)) 
	{
	  scheme_current_thread->error_buf = save;
	  std::cout << "Error: scheme_setjmp failed.\n";
	  return SPARK_INTERPRETER_ERROR;
	} 
      if (has_file)
	{
	  v = scheme_load(_commandline_options.file_to_load.c_str());
	  if (v == NULL)
	    {
	      std::cout << "Error: while loading " 
			<< _commandline_options.file_to_load << '\n';
	      return SPARK_INTERPRETER_ERROR;
	    }
	  scheme_display(v, curout);
	  v = scheme_make_character('\n');
	  scheme_display(v, curout);
	  MZ_GC_UNREG();
	  return SPARK_SUCCESS;
	}	   
      if (has_expr)
	{
	  v = scheme_eval_string(_commandline_options.s_expr.c_str(), scheme_env);
	  scheme_display(v, curout);
	  v = scheme_make_character('\n');
	  scheme_display(v, curout);
	  MZ_GC_UNREG();
	  return SPARK_SUCCESS;
	}
    }
  MZ_GC_UNREG();
  return SPARK_SUCCESS;
}

bool
_load_spark(Scheme_Env* scheme_env)
{
  spark::Status_code sc;
  if ((sc = spark_socket::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_socket::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_sysinfo::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_sysinfo::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_select::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_select::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_sqlite::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_sqlite::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_expat::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_expat::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_fltk::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_fltk::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_curl::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_curl::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_opengl::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_opengl::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_fcgi::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_fcgi::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  if ((sc = spark_odbc::initialize(scheme_env)) != spark::SUCCESS)
    {
      std::cout << "spark_odbc::initialize failed with error: "
		<< sc << '\n';
      return false;
    }
  return true;
}

void 
_initialize_commandline_arguments(int argc, char** argv)
{
  if (argc > 1)
    {
      int start_i = _script_file_index;
      ++start_i;
      const int sz = argc - start_i;
      if (sz <= 0)
	return;
      for (int i=start_i; i<argc; ++i)
	command_line_arguments.push_back(argv[i]);
    }
}

void
_print_welcome()
{
  std::cout << "spark v" 
	    << spark::get_version() << "\n\n"
	    << "This is free software; see the source for copying conditions.\n"
	    << "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"
	    << "To quit this REPL, use the (exit) procedure.\n"
	    << std::endl;
}

void 
_print_usage()
{
  std::cout << "Spark - A way to scheme.\n"
	    << "Usage: spark [OPTIONS] scheme-file [OPTIONS-FOR-THE-SCHEME-SCRIPT]\n"
	    << '\n'
	    << "    -s                 load Spark extentions as source (not the compiled form)\n"
	    << "    -e <expression>    evaluate the expression and print the result\n"
	    << "    --version          display version\n"
	    << "    --copyright        show copyright information\n"
	    << "    --credits          show credits\n"
	    << "    -h, --help         print this help\n"
	    << "\n"
	    << "If a Scheme file is specified, Spark will load and execute it.\n"
	    << "If no options are given, Spark will start the read-eval-print loop.\n"
	    << std::endl;
}

void
_print_version()
{
  std::cout << "Spark " << spark::get_version() << '\n'
	    << "Copyright (C) 2007, 2008, 2009 Vijay Mathew Pandyalakal\n"
	    << "This is free software.  You may redistribute copies of it under the terms of\n"
	    << "the GNU General Public License <http://www.gnu.org/licenses/gpl.html>.\n"
	    << "There is NO WARRANTY, to the extent permitted by law."
	    << std::endl;
}

void
_print_credits()
{
  std::cout << "We wish to thank the following people for their \nexcellent code libraries: \n\n"
    
	    << "Matthew Flatt and others for PLT MzScheme. \n\n"
	    << "James Clark for the Expat XML Library. \n\n"
	    << "Daniel Stenberg for Curl, a library for transferring \ndata specified with URL syntax. \n\n"
	    << "Bill Spitzak and others for the Fltk GUI toolkit. \n\n"
	    << "D. Richard Hipp for Sqlite, an Embeddable SQL Database Engine. \n"
	    << std::endl;
}

void
_print_copyright()
{
  std::cout << "Spark, Copyright (c) 2007, 2008, 2009 Vijay Mathew Pandyalakal. \n\n"
	    << "MzScheme v371 [3m], Copyright (c) 2004-2007 PLT Scheme Inc. \n\n"
	    << "Expat, Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd and Clark Cooper. \n"
	    << "Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers. \n\n"
	    << "Curl, Copyright (c) 1996 - 2008, Daniel Stenberg. \n\n"
	    << "FLTK is copyright 1998-2006 by Bill Spitzak and others. \n"
	    << std::endl;
}

void
_print_license()
{

  std::cout << "Spark is an advanced Scheme programming system. \n"
	    << "Copyright (C) 2007, 2008, 2009 Vijay Mathew Pandyalakal. \n\n"
	    << "This program is free software: you can redistribute it and/or modify \n"
	    << "it under the terms of the GNU General Public License as published by \n"
	    << "the Free Software Foundation, either version 3 of the License, or \n"
	    << "(at your option) any later version.\n\n"
	    << "This program is distributed in the hope that it will be useful, \n"
	    << "but WITHOUT ANY WARRANTY; without even the implied warranty of \n"
	    << "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \n"
	    << "GNU General Public License for more details. \n\n"
	    << "You should have received a copy of the GNU General Public License \n"
	    << "along with this program.  If not, see <http://www.gnu.org/licenses/>. \n\n"
	    << "Send mail to vijay.the.schemer@gmail.com for more information.\n"
	    << std::endl;
}

static void
_assert_less_than(int a, int b, const char* msg)
{
  if (b >= a)
    {
      if (msg)
	std::cout << msg << std::endl;
      exit(SPARK_LOAD_ERROR);
    }
}

bool
_parse_commandline(int argc, char** argv) 
{
  if (argc > 1)
    {
      for (int i=1; i<argc; ++i)
	{
	  std::string opt = argv[i];
	  if (opt[0] == '-')
	    {
	      if (opt == "-s")
		_commandline_options.load_uncompiled = true;
	      else if (opt == "-e") 
		{
		  ++i;
		  _assert_less_than(argc, i, "Expected value for option not found.");
		  _commandline_options.s_expr = argv[i];
		}
	      else if (opt == "-h" || opt == "--help")
		_commandline_options.print_help = true;
	      else if (opt == "--credits")
		_commandline_options.print_credits = true;
	      else if (opt == "--copyright")
		_commandline_options.print_copyright = true;
	      else if (opt == "--license")
		_commandline_options.print_license = true;
	      else if (opt == "--version")
		_commandline_options.print_version = true;
	      else 
		{
		  std::cout << "Invalid option.\n";
		  exit(1);
		}
	    }
	  else
	    {
	      _commandline_options.file_to_load = argv[i];
	      _script_file_index = i;
	      // leave the rest for the scheme script
	      return true;
	    }
	}
    }
  return true;
}

static bool
_get_kv(const std::string& s, std::string& k, 
	std::string& v)
{
  size_t index = s.find('=');
  if (index != std::string::npos)
    {
      k = s.substr(0, index);
      v = s.substr((index + 1), (s.length() - index));
      return true;
    }
  return false;
}

void
_init_spark()
{
  std::string home = spark::Envvar::get("HOME");
  if (home.length() == 0)
    {
      std::cout << "Cannot get HOME environment variable\n";
      exit(1);
    }
  std::ostringstream init_file;
  init_file << home << "/.spark-init";
  std::ifstream in(init_file.str().c_str());
  if (!in)
    {
      std::cout << "Failed to open " << init_file.str() << '\n';
      return;
    }
  const int MAXLEN = 1024;
  char s[MAXLEN + 1];
  in.getline(s, MAXLEN);
  while (!in.eof())
    {
      std::string k;
      std::string v;
      if (_get_kv(s, k, v))
	_init_options[k] = v;
      in.getline(s, MAXLEN);
    }
  in.close();
}

