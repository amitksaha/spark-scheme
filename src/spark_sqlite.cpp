// MzScheme inetrface to the SQLite API.
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
#include "spark_sqlite.h"
using namespace spark_sqlite;

static const char* MODULE_NAME = "#%spark-sqlite";

static std::string _global_error_buff;

enum Sqlite_tag
  {
    SQLITE_TAG,
    SQLITE_STMT_TAG
  }; // enum Sqlite_tag

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);
static sqlite3* _scheme_object_to_sqlite(int argc, 
					 Scheme_Object** argv,
					 int idx);				 
static sqlite3_stmt* _scheme_object_to_sqlite_stmt(int argc, 
					      Scheme_Object** argv,
					      int idx);

spark::Status_code
spark_sqlite::initialize(Scheme_Env* env)
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
namespace spark_sqlite
{
  static Scheme_Object* open(int, Scheme_Object**);
  static Scheme_Object* close(int, Scheme_Object**);
  static Scheme_Object* prepare(int, Scheme_Object**);
  static Scheme_Object* bind_double(int, Scheme_Object**);
  static Scheme_Object* bind_int(int, Scheme_Object**);
  static Scheme_Object* bind_int64(int, Scheme_Object**);
  static Scheme_Object* bind_double(int, Scheme_Object**);
  static Scheme_Object* bind_text(int, Scheme_Object**);
  static Scheme_Object* bind_null(int, Scheme_Object**);
  static Scheme_Object* reset(int, Scheme_Object**);
  static Scheme_Object* clear_bindings(int, Scheme_Object**);
  static Scheme_Object* finalize(int, Scheme_Object**);
  static Scheme_Object* step(int, Scheme_Object**);
  static Scheme_Object* column_blob(int, Scheme_Object**);
  static Scheme_Object* column_bytes(int, Scheme_Object**);
  static Scheme_Object* column_double(int, Scheme_Object**);
  static Scheme_Object* column_int(int, Scheme_Object**);
  static Scheme_Object* column_int64(int, Scheme_Object**);
  static Scheme_Object* column_text(int, Scheme_Object**);
  static Scheme_Object* column_name(int, Scheme_Object**);
  static Scheme_Object* column_database_name(int, Scheme_Object**);
  static Scheme_Object* column_table_name(int, Scheme_Object**);
  static Scheme_Object* column_origin_name(int, Scheme_Object**);
  static Scheme_Object* column_type(int, Scheme_Object**);
  static Scheme_Object* column_decl_type(int, Scheme_Object**);
  static Scheme_Object* column_count(int, Scheme_Object**);
  static Scheme_Object* execute(int, Scheme_Object**);
  static Scheme_Object* get_changes(int, Scheme_Object**);
  static Scheme_Object* get_total_changes(int, Scheme_Object**);
  static Scheme_Object* get_autocommit(int, Scheme_Object**);
  static Scheme_Object* strerror(int, Scheme_Object**);
  static Scheme_Object* get_global_error(int, Scheme_Object**);
} // namespace spark_sqlite

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("OPEN-READONLY", SQLITE_OPEN_READONLY),
    Constant("OPEN-READWRITE", SQLITE_OPEN_READWRITE),
    Constant("OPEN-READWRITE-CREATE", 
	     SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE),
    Constant("SQLITE-INTEGER", SQLITE_INTEGER),
    Constant("SQLITE-FLOAT", SQLITE_FLOAT),
    Constant("SQLITE-TEXT", SQLITE_TEXT),
    Constant("SQLITE-BLOB", SQLITE_BLOB),
    Constant("SQLITE-NULL", SQLITE_NULL),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-sqlite");
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_sqlite::open, "open", 5),
    new Procedure(spark_sqlite::close, "close", 1),
    new Procedure(spark_sqlite::prepare, "prepare", 2),
    new Procedure(spark_sqlite::bind_double, "bind-double", 3),
    new Procedure(spark_sqlite::bind_int, "bind-int", 3),
    new Procedure(spark_sqlite::bind_int64, "bind-int64", 3),
    new Procedure(spark_sqlite::bind_text, "bind-text", 3),
    new Procedure(spark_sqlite::bind_null, "bind-null", 2),
    new Procedure(spark_sqlite::reset, "reset", 1),
    new Procedure(spark_sqlite::clear_bindings, "clear-bindings", 1),
    new Procedure(spark_sqlite::finalize, "finalize", 1),
    new Procedure(spark_sqlite::step, "step", 1),
    new Procedure(spark_sqlite::column_blob, "column-blob", 2),
    new Procedure(spark_sqlite::column_bytes, "column-bytes", 2),
    new Procedure(spark_sqlite::column_double, "column-double", 2),
    new Procedure(spark_sqlite::column_int, "column-int", 2),
    new Procedure(spark_sqlite::column_int64, "column-int64", 2),
    new Procedure(spark_sqlite::column_text, "column-text", 2),
    new Procedure(spark_sqlite::column_name, "column-name", 2),
    new Procedure(spark_sqlite::column_database_name, "column-database-name", 2),
    new Procedure(spark_sqlite::column_table_name, "column-table-name", 2),
    new Procedure(spark_sqlite::column_origin_name, "column-origin-name", 2),
    new Procedure(spark_sqlite::column_type, "column-type", 2),
    new Procedure(spark_sqlite::column_decl_type, "column-decl-type", 2),
    new Procedure(spark_sqlite::column_count, "column-count", 1),
    new Procedure(spark_sqlite::execute, "execute", 2),
    new Procedure(spark_sqlite::get_changes, "get-changes", 1),    
    new Procedure(spark_sqlite::get_total_changes, "get-total-changes", 1),  
    new Procedure(spark_sqlite::get_autocommit, "get-autocommit", 1),      
    new Procedure(spark_sqlite::get_global_error, "get-global-error", 0),    
    new Procedure(spark_sqlite::strerror, "strerror", 1),    
    0
  };
  return spark::add_procedures(env, procedures, "spark-sqlite");
}

// Exported sqlite API

// Opens a sqlite3 database.
// Takes 6 arguments:
// 1. Database name.
// 2. User-name. (ignored).
// 3. Password. (ignored).
// 4. Flags. Any valid value accepted by sqlite3_open_v2().
// 5. Name of VFS module to use.
// Returns the sqlite handle on success.
// If the function fails, the returns value will be either the error message
// or null if no error message can be retrieved from the system.
Scheme_Object* 
spark_sqlite::open(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  // database name
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("open", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string db_name = SCHEME_BYTE_STR_VAL(str);

  // username and password are ignored.

  // flags
  int flags = 0;
  if (spark::Utils::int_from_scheme_long(argv[3], flags))
    {
      // VFS name
      std::string vfs_name;
      if (argv[4] != scheme_null)
	{
	  if (!SCHEME_CHAR_STRINGP(argv[0]))
	    scheme_wrong_type("open", "string", 4, argc, argv);
	  str = scheme_char_string_to_byte_string(argv[0]);
	  vfs_name = SCHEME_BYTE_STR_VAL(str);
	}
      sqlite3* db;
      /*
	int rc = sqlite3_open_v2(db_name.c_str(),
	&db,
	flags,
	vfs_name);
      */
      int rc = sqlite3_open(db_name.c_str(),
			    &db);
      if (rc == SQLITE_OK)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(SQLITE_TAG);
	  _ret_ = scheme_make_cptr(db, tag);
	  MZ_GC_UNREG();
	}
      else
	{
	  _ret_ = scheme_make_utf8_string(sqlite3_errmsg(db));
	  sqlite3_close(db);
	}
    }      
  DEFAULT_RET_FINISH;
}

// Takes one argument: the sqlite object to close.
// Returns true on success, null on error.
Scheme_Object* 
spark_sqlite::close(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  if (db)
    {
      if (sqlite3_close(db) == SQLITE_OK)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// Compiles an SQL statement into byte codes.
// Takes two arguments:
// 1. sqlite3 handle
// 2. SQL statement to compile.
// Returns a sqlite3_stmt handle on success, null on error.
Scheme_Object* 
spark_sqlite::prepare(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  if (!db)
    {
      DEFAULT_RET_FINISH;
    }
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
  std::string sql = SCHEME_BYTE_STR_VAL(str);
  sqlite3_stmt* stmt;
  // const char* tail;
  if (sqlite3_prepare(db, sql.c_str(),
		      sql.length(),
		      &stmt,
		      0) != SQLITE_OK)
    {
      DEFAULT_RET_FINISH;
    }

  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(SQLITE_STMT_TAG);
    _ret_ = scheme_make_cptr(stmt, tag);
    MZ_GC_UNREG();
  }
  DEFAULT_RET_FINISH;
}

// The following functions replaces literals in the forms of
// ?, ?NNN, :AAA, @AAA, $VVV with parameters. To be compliant
// with other database drivers stick to the form '?'.
// They take three arguments:
// 1. sqlite_stmt handle
// 2. index of the parameter to bind. index starts at 1.
// 3. value to bind.
// They return #t on success, null on error.

enum Bind_type
  {
    BIND_DOUBLE,
    BIND_INT,
    BIND_INT64,
    BIND_TEXT,
    BIND_NULL
  }; // enum Bind_type

static Scheme_Object* _bind(int argc, Scheme_Object** argv, 
			    Bind_type type);

Scheme_Object* 
spark_sqlite::bind_double(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _bind(argc, argv, BIND_DOUBLE);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::bind_int(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _bind(argc, argv, BIND_INT);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::bind_int64(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _bind(argc, argv, BIND_INT64);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::bind_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _bind(argc, argv, BIND_TEXT);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::bind_null(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _bind(argc, argv, BIND_NULL);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
_bind(int argc, Scheme_Object** argv, Bind_type type)
{
  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int index = 0;
      if (!spark::Utils::int_from_scheme_long(argv[1], index))
	return scheme_null;
      switch (type)
	{
	case BIND_DOUBLE:
	  {
	    double val = 0.0f;
	    if (!spark::Utils::double_from_scheme_double(argv[2], val))
	      return scheme_null;
	    if (sqlite3_bind_double(stmt, index, val) == SQLITE_OK)
	      return scheme_true;
	  }
	case BIND_INT:
	  {
	    int val = 0;
	    if (!spark::Utils::int_from_scheme_long(argv[2], val))
	      return scheme_null;
	    if (sqlite3_bind_int(stmt, index, val) == SQLITE_OK)
	      return scheme_true;
	  }
	case BIND_INT64:
	  {
	    long val = 0;
	    if (!spark::Utils::long_from_scheme_long(argv[2], val))
	      return scheme_null;
	    if (sqlite3_bind_int(stmt, index, val) == SQLITE_OK)
	      return scheme_true;
	  }
	case BIND_TEXT:
	  {
	    Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	    std::string val = SCHEME_BYTE_STR_VAL(str);
	    if (sqlite3_bind_text(stmt, index, 
				  val.c_str(), val.length(),
				  SQLITE_TRANSIENT) == SQLITE_OK)
	      return scheme_true;
	  }
	case BIND_NULL:
	  {
	    if (sqlite3_bind_null(stmt, index) == SQLITE_OK)
	      return scheme_true;
	  }
	}
    }
  return scheme_null;
}

// The (reset) procedure is called to reset a compiled SQL statement object 
// back to it's initial state, ready to be re-executed. 
// Any SQL statement variables that had values bound to them using the 
// (bind-*) procedure retain their values. To remove bindings 
// call (clear-bindings).
Scheme_Object* 
spark_sqlite::reset(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      if (sqlite3_reset(stmt) == SQLITE_OK)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::clear_bindings(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      if (sqlite3_clear_bindings(stmt) == SQLITE_OK)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// The (finalize) procedure is called to delete a 
// compiled SQL statement.
Scheme_Object* 
spark_sqlite::finalize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      if (sqlite3_finalize(stmt) == SQLITE_OK)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// Call this method one or more times to evaluate a
// compiled SQL statement.
// Returns #t if a row is fetched successfully, #f if
// no more rows are left and null on error.
Scheme_Object* 
spark_sqlite::step(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  int rc = sqlite3_step(stmt);
  if (rc == SQLITE_ROW)
    _ret_ = scheme_true;
  else if (rc == SQLITE_DONE)
    _ret_ = scheme_false;

  DEFAULT_RET_FINISH;
}

// These routines return information about a single column of 
// the current result row of a query. 
// The left-most column of the result set has an index of 0.

// Returns the current BLOB value of the given column.
// The value is returned as a list of bytes.
// The return value can also be scheme_null if an error occured,
// the field itself is NULL or type conversion is not possible.
// Use (column_type) to determine if the column is actually NULL or not.
Scheme_Object* 
spark_sqlite::column_blob(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  void* data = const_cast<void*>(sqlite3_column_blob(stmt, col_index));
	  if (data)
	    {
	      int len = sqlite3_column_bytes(stmt, col_index);	      
	      _ret_ = spark::Utils::make_bytes_list(static_cast<char*>(data), 
						    len);
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

// If the value of the given column is a BLOB or string, this
// function will return the number of bytes in that field.
Scheme_Object* 
spark_sqlite::column_bytes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	_ret_ = scheme_make_integer(sqlite3_column_bytes(stmt, col_index));
    }

  DEFAULT_RET_FINISH;
}

// Returns the value of the given column as a double.
Scheme_Object* 
spark_sqlite::column_double(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  double val = sqlite3_column_double(stmt, col_index);
	  _ret_ = scheme_make_double(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the value of the given column as a 32 bit integer.
Scheme_Object* 
spark_sqlite::column_int(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  int val = sqlite3_column_int(stmt, col_index);
	  _ret_ = scheme_make_integer(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the value of the given column as a 64 bit integer,
// if the platform supports it.
Scheme_Object* 
spark_sqlite::column_int64(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  mzlonglong val = sqlite3_column_int64(stmt, col_index);
	  _ret_ = scheme_make_integer_value_from_long_long(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the value of the given column as a Unicode string.
Scheme_Object* 
spark_sqlite::column_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  const char* val = 
	    reinterpret_cast<const char*>(sqlite3_column_text(stmt, col_index));
	  if (val)
	    _ret_ = scheme_make_utf8_string(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the name assigned to the given column in the resultset.
Scheme_Object* 
spark_sqlite::column_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  const char* val = sqlite3_column_name(stmt, col_index);
	  if (val)
	    _ret_ = scheme_make_utf8_string(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the name of the database from which this column is selected.
Scheme_Object* 
spark_sqlite::column_database_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  const char* val = sqlite3_column_database_name(stmt, col_index);
	  if (val)
	    _ret_ = scheme_make_utf8_string(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the name of the table from which this column is selected.
Scheme_Object* 
spark_sqlite::column_table_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  const char* val = sqlite3_column_table_name(stmt, col_index);
	  if (val)
	    _ret_ = scheme_make_utf8_string(val);
	}
    }

  DEFAULT_RET_FINISH;
}

// Returns the original name of the column.
Scheme_Object* 
spark_sqlite::column_origin_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{
	  
	  const char* val = sqlite3_column_origin_name(stmt, col_index);
	  if (val)
	    _ret_ = scheme_make_utf8_string(val);
	}
    }

  DEFAULT_RET_FINISH;
}

static Scheme_Object* _column_type(int argc, Scheme_Object** argv,
				   bool decl_type);

// Returns the type of the column as an integer constant, ie either,
// SQLITE-INTEGER, SQLITE-FLOAT, SQLITE-BLOB, SQLITE-TEXT or SQLITE-NULL.
Scheme_Object* 
spark_sqlite::column_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _column_type(argc, argv, false);

  DEFAULT_RET_FINISH;
}

// Returns a string that denotes the type specified at the time
// the  column was defined.
Scheme_Object* 
spark_sqlite::column_decl_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = _column_type(argc, argv, true);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
_column_type(int argc, Scheme_Object** argv,
	     bool decl_type)
{
  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int col_index = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], col_index))
	{	  
	  if (decl_type)
	    {
	      const char* type = sqlite3_column_decltype(stmt, col_index);
	      return scheme_make_utf8_string(type);
	    }
	  else
	    {
	      int type = sqlite3_column_type(stmt, col_index);
	      switch (type)
		{
		case SQLITE_INTEGER:
		case SQLITE_FLOAT:
		case SQLITE_TEXT:
		case SQLITE_BLOB:
		case SQLITE_NULL:
		  return scheme_make_integer(type);
		}
	    }
	}
    }
  return scheme_null;
}

// Returns the number of columns in the resultset.
Scheme_Object* 
spark_sqlite::column_count(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3_stmt* stmt = _scheme_object_to_sqlite_stmt(argc, argv, 0);
  if (stmt)
    {
      int val = sqlite3_column_count(stmt);
      _ret_ = scheme_make_integer(val);
    }
	  
  DEFAULT_RET_FINISH;
}

// Executes a SQL statement. Returns #t in success, null otherwise.
// If an error occurs, the error message is set to _global_error_buff.
// Use this procedure to execute all commands except SELECT.
Scheme_Object* 
spark_sqlite::execute(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
  std::string sql = SCHEME_BYTE_STR_VAL(str);
  char* errmsg;
  int rc = sqlite3_exec(db, sql.c_str(),
			0, 0,
			&errmsg);
  if (rc == SQLITE_OK)
    _ret_ = scheme_true;
  else
    {
      _global_error_buff = errmsg;
      sqlite3_free(errmsg);
    }

  DEFAULT_RET_FINISH;
}

// This function returns the number of database rows that were 
// changed (or inserted or deleted) by the most recent SQL statement.
Scheme_Object* 
spark_sqlite::get_changes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  _ret_ = scheme_make_integer(sqlite3_changes(db));

  DEFAULT_RET_FINISH;
}

// This function returns the number of database rows that have been 
// modified by INSERT, UPDATE or DELETE statements since the database 
// handle was opened.
Scheme_Object* 
spark_sqlite::get_total_changes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  _ret_ = scheme_make_integer(sqlite3_total_changes(db));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::get_global_error(int, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_utf8_string(_global_error_buff.c_str());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_sqlite::get_autocommit(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  _ret_ = (sqlite3_get_autocommit(db) ? scheme_true : scheme_false);

  DEFAULT_RET_FINISH;
}

// Return English-language text that describes the last sqlite error.
Scheme_Object* 
spark_sqlite::strerror(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  sqlite3* db = _scheme_object_to_sqlite(argc, argv, 0);
  if (db)
    _ret_ = scheme_make_utf8_string(sqlite3_errmsg(db));

  DEFAULT_RET_FINISH;
}

sqlite3*
_scheme_object_to_sqlite(int argc, Scheme_Object** argv, int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_sqlite", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_sqlite", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Sqlite_tag tag = static_cast<Sqlite_tag>(i);
  if (tag != SQLITE_TAG)
    {
      scheme_wrong_type("_scheme_object_to_sqlite", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[0]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_sqlite", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<sqlite3*>(p);
}

sqlite3_stmt*
_scheme_object_to_sqlite_stmt(int argc, Scheme_Object** argv, int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_sqlite_stmt", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_sqlite_stmt", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Sqlite_tag tag = static_cast<Sqlite_tag>(i);
  if (tag != SQLITE_STMT_TAG)
    {
      scheme_wrong_type("_scheme_object_to_sqlite_stmt", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[0]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_sqlite_stmt", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<sqlite3_stmt*>(p);
}
