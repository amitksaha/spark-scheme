// MzScheme inetrface to ODBC.
// Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

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

#include <sql.h>
#include <sqltypes.h>
#include <sqlext.h>
#include "spark_odbc.h"
using namespace spark_odbc;

static const char* MODULE_NAME = "#%spark-odbc";

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);

static SQLHANDLE _scheme_object_to_sql_handle(Scheme_Object* obj);
static SQLHANDLE _scheme_object_to_sql_hstmt(Scheme_Object* obj);
static Scheme_Object* _sql_handle_to_scheme_object(SQLHANDLE handle);
static Scheme_Object* _sql_return_to_symbol(SQLRETURN r, bool s = true);
static void _extract_error(SQLHANDLE handle, SQLSMALLINT type, 
			   std::string& out_text);

spark::Status_code
spark_odbc::initialize(Scheme_Env* env)
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
namespace spark_odbc
{
  static Scheme_Object* sql_alloc_handle(int, Scheme_Object**);  
  static Scheme_Object* sql_set_env_attr(int, Scheme_Object**);  
  static Scheme_Object* sql_set_connect_attr(int, Scheme_Object**);  
  static Scheme_Object* sql_connect(int, Scheme_Object**);  
  static Scheme_Object* sql_get_diag_rec(int, Scheme_Object**);  
  static Scheme_Object* sql_exec_direct(int, Scheme_Object**);  
  static Scheme_Object* sql_num_result_cols(int, Scheme_Object**);  
  static Scheme_Object* sql_row_count(int, Scheme_Object**);  
  static Scheme_Object* sql_fetch(int, Scheme_Object**);  
  static Scheme_Object* sql_get_data(int, Scheme_Object**);  
  static Scheme_Object* sql_col_attribute(int, Scheme_Object**);  
  static Scheme_Object* sql_prepare(int, Scheme_Object**);  
  static Scheme_Object* sql_bind_param(int, Scheme_Object**);  
  static Scheme_Object* sql_execute_with_params(int, Scheme_Object**);  
  static Scheme_Object* sql_free_handle(int, Scheme_Object**);  
  static Scheme_Object* sql_disconnect(int, Scheme_Object**);  
  static Scheme_Object* sql_end_tran(int, Scheme_Object**);  
} // namespace spark_odbc

spark::Status_code
_add_constants(Scheme_Env* env)
{
 using spark::Constant;
 Constant constants[] = {
   // handle types
   Constant("SQL-HANDLE-ENV", SQL_HANDLE_ENV),
   Constant("SQL-HANDLE-DBC", SQL_HANDLE_DBC),
   Constant("SQL-HANDLE-STMT", SQL_HANDLE_STMT),
   Constant("SQL-HANDLE-DESC", SQL_HANDLE_DESC),
   // environment attributes
   Constant("SQL-ATTR-CONNECTION-POOLING", SQL_ATTR_CONNECTION_POOLING),
   Constant("SQL-ATTR-CP-MATCH", SQL_ATTR_CP_MATCH),
   Constant("SQL-ATTR-ODBC-VERSION", SQL_ATTR_ODBC_VERSION),
   Constant("SQL-ATTR-OUTPUT-NTS", SQL_ATTR_OUTPUT_NTS),
   // attribute values
   Constant("SQL-CP-OFF", SQL_CP_OFF),
   Constant("SQL-CP-ONE-PER-DRIVER", SQL_CP_ONE_PER_DRIVER),
   Constant("SQL-CP-ONE-PER-HENV", SQL_CP_ONE_PER_HENV),
   Constant("SQL-CP-ONE-STRICT-MATCH", SQL_CP_STRICT_MATCH),
   Constant("SQL-CP-ONE-RELAXED-MATCH", SQL_CP_RELAXED_MATCH),
   Constant("SQL-OV-ODBC3", SQL_OV_ODBC3),
   Constant("SQL-OV-ODBC2", SQL_OV_ODBC2),
   Constant("SQL-TRUE", SQL_TRUE),
   Constant("SQL-FALSE", SQL_FALSE),
   // connection atributes and values
   Constant("SQL-ATTR-ACCESS-MODE", SQL_ATTR_ACCESS_MODE),
   Constant("SQL-MODE-READ-ONLY", SQL_MODE_READ_ONLY),
   Constant("SQL-MODE-READ-WRITE", SQL_MODE_READ_WRITE),
   Constant("SQL-ATTR-ASYNC-ENABLE", SQL_ATTR_ASYNC_ENABLE),
   Constant("SQL-ASYNC-ENABLE-OFF", SQL_ASYNC_ENABLE_OFF),
   Constant("SQL-ASYNC-ENABLE-ON", SQL_ASYNC_ENABLE_ON),
   Constant("SQL-ATTR-AUTO-IPD", SQL_ATTR_AUTO_IPD),
   Constant("SQL-ATTR-AUTOCOMMIT", SQL_ATTR_AUTOCOMMIT),
   Constant("SQL-AUTOCOMMIT-ON", SQL_AUTOCOMMIT_ON),
   Constant("SQL-AUTOCOMMIT-OFF", SQL_AUTOCOMMIT_OFF),
   Constant("SQL-ATTR-CONNECTION-DEAD", SQL_ATTR_CONNECTION_DEAD),
   Constant("SQL-CD-TRUE", SQL_CD_TRUE),
   Constant("SQL-CD-FALSE", SQL_CD_FALSE),
   Constant("SQL-ATTR-CONNECTION-TIMEOUT", SQL_ATTR_CONNECTION_TIMEOUT),
   Constant("SQL-ATTR-LOGIN-TIMEOUT", SQL_ATTR_LOGIN_TIMEOUT),
   Constant("SQL-ATTR-CURRENT-CATALOG", SQL_ATTR_CURRENT_CATALOG),
   Constant("SQL-ATTR-METADATA-ID", SQL_ATTR_METADATA_ID),
   Constant("SQL-ATTR-ODBC-CURSORS", SQL_ATTR_ODBC_CURSORS),
   Constant("SQL-CUR-USE-IF-NEEDED", SQL_CUR_USE_IF_NEEDED),
   Constant("SQL-CUR-USE-ODBC", SQL_CUR_USE_ODBC),
   Constant("SQL-CUR-USE-DRIVER", SQL_CUR_USE_DRIVER),
   Constant("SQL-ATTR-PACKET-SIZE", SQL_ATTR_PACKET_SIZE),
   Constant("SQL-ATTR-QUIET-MODE", SQL_ATTR_QUIET_MODE),
   Constant("SQL-ATTR-TRACE", SQL_ATTR_TRACE),
   Constant("SQL-OPT-TRACE-OFF", SQL_OPT_TRACE_OFF),
   Constant("SQL-OPT-TRACE-ON", SQL_OPT_TRACE_ON),
   Constant("SQL-ATTR-TRACEFILE", SQL_ATTR_TRACEFILE),
   Constant("SQL-ATTR-TRANSLATE-LIB", SQL_ATTR_TRANSLATE_LIB),
   Constant("SQL-ATTR-TRANSLATE-OPTION", SQL_ATTR_TRANSLATE_OPTION),
   Constant("SQL-ATTR-TXN-ISOLATION", SQL_ATTR_TXN_ISOLATION),
   // data types
   Constant("SQL-C-CHAR", SQL_C_CHAR),
   Constant("SQL-C-WCHAR", SQL_C_WCHAR),
   Constant("SQL-C-SHORT", SQL_C_SHORT),
   Constant("SQL-C-SSHORT", SQL_C_SSHORT),
   Constant("SQL-C-USHORT", SQL_C_USHORT),
   Constant("SQL-C-LONG", SQL_C_LONG),
   Constant("SQL-C-SLONG", SQL_C_SLONG),
   Constant("SQL-C-ULONG", SQL_C_ULONG),
   Constant("SQL-C-FLOAT", SQL_C_FLOAT),
   Constant("SQL-C-DOUBLE", SQL_C_DOUBLE),
   Constant("SQL-C-BIT", SQL_C_BIT),
   Constant("SQL-C-TINYINT", SQL_C_TINYINT),
   Constant("SQL-C-STINYINT", SQL_C_STINYINT),
   Constant("SQL-C-UTINYINT", SQL_C_UTINYINT),
   Constant("SQL-C-SBIGINT", SQL_C_SBIGINT),
   Constant("SQL-C-UBIGINT", SQL_C_UBIGINT),
   Constant("SQL-C-BINARY", SQL_C_BINARY),
   Constant("SQL-C-BOOKMARK", SQL_C_BOOKMARK),
   Constant("SQL-C-VARBOOKMARK", SQL_C_VARBOOKMARK),
   Constant("SQL-C-TYPE-DATE", SQL_C_TYPE_DATE),
   Constant("SQL-C-TYPE-TIME", SQL_C_TYPE_TIME),
   Constant("SQL-C-TYPE-TIMESTAMP", SQL_C_TYPE_TIMESTAMP),
   Constant("SQL-C-NUMERIC", SQL_C_NUMERIC),
   Constant("SQL-C-GUID", SQL_C_GUID),
   // column attributes
   Constant("SQL-DESC-BASE-COLUMN-NAME", SQL_DESC_BASE_COLUMN_NAME),
   Constant("SQL-DESC-BASE-TABLE-NAME", SQL_DESC_BASE_TABLE_NAME),
   Constant("SQL-DESC-CATALOG-NAME", SQL_DESC_CATALOG_NAME),
   Constant("SQL-DESC-LABEL", SQL_DESC_LABEL),
   Constant("SQL-DESC-LITERAL-PREFIX", SQL_DESC_LITERAL_PREFIX),
   Constant("SQL-DESC-LITERAL-SUFFIX", SQL_DESC_LITERAL_SUFFIX),
   Constant("SQL-DESC-LOCAL-TYPE-NAME", SQL_DESC_LOCAL_TYPE_NAME),
   Constant("SQL-DESC-NAME", SQL_DESC_NAME),
   Constant("SQL-DESC-SCHEMA-NAME", SQL_DESC_SCHEMA_NAME),
   Constant("SQL-DESC-TABLE-NAME", SQL_DESC_TABLE_NAME),
   Constant("SQL-DESC-TYPE-NAME", SQL_DESC_TYPE_NAME),
   Constant("SQL-DESC-AUTO-UNIQUE-VALUE", SQL_DESC_AUTO_UNIQUE_VALUE),
   Constant("SQL-DESC-CASE-SENSITIVE", SQL_DESC_CASE_SENSITIVE),
   Constant("SQL-DESC-CONCISE-TYPE", SQL_DESC_CONCISE_TYPE),
   Constant("SQL-DESC-COUNT", SQL_DESC_COUNT),
   Constant("SQL-DESC-DISPLAY-SIZE", SQL_DESC_DISPLAY_SIZE),
   Constant("SQL-DESC-FIXED-PREC-SCALE", SQL_DESC_FIXED_PREC_SCALE),
   Constant("SQL-DESC-LENGTH", SQL_DESC_LENGTH),
   Constant("SQL-DESC-NULLABLE", SQL_DESC_NULLABLE),
   Constant("SQL-DESC-NUM-PREC-RADIX", SQL_DESC_NUM_PREC_RADIX),
   Constant("SQL-DESC-OCTET-LENGTH", SQL_DESC_OCTET_LENGTH),
   Constant("SQL-DESC-PRECISION", SQL_DESC_PRECISION),
   Constant("SQL-DESC-SCALE", SQL_DESC_SCALE),
   Constant("SQL-DESC-SEARCHABLE", SQL_DESC_SEARCHABLE),
   Constant("SQL-DESC-TYPE", SQL_DESC_TYPE),
   Constant("SQL-DESC-UNNAMED", SQL_DESC_UNNAMED),
   Constant("SQL-DESC-UNSIGNED", SQL_DESC_UNSIGNED),
   Constant("SQL-DESC-UPDATABLE", SQL_DESC_UPDATABLE),
   // Transaction control
   Constant("SQL-COMMIT", SQL_COMMIT),
   Constant("SQL-ROLLBACK", SQL_ROLLBACK),
   Constant("", 0)
 };
 return add_constants(env, constants, "spark-odbc");
}


spark::Status_code
_add_procedures(Scheme_Env* env)
{
 using spark::Procedure;
 Procedure* procedures[] = {
   new Procedure(spark_odbc::sql_alloc_handle, "sql-alloc-handle", 2),
   new Procedure(spark_odbc::sql_free_handle, "sql-free-handle", 2),
   new Procedure(spark_odbc::sql_set_env_attr, "sql-set-env-attr", 3),
   new Procedure(spark_odbc::sql_set_connect_attr, "sql-set-connect-attr", 3),
   new Procedure(spark_odbc::sql_connect, "sql-connect", 4),
   new Procedure(spark_odbc::sql_get_diag_rec, "sql-get-diag-rec", 2),
   new Procedure(spark_odbc::sql_exec_direct, "sql-exec-direct", 2),
   new Procedure(spark_odbc::sql_num_result_cols, "sql-num-result-cols", 1),
   new Procedure(spark_odbc::sql_row_count, "sql-row-count", 1),
   new Procedure(spark_odbc::sql_col_attribute, "sql-col-attribute", 3),
   new Procedure(spark_odbc::sql_prepare, "sql-prepare", 2),
   new Procedure(spark_odbc::sql_bind_param, "sql-bind-param", 4),
   new Procedure(spark_odbc::sql_execute_with_params, 
		 "sql-execute-with-params", 2),
   new Procedure(spark_odbc::sql_fetch, "sql-fetch", 1),
   new Procedure(spark_odbc::sql_get_data, "sql-get-data", 4),
   new Procedure(spark_odbc::sql_disconnect, "sql-disconnect", 1),
   new Procedure(spark_odbc::sql_end_tran, "sql-end-tran", 3),
   0
 };
 return spark::add_procedures(env, procedures, "spark-odbc");
}

struct Bind_param
{
  SQLSMALLINT index;
  SQLSMALLINT param_type;
  SQLSMALLINT value_type;
  SQLULEN col_size;
  SQLPOINTER value;

  Bind_param() : index(1),
		 param_type(SQL_CHAR),
		 value_type(SQL_C_DEFAULT),
		 col_size(0),
		 value(0)
  { }

  Bind_param(SQLSMALLINT idx,
	     SQLSMALLINT pt,
	     Scheme_Object* v)
    : index(idx), 
      param_type(SQL_CHAR),
      value_type(SQL_C_DEFAULT),
      col_size(0),
      value(0)
  {
    long i = 0;
    float d = 0;
    if (v != 0)
      {
	if (SCHEME_CHAR_STRINGP(v))
	  {  
	    param_type = SQL_VARCHAR;
	    Scheme_Object* bstr = scheme_char_string_to_byte_string(v);
	    std::string s = SCHEME_BYTE_STR_VAL(bstr);
	    char* buff = reinterpret_cast<char*>(malloc(s.length() + 1));
	    strcpy(buff, s.c_str());
	    SQLCHAR* c_buff = reinterpret_cast<SQLCHAR*>(buff);
	    value = reinterpret_cast<SQLPOINTER>(c_buff);
	    col_size = static_cast<SQLULEN>(s.length());
	  }
	else if (spark::Utils::long_from_scheme_long(v, i))
	  {
	    param_type = SQL_INTEGER;
	    SQLINTEGER* buff =
	      reinterpret_cast<SQLINTEGER*>(malloc(sizeof(SQLINTEGER)));
	    *buff = static_cast<SQLINTEGER>(i);
	    value = reinterpret_cast<SQLPOINTER>(buff);
	    col_size = static_cast<SQLULEN>(sizeof(SQLINTEGER));
	  }
	else if (spark::Utils::float_from_scheme_double(v, d))
	  {
	    param_type = SQL_FLOAT;
	    SQLREAL* buff = 
	      reinterpret_cast<SQLREAL*>(malloc(sizeof(SQLREAL)));
	    *buff = static_cast<SQLREAL>(d);
	    value = reinterpret_cast<SQLPOINTER>(buff);
	    col_size = static_cast<SQLULEN>(sizeof(SQLREAL));
	  }
      }	
  }
  ~Bind_param()
  {
    if (value)
      free(value);
  }
}; // struct Bind_param

typedef std::vector<Bind_param*> Bind_params;

// Exported OpenGL functions

Scheme_Object*
spark_odbc::sql_alloc_handle(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int t = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], t))
   {
     scheme_wrong_type("sql-alloc-handle", "int",
                       0, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }
 SQLSMALLINT handle_type = static_cast<SQLSMALLINT>(t);
 SQLHANDLE input_handle = _scheme_object_to_sql_handle(argv[1]);
 SQLHANDLE output_handle = 0;
 SQLRETURN r = SQLAllocHandle(handle_type, input_handle, &output_handle);
 if (r == SQL_SUCCESS)
   _ret_ = _sql_handle_to_scheme_object(output_handle);
 else 
   _ret_ = _sql_return_to_symbol(r);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_get_diag_rec(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int t = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], t))
   {
     scheme_wrong_type("sql-get-diag-rec", "int",
                       0, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }
 SQLSMALLINT handle_type = static_cast<SQLSMALLINT>(t);
 SQLHANDLE input_handle = _scheme_object_to_sql_handle(argv[1]);
 std::string text = "";
 _extract_error(input_handle, handle_type, text);
 _ret_ = scheme_make_utf8_string(text.c_str());

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_exec_direct(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
 if (!SCHEME_CHAR_STRINGP(argv[1]))
   {
     scheme_signal_error("Invalid type for query.");
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
 std::string sql = SCHEME_BYTE_STR_VAL(str);
 
 _ret_ = _sql_return_to_symbol(SQLExecDirect(h_stmt,
					     reinterpret_cast<SQLCHAR*>(const_cast<char*>(sql.c_str())),
					     SQL_NTS));

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_num_result_cols(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
 SQLSMALLINT num_cols = 0;
 SQLRETURN r = SQLNumResultCols(h_stmt, &num_cols);
 if (r == SQL_SUCCESS)
   _ret_ = scheme_make_integer(static_cast<int>(num_cols));
 else
   _ret_ = _sql_return_to_symbol(r);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_row_count(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
 SQLINTEGER num_rows = 0;
 SQLRETURN r = SQLRowCount(h_stmt, &num_rows);
 if (r == SQL_SUCCESS)
   _ret_ = scheme_make_integer(static_cast<int>(num_rows));
 else
   _ret_ = _sql_return_to_symbol(r);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_fetch(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
 SQLRETURN r = SQLFetch(h_stmt);
 if (r == SQL_SUCCESS)
   _ret_ = scheme_true;
 else if (r == SQL_NO_DATA)
   _ret_ = scheme_false;
 else
   _ret_ = _sql_return_to_symbol(r);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_get_data(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }

 int i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], i))
   {
     
     scheme_wrong_type("sql-get-data", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
    }
 SQLUSMALLINT col_num = static_cast<SQLUSMALLINT>(i);

 i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], i))
   {
     
     scheme_wrong_type("sql-get-data", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
    }
 SQLSMALLINT type = static_cast<SQLSMALLINT>(i);

 i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[3], i))
   {
     
     scheme_wrong_type("sql-get-data", "int",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
    }
 SQLLEN buff_len = static_cast<SQLLEN>(i);

 SQLPOINTER target_value = malloc(buff_len);
 SQLLEN out_len = 0;
 SQLRETURN r = SQLGetData(h_stmt, col_num,
			  type, target_value,
			  buff_len, &out_len);
 if (r == SQL_SUCCESS)
   {
     Scheme_Object* val = NULL;
     Scheme_Object* val_size = NULL;

     MZ_GC_DECL_REG(2);
     MZ_GC_VAR_IN_REG(0, val);
     MZ_GC_VAR_IN_REG(1, val_size);
     MZ_GC_REG();

     val = scheme_make_utf8_string(reinterpret_cast<char*>(target_value));
     if (out_len == SQL_NO_TOTAL)
       val_size = scheme_make_symbol("not-total");
     else if (out_len == SQL_NULL_DATA)
       val_size = scheme_make_symbol("null-data");
     else
       val_size = scheme_make_integer(static_cast<int>(out_len));
     Scheme_Object** list_elems = new Scheme_Object*[2];
     list_elems[0] = val;
     list_elems[1] = val_size;
     _ret_ = scheme_build_list(2, list_elems);

     MZ_GC_UNREG();
     delete[] list_elems;
   }
 else
   _ret_ = _sql_return_to_symbol(r);
 
 free(target_value);

 DEFAULT_RET_FINISH;
}

static bool 
_is_char_attribute(SQLUSMALLINT field_id)
{
  switch (field_id)
    {
    case SQL_DESC_BASE_COLUMN_NAME:
    case SQL_DESC_BASE_TABLE_NAME:
    case SQL_DESC_CATALOG_NAME:
    case SQL_DESC_LABEL:
    case SQL_DESC_LITERAL_PREFIX:
    case SQL_DESC_LITERAL_SUFFIX:
    case SQL_DESC_LOCAL_TYPE_NAME:
    case SQL_DESC_NAME:
    case SQL_DESC_SCHEMA_NAME:
    case SQL_DESC_TABLE_NAME:
    case SQL_DESC_TYPE_NAME:
      return true;
    }
  return false;
}

Scheme_Object*
spark_odbc::sql_col_attribute(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
 int i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], i))
   {
     
     scheme_wrong_type("sql-col-attribute", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
    }
 SQLUSMALLINT col_num = static_cast<SQLUSMALLINT>(i);

 i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], i))
   {
     
     scheme_wrong_type("sql-col-attribute", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 SQLUSMALLINT field_id = static_cast<SQLUSMALLINT>(i);

 SQLPOINTER char_attr = 0;
 SQLSMALLINT buff_len = 0;
 SQLSMALLINT str_len = 0;
 SQLLEN num_attr = 0;

 if (_is_char_attribute(field_id))
   {
     buff_len = 129;
     char_attr = reinterpret_cast<SQLPOINTER>(malloc(buff_len));
   }
     
 SQLRETURN r = SQLColAttribute(h_stmt,
			       col_num,
			       field_id,
			       char_attr,
			       buff_len,
			       &str_len,
			       &num_attr);
 if (r == SQL_SUCCESS)
   {
     if (_is_char_attribute(field_id))
       _ret_ = scheme_make_utf8_string(reinterpret_cast<char*>(char_attr));
     else
       _ret_ = scheme_make_integer(static_cast<int>(num_attr));
   }
 else
   _ret_ = _sql_return_to_symbol(r);

 if (char_attr)
   free(char_attr);

 DEFAULT_RET_FINISH;
}

// helpers for sql_set_env_attr
Scheme_Object* _set_uint_env_attr(SQLHENV env, 
				  SQLINTEGER attr,
				  Scheme_Object* attr_value,
				  const char* attr_msg);
Scheme_Object* _set_int_env_attr(SQLHENV env, 
				 SQLINTEGER attr,
				 Scheme_Object* attr_value,
				 const char* attr_msg);
// :~

Scheme_Object*
spark_odbc::sql_bind_param(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], i))
    {
      scheme_signal_error("Expected index.");
      return 0;
    }
  int t = 0;
  if (!spark::Utils::int_from_scheme_long(argv[2], t))
    {
      scheme_signal_error("Expected type.");
      return 0;
    }
  Bind_param* bp = new Bind_param(i, t, argv[3]);
  SQLLEN strlen = 0;
  if (bp->param_type == SQL_VARCHAR)
    strlen = SQL_NTS;
  SQLRETURN r = SQLBindParameter(h_stmt,
				 bp->index,
				 SQL_PARAM_INPUT,
				 bp->value_type,
				 bp->param_type,
				 bp->col_size,
				 0,
				 bp->value,
				 bp->col_size,
				 &strlen);
  delete bp;
  if (r != SQL_SUCCESS 
      && r != SQL_SUCCESS_WITH_INFO)
    {
      _ret_ = _sql_return_to_symbol(r);
    }
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_prepare(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }
     
     
 if (!SCHEME_CHAR_STRINGP(argv[1]))
   {
     scheme_signal_error("Invalid type for sql command.");
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
 std::string sql = SCHEME_BYTE_STR_VAL(str);

 SQLRETURN r = SQLPrepare(h_stmt,
			  reinterpret_cast<SQLCHAR*>
			  (const_cast<char*>(sql.c_str())),
			  sql.length());
 _ret_ = _sql_return_to_symbol(r);

 DEFAULT_RET_FINISH;
}

static Bind_param*
_list_to_bind_param(Scheme_Object* obj)
{
  Scheme_Object* e = SCHEME_CAR(obj);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(e, i))
    {
      scheme_signal_error("Expected index.");
      return 0;
    }
  obj = SCHEME_CDR(obj);
  e = SCHEME_CAR(obj);
  int t = 0;
  if (!spark::Utils::int_from_scheme_long(e, t))
    {
      scheme_signal_error("Expected SQL type.");
      return 0;
    }
  if (obj == 0 || obj == scheme_null)
    {
      scheme_signal_error("Expected parameter value.");
      return 0;
    }
  obj = SCHEME_CDR(obj);
  e = SCHEME_CAR(obj);
  return new Bind_param(static_cast<SQLSMALLINT>(i),
			static_cast<SQLSMALLINT>(t), e);
}

// The Scheme call takes 2 arguments - the statement handle
// and a list in the following format:
// '('(index sql-type value))
// sql-type will be a symbol.
// The type of value will depend on sql-type.
Scheme_Object*
spark_odbc::sql_execute_with_params(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 SQLHSTMT h_stmt = _scheme_object_to_sql_hstmt(argv[0]);
 if (h_stmt == SQL_NULL_HANDLE)
   {
     scheme_signal_error("Null handle to SQL statement.");
     DEFAULT_RET_FINISH;
   }

 size_t sz = 0;
 Bind_params bps;
 if (SCHEME_LISTP(argv[1]))
   {
     Scheme_Object* lst = argv[1];
     while (lst && lst != scheme_null)
       {
	 Scheme_Object* obj = SCHEME_CAR(lst);
	 if (!SCHEME_LISTP(obj))
	   {
	     scheme_signal_error("Parameter should be a list.");
	     DEFAULT_RET_FINISH;
	   }
	 Bind_param* bp = _list_to_bind_param(obj);
	 if (bp == 0)
	   {
	     for (size_t j=0; j<bps.size(); ++j)
	       delete bps[j];
	     scheme_signal_error("Invalid parameter specification.");
	   }
	 bps.push_back(bp);
	 lst = SCHEME_CDR(lst);
       }
     
     SQLLEN strlen = 0;
     sz = bps.size();
     for (size_t j=0; j<sz; ++j)
       {
	 Bind_param* bp = bps[j];
	 if (bp->param_type == SQL_VARCHAR)
	   strlen = SQL_NTS;
	 SQLRETURN r = SQLBindParameter(h_stmt,
					bp->index,
					SQL_PARAM_INPUT,
					bp->value_type,
					bp->param_type,
					bp->col_size,
					0,
					bp->value,
					bp->col_size,
					&strlen);
	 if (r != SQL_SUCCESS 
	     && r != SQL_SUCCESS_WITH_INFO)
	   {
	     _ret_ = _sql_return_to_symbol(r);
	     for (size_t j=0; j<sz; ++j)
	       delete bps[j];
	     DEFAULT_RET_FINISH;
	   }				    
       }
   }
 
 SQLRETURN r = SQLExecute(h_stmt);
 _ret_ = _sql_return_to_symbol(r);

 for (size_t j=0; j<sz; ++j)
   delete bps[j];

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_set_env_attr(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  SQLHENV env_handle = 
    reinterpret_cast<SQLHENV>(_scheme_object_to_sql_handle(argv[0]));
  if (env_handle == 0 || env_handle == scheme_null)
    {
      scheme_signal_error("Invalid environment handle.");
      DEFAULT_RET_FINISH;
    }

  int i = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], i))
    {

      scheme_wrong_type("sql-set-env-handle", "int",
			1, argc,
			argv);
      DEFAULT_RET_FINISH;
    }
  SQLINTEGER attr = static_cast<SQLINTEGER>(i);
  switch (attr)
    {
    case SQL_ATTR_CONNECTION_POOLING:
      _ret_ = _set_uint_env_attr(env_handle, 
				 attr,
				 argv[2],
				 "connection pooling");
      break;
    case SQL_ATTR_OUTPUT_NTS:
      _ret_ = _set_int_env_attr(env_handle, 
				attr,
				argv[2],
				"output nts");
      break;
    case SQL_ATTR_ODBC_VERSION:
      _ret_ = _set_int_env_attr(env_handle, 
				attr,
				argv[2],
				"odbc version");
      break;
    case SQL_ATTR_CP_MATCH:
      _ret_ = _set_int_env_attr(env_handle, 
				attr,
				argv[2],
				"cp match");
      break;
    default:
      scheme_signal_error("Invalid attribute type.");
    }

  DEFAULT_RET_FINISH;
}

// helpers for sql_set_connect_attr.
Scheme_Object* _set_uint_connection_attr(SQLHDBC conn, 
					 SQLINTEGER attr,
					 Scheme_Object* attr_val,
					 const char* msg);
Scheme_Object* _set_int_connection_attr(SQLHDBC conn, 
					SQLINTEGER attr,
					Scheme_Object* attr_val,
					const char* msg);
Scheme_Object* _set_str_connection_attr(SQLHDBC conn, 
					SQLINTEGER attr,
					Scheme_Object* attr_val,
					const char* msg);
// :~

Scheme_Object*
spark_odbc::sql_set_connect_attr(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  SQLHDBC conn_handle = 
    reinterpret_cast<SQLHDBC>(_scheme_object_to_sql_handle(argv[0]));
  if (conn_handle == 0 || conn_handle == scheme_null)
    {
      scheme_signal_error("Invalid connection handle.");
      DEFAULT_RET_FINISH;
    }

  int i = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], i))
    {
      scheme_wrong_type("sql-set-connect-handle", "int",
			1, argc,
			argv);
      DEFAULT_RET_FINISH;
    }
  SQLINTEGER attr = static_cast<SQLINTEGER>(i);
  switch (attr)
    {
    case SQL_ATTR_ACCESS_MODE:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"access mode");
      break;
    case SQL_ATTR_ASYNC_ENABLE:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"async enable");
      break;
    case SQL_ATTR_AUTO_IPD:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"auto ipd");
      break;
    case SQL_ATTR_AUTOCOMMIT:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"autocommit");
      break;
    case SQL_ATTR_CONNECTION_DEAD:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"connection dead");
      break;
    case SQL_ATTR_CONNECTION_TIMEOUT:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"connection timeout");
      break;
    case SQL_ATTR_CURRENT_CATALOG:
      _ret_ = _set_str_connection_attr(conn_handle, 
				       attr,
				       argv[2],
				       "current catalog");
      break;
    case SQL_ATTR_LOGIN_TIMEOUT:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"login timeout");
      break;
    case SQL_ATTR_METADATA_ID:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"metadata id");
      break;
    case SQL_ATTR_ODBC_CURSORS:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"odbc cursors");
      break;
    case SQL_ATTR_PACKET_SIZE:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"packet size");
      break;
    case SQL_ATTR_QUIET_MODE:
      _ret_ = scheme_null;
      break;
    case SQL_ATTR_TRACE:
      _ret_ = _set_uint_connection_attr(conn_handle, 
					attr,
					argv[2],
					"trace");
      break;
    case SQL_ATTR_TRACEFILE:
      _ret_ = _set_str_connection_attr(conn_handle, 
				       attr,
				       argv[2],
				       "tracefile");
      break;
    case SQL_ATTR_TRANSLATE_LIB:
      _ret_ = _set_str_connection_attr(conn_handle, 
				       attr,
				       argv[2],
				       "translate lib");
      break;
    case SQL_ATTR_TRANSLATE_OPTION:
      _ret_ = _set_int_connection_attr(conn_handle, 
				       attr,
				       argv[2],
				       "translate option");
      break;
    case SQL_ATTR_TXN_ISOLATION:
      _ret_ = _set_int_connection_attr(conn_handle, 
				       attr,
				       argv[2],
				       "txn isolation");
      break;
    default:
      scheme_signal_error("Invalid attribute type.");
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_connect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  SQLHDBC conn_handle = 
    reinterpret_cast<SQLHDBC>(_scheme_object_to_sql_handle(argv[0]));
  if (conn_handle == 0 || conn_handle == scheme_null)
    {
      scheme_signal_error("Invalid connection handle.");
      DEFAULT_RET_FINISH;
    }

  if (!SCHEME_CHAR_STRINGP(argv[1]))
    {
      scheme_signal_error("Invalid type for server name.");
      DEFAULT_RET_FINISH;
    }
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
  std::string server_name = SCHEME_BYTE_STR_VAL(str);

  if (!SCHEME_CHAR_STRINGP(argv[2]))
    {
      scheme_signal_error("Invalid type for user name.");
      DEFAULT_RET_FINISH;
    }
  str = scheme_char_string_to_byte_string(argv[2]);
  std::string user_name = SCHEME_BYTE_STR_VAL(str);

  if (!SCHEME_CHAR_STRINGP(argv[3]))
    {
      scheme_signal_error("Invalid type for authentication.");
      DEFAULT_RET_FINISH;
    }
  str = scheme_char_string_to_byte_string(argv[3]);
  std::string auth = SCHEME_BYTE_STR_VAL(str);

  _ret_ = _sql_return_to_symbol
    (SQLConnect(conn_handle,
		reinterpret_cast<SQLCHAR*>(const_cast<char*>(server_name.c_str())),
		SQL_NTS,
		reinterpret_cast<SQLCHAR*>(const_cast<char*>(user_name.c_str())),
		SQL_NTS,
		reinterpret_cast<SQLCHAR*>(const_cast<char*>(auth.c_str())),
		SQL_NTS));

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_disconnect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  SQLHDBC conn_handle = 
    reinterpret_cast<SQLHDBC>(_scheme_object_to_sql_handle(argv[0]));
  if (conn_handle == 0 || conn_handle == scheme_null)
    {
      scheme_signal_error("Invalid connection handle.");
      DEFAULT_RET_FINISH;
    }

  _ret_ = _sql_return_to_symbol(SQLDisconnect(conn_handle));

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_odbc::sql_end_tran(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int t = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], t))
    {
      scheme_wrong_type("sql-end-tran", "int",
			0, argc,
			argv);
      DEFAULT_RET_FINISH;
    }
  SQLSMALLINT handle_type = static_cast<SQLSMALLINT>(t);
  SQLHANDLE input_handle = _scheme_object_to_sql_handle(argv[1]);
  t = 0;
  if (!spark::Utils::int_from_scheme_long(argv[2], t))
    {
      scheme_wrong_type("sql-end-tran", "int",
			2, argc,
			argv);
      DEFAULT_RET_FINISH;
    }
  SQLSMALLINT trans_type = static_cast<SQLSMALLINT>(t);
  _ret_ = _sql_return_to_symbol(SQLEndTran(handle_type, input_handle,
					   trans_type));  

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_odbc::sql_free_handle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

 int t = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], t))
   {
     scheme_wrong_type("sql-free-handle", "int",
                       0, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }
 SQLSMALLINT handle_type = static_cast<SQLSMALLINT>(t);
 SQLHANDLE input_handle = _scheme_object_to_sql_handle(argv[1]);

 _ret_ = _sql_return_to_symbol(SQLFreeHandle(handle_type,
					     input_handle));

 DEFAULT_RET_FINISH;
}

// Utilties

static const int SQLHANDLE_TAG = 200;

SQLHANDLE
_scheme_object_to_sql_handle(Scheme_Object* obj)
{
  if (!obj)
    return SQL_NULL_HANDLE;
  if (obj == scheme_null)
    return SQL_NULL_HANDLE;
  if (!SCHEME_CPTRP(obj))
    {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT, "Not a c-pointer.");
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(obj);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_raise_exn(MZEXN_FAIL, "Invalid type for SQLHANDLE tag.");
      return 0;
    }  
  if (i != SQLHANDLE_TAG)
    {
      scheme_raise_exn(MZEXN_FAIL, "Invalid SQLHANDLE tag.");
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(obj);
  if (!p)
    {
      scheme_raise_exn(MZEXN_FAIL, "Failed to get SQLHANDLE pointer.");
      return 0;
    }
  return reinterpret_cast<SQLHANDLE*>(p);
}

SQLHANDLE
_scheme_object_to_sql_hstmt(Scheme_Object* obj)
{
  SQLHANDLE h = _scheme_object_to_sql_handle(obj);
  if (h != SQL_NULL_HANDLE)
    return reinterpret_cast<SQLHSTMT*>(h);
  return SQL_NULL_HANDLE;
}

Scheme_Object*
_sql_handle_to_scheme_object(SQLHANDLE handle)
{
  if (!handle)
    return scheme_null;
  Scheme_Object* tag = scheme_make_integer(SQLHANDLE_TAG);
  return scheme_make_cptr(reinterpret_cast<void*>(handle), tag);
}

Scheme_Object*
_sql_return_to_symbol(SQLRETURN r, bool success_as_bool)
{
  switch (r)
    {
    case SQL_SUCCESS:
      {
	if (success_as_bool)
	  return scheme_true;
	return scheme_make_symbol("success");
      }
    case SQL_SUCCESS_WITH_INFO:
      return scheme_make_symbol("success-with-info");
    case SQL_INVALID_HANDLE:
      return scheme_make_symbol("invalid-handle");
    case SQL_ERROR:
      return scheme_make_symbol("error");
    }
  return scheme_make_symbol("unknown");
}

void
_extract_error(SQLHANDLE handle,
	       SQLSMALLINT type,
	       std::string& out_text)
{
  SQLINTEGER i = 0;
  SQLINTEGER native;
  SQLCHAR state[7];
  SQLCHAR text[256];
  SQLSMALLINT len;
  SQLRETURN ret;
  std::ostringstream out;
  do
    {
      ret = SQLGetDiagRec(type, handle, ++i, state, &native, text,
			  sizeof(text), &len);
      if (SQL_SUCCEEDED(ret))
	out << state << ", " << i << ", " << native << ", " << text << "\n";
    }
  while(ret == SQL_SUCCESS);
  out_text = out.str();
}

// helpers for sql_set_env_attr
Scheme_Object* 
_set_uint_env_attr(SQLHENV env, 
		   SQLINTEGER attr,
		   Scheme_Object* attr_value,
		   const char* attr_msg)
		   
{
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(attr_value, i))
    {
      std::ostringstream out;
      out << "Expected integer value for " << attr_msg;
      scheme_signal_error(out.str().c_str());
      return scheme_null;
    }
  SQLUINTEGER val = static_cast<SQLUINTEGER>(i);
  return _sql_return_to_symbol(SQLSetEnvAttr(env, 
					     attr,
					     reinterpret_cast<SQLPOINTER>(val),
					     0));
}

Scheme_Object* 
_set_int_env_attr(SQLHENV env, 
		  SQLINTEGER attr,
		  Scheme_Object* attr_value,
		  const char* attr_msg)
  
{
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(attr_value, i))
    {
      std::ostringstream out;
      out << "Expected integer value for " << attr_msg;
      scheme_signal_error(out.str().c_str());
      return scheme_null;
    }
  SQLINTEGER val = static_cast<SQLINTEGER>(i);
  return _sql_return_to_symbol(SQLSetEnvAttr(env, 
					     attr,
					     reinterpret_cast<SQLPOINTER>(val),
					     0));
}
// :~

// helpers for sql_set_connect_attr.
Scheme_Object* 
_set_uint_connection_attr(SQLHDBC conn, SQLINTEGER attr,		     
		     Scheme_Object* attr_val,
		     const char* attr_msg)
{
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(attr_val, i))
    {
      std::ostringstream out;
      out << "Expected integer value for " << attr_msg;
      scheme_signal_error(out.str().c_str());
      return scheme_null;
    }
  SQLUINTEGER val = static_cast<SQLUINTEGER>(i);
  return _sql_return_to_symbol(SQLSetConnectAttr(conn, 
						    attr,
						    reinterpret_cast<SQLPOINTER>(val),
						    0));
}

Scheme_Object* 
_set_int_connection_attr(SQLHDBC conn, SQLINTEGER attr,		     
			 Scheme_Object* attr_val,
			 const char* attr_msg)
{
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(attr_val, i))
    {
      std::ostringstream out;
      out << "Expected integer value for " << attr_msg;
      scheme_signal_error(out.str().c_str());
      return scheme_null;
    }
  SQLINTEGER val = static_cast<SQLINTEGER>(i);
  return _sql_return_to_symbol(SQLSetConnectAttr(conn, 
						 attr,
						 reinterpret_cast<SQLPOINTER>(val),
						 0));
}

Scheme_Object* 
_set_str_connection_attr(SQLHDBC conn, SQLINTEGER attr,		     
			 Scheme_Object* attr_val,
			 const char* attr_msg)
{
  if (!SCHEME_CHAR_STRINGP(attr_val))
    {
      std::ostringstream out;
      out << "Expected string value for " << attr_msg;
      scheme_signal_error(out.str().c_str());
      return scheme_null;
    }
  Scheme_Object* str = scheme_char_string_to_byte_string(attr_val);
  std::string val = SCHEME_BYTE_STR_VAL(str);
  return 
    _sql_return_to_symbol
    (SQLSetConnectAttr(conn, 
		       attr,
		       reinterpret_cast<SQLPOINTER>(const_cast<char*>(val.c_str())),
		       static_cast<SQLINTEGER>(val.length())));
}
// :~
