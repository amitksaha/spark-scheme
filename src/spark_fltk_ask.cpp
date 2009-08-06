// MzScheme inetrface to the FLTK message box API.
// Copyright (C) 2008  Vijay Mathew Pandyalakal
 
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

#include <FL/fl_ask.H>
#include "spark_fltk.h"
#include "spark_fltk_common.h"
using namespace spark_fltk;

// exported function signatures
namespace spark_fltk_ask
{
  static Scheme_Object* choice(int, Scheme_Object**);
  static Scheme_Object* message(int, Scheme_Object**);
  static Scheme_Object* alert(int, Scheme_Object**);
  static Scheme_Object* beep(int, Scheme_Object**);
  static Scheme_Object* message_icon(int, Scheme_Object**);
  static Scheme_Object* message_font(int, Scheme_Object**);
  static Scheme_Object* input(int, Scheme_Object**);
  static Scheme_Object* password(int, Scheme_Object**);
} // namespace spark_fltk_ask

spark::Status_code
spark_fltk::_add_ask_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    // alignments
    Constant("FL-BEEP-DEFAULT", FL_BEEP_DEFAULT),
    Constant("FL-BEEP-MESSAGE", FL_BEEP_MESSAGE),
    Constant("FL-BEEP-ERROR", FL_BEEP_ERROR),
    Constant("FL-BEEP-QUESTION", FL_BEEP_QUESTION),
    Constant("FL-BEEP-PASSWORD", FL_BEEP_PASSWORD),
    Constant("FL-BEEP-NOTIFICATION", FL_BEEP_NOTIFICATION),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_ask_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_ask::choice, "choices", 1, 4),
    new Procedure(spark_fltk_ask::message, "message", 1),
    new Procedure(spark_fltk_ask::beep, "beep", 0, 1),
    new Procedure(spark_fltk_ask::alert, "alert", 1),
    new Procedure(spark_fltk_ask::message_icon, "message-icon", 0),
    new Procedure(spark_fltk_ask::message_font, "message-font", 2),
    new Procedure(spark_fltk_ask::input, "input-dialog", 1, 2),
    new Procedure(spark_fltk_ask::password, "password-dialog", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

Scheme_Object*
spark_fltk_ask::choice(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  if (!SCHEME_CHAR_STRINGP(argv[i]))
    scheme_wrong_type("choices", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[i++]);
  std::string label = SCHEME_BYTE_STR_VAL(str);
  std::string b0;
  if (argc >= 2)
    {
      if (SCHEME_CHAR_STRINGP(argv[i]))
	{
	  str = scheme_char_string_to_byte_string(argv[i++]);
	  b0 = SCHEME_BYTE_STR_VAL(str);
	}
    }
  std::string b1;
  if (argc >= 3)
    {
      if (SCHEME_CHAR_STRINGP(argv[i]))
	{
	  str = scheme_char_string_to_byte_string(argv[i++]);
	  b1 = SCHEME_BYTE_STR_VAL(str);
	}
    }
  std::string b2;
  if (argc >= 4)
    {
      if (SCHEME_CHAR_STRINGP(argv[i]))
	{
	  str = scheme_char_string_to_byte_string(argv[i++]);
	  b2 = SCHEME_BYTE_STR_VAL(str);
	}
    }
  _ret_ = scheme_make_integer(fl_choice(label.c_str(), 
					b0.c_str(), 
					b1.c_str(), 
					b2.c_str()));

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::message(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("message", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string msg = SCHEME_BYTE_STR_VAL(str);
  fl_message(msg.c_str());
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::alert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("alert", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string msg = SCHEME_BYTE_STR_VAL(str);
  fl_alert(msg.c_str());
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::beep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = FL_BEEP_DEFAULT;
  spark::Utils::int_from_scheme_long(argv[0], i);
  fl_beep(i);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::message_icon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* w = fl_message_icon();
  if (w)
    {
      Fltk_tag t = FL_WIDGET_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	MZ_GC_UNREG();
	_ret_ = scheme_make_cptr(w, tag);
      }
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::message_font(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int f = 0;
  spark::Utils::int_from_scheme_long(argv[0], f);
  int s = 0;
  spark::Utils::int_from_scheme_long(argv[1], s);
  fl_message_font(static_cast<Fl_Font>(f),
		  static_cast<uchar>(s));
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::input(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("get-input", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string label = SCHEME_BYTE_STR_VAL(str);
  const char* def = 0;
  if (argc >= 2)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  str = scheme_char_string_to_byte_string(argv[1]);
	  def = SCHEME_BYTE_STR_VAL(str);
	}
    }
  const char* r = fl_input(label.c_str(), def);
  if (r)
    _ret_ = scheme_make_utf8_string(r);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_ask::password(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("get-password", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string label = SCHEME_BYTE_STR_VAL(str);
  const char* def = 0;
  if (argc >= 2)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  str = scheme_char_string_to_byte_string(argv[1]);
	  def = SCHEME_BYTE_STR_VAL(str);
	}
    }
  const char* r = fl_password(label.c_str(), def);
  if (r)
    _ret_ = scheme_make_utf8_string(r);

  DEFAULT_RET_FINISH;
}

