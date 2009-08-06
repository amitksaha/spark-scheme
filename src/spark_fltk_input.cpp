// MzScheme inetrface to the FLTK Input widget.
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

#include <FL/Fl_Input.H>
#include <FL/Fl_File_Input.H>
#include <FL/Fl_Float_Input.H>
#include <FL/Fl_Int_Input.H>
#include <FL/Fl_Multiline_Input.H>
#include <FL/Fl_Output.H> 
#include <FL/Fl_Secret_Input.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_input
{
  static Scheme_Object* fl_input(int, Scheme_Object**);
  // Fl_Input_
  static Scheme_Object* copy(int, Scheme_Object**);
  static Scheme_Object* copy_cuts(int, Scheme_Object**);
  static Scheme_Object* cut(int, Scheme_Object**);
  static Scheme_Object* input_type(int, Scheme_Object**);
  static Scheme_Object* insert(int, Scheme_Object**);
  static Scheme_Object* mark(int, Scheme_Object**);
  static Scheme_Object* maximum_size(int, Scheme_Object**);
  static Scheme_Object* position(int, Scheme_Object**);
  static Scheme_Object* readonly(int, Scheme_Object**);
  static Scheme_Object* replace(int, Scheme_Object**);
  static Scheme_Object* undo(int, Scheme_Object**);
  static Scheme_Object* wrap(int, Scheme_Object**);
  // Fl_Input
  static Scheme_Object* cursor_color(int, Scheme_Object**);
  static Scheme_Object* index(int, Scheme_Object**);
  static Scheme_Object* size(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* when(int, Scheme_Object**);
  // Fl_File_Input
  static Scheme_Object* down_box(int, Scheme_Object**);
}

spark::Status_code
spark_fltk::_add_input_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_input::fl_input, "fl-input", 5, 6),
    // Fl_Input_
    new Procedure(spark_fltk_input::copy, "copy", 2),
    new Procedure(spark_fltk_input::cut, "cut", 1, 3),
    new Procedure(spark_fltk_input::copy_cuts, "copy-cuts", 1),
    new Procedure(spark_fltk_input::input_type, "input-type", 1, 2),
    new Procedure(spark_fltk_input::insert, "insert-text", 2),
    new Procedure(spark_fltk_input::mark, "mark", 1, 2),
    new Procedure(spark_fltk_input::maximum_size, "maximum-size", 1, 2),
    new Procedure(spark_fltk_input::position, "selection-position", 1, 3),
    new Procedure(spark_fltk_input::readonly, "read-only", 1, 2),
    new Procedure(spark_fltk_input::replace, "replace-text", 4),
    new Procedure(spark_fltk_input::undo, "undo", 1),
    new Procedure(spark_fltk_input::wrap, "wrap-text", 1, 2),
    // Fl_Input
    new Procedure(spark_fltk_input::cursor_color, "input-cursor-color", 1, 2),
    new Procedure(spark_fltk_input::index, "char-at", 2),
    new Procedure(spark_fltk_input::size, "text-length", 1),
    new Procedure(spark_fltk_input::textcolor, "input-text-color", 1, 2),
    new Procedure(spark_fltk_input::textfont, "input-text-font", 1, 2),
    new Procedure(spark_fltk_input::textsize, "input-text-size", 1, 2),
    new Procedure(spark_fltk_input::value, "input-value", 1, 2),
    new Procedure(spark_fltk_input::when, "input-when", 1, 3),
    // Fl_File_Input
    new Procedure(spark_fltk_input::down_box, "file-input-down-box", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_input_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("NORMAL-INPUT", FL_NORMAL_INPUT),
    Constant("FLOAT-INPUT", FL_FLOAT_INPUT),
    Constant("INT-INPUT", FL_INT_INPUT),
    Constant("MULTILINE-INPUT", FL_MULTILINE_INPUT),
    Constant("SECRET-INPUT", FL_SECRET_INPUT),
    Constant("INPUT-TYPE", FL_INPUT_TYPE),
    Constant("INPUT-READONLY", FL_INPUT_READONLY),
    Constant("NORMAL-OUTPUT", FL_NORMAL_OUTPUT),
    Constant("MULTILINE-OUTPUT", FL_MULTILINE_OUTPUT),
    Constant("INPUT-WRAP", FL_INPUT_WRAP),
    Constant("MULTILINE-INPUT-WRAP", FL_MULTILINE_INPUT_WRAP),
    Constant("MULTILINE-OUTPUT-WRAP", FL_MULTILINE_OUTPUT_WRAP),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

Scheme_Object* 
spark_fltk_input::fl_input(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int x = 0; int y = 0; int w = 0; int h = 0;
  spark::Utils::int_from_scheme_long(argv[0], x);
  spark::Utils::int_from_scheme_long(argv[1], y);
  spark::Utils::int_from_scheme_long(argv[2], w);
  spark::Utils::int_from_scheme_long(argv[3], h);
  std::string title;
  if (argv[4] != scheme_null)
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
      title = SCHEME_BYTE_STR_VAL(str);
    }
  Fl_Input* input = 0;
  if (argc == 5)
    input = new Fl_Input(x, y, w, h);
  else
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-input", "symbol", 5, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[5]);
      if (s == "file")
	input = new Fl_File_Input(x, y, w, h);
      else if (s == "float")
	input = new Fl_Float_Input(x, y, w, h);
      else if (s == "int")
	input = new Fl_Int_Input(x, y, w, h, 0);
      else if (s == "multiline")
	input = new Fl_Multiline_Input(x, y, w, h);
      else if (s == "output")
	input = new Fl_Output(x, y, w, h);
      else if (s == "secret")
	input = new Fl_Secret_Input(x, y, w, h);
      else
	{
	  DEFAULT_RET_FINISH;
	}
    }
  if (title.length() > 0)
    input->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  input->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(input, tag);
  }

  DEFAULT_RET_FINISH;
}

static Fl_Input_* _get_fl_input_(int, Scheme_Object**, int);
static Fl_Input* _get_fl_input(int, Scheme_Object**, int);
static Fl_File_Input* _get_fl_file_input(int, Scheme_Object**, int);

Scheme_Object* 
spark_fltk_input::copy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	_ret_ = input->copy(i) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::copy_cuts(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    _ret_ = input->copy_cuts() ? scheme_true : scheme_false;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::cut(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = input->cut() ? scheme_true : scheme_false;
      else if (argc == 2)
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  _ret_ = input->cut(i) ? scheme_true : scheme_false;
	}
      else if (argc == 3)
	{
	  int i = 1;
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[i++], a);
	  int b = 0;
	  spark::Utils::int_from_scheme_long(argv[i], b);
	  _ret_ = input->cut(a, b) ? scheme_true : scheme_false;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::input_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(input->input_type());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  input->input_type(i);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::readonly(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = input->readonly() ? scheme_true : scheme_false;
      else
	{
	  int i = (argv[1] == scheme_true) ? 1 : 0;
	  input->readonly(i);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::mark(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(input->mark());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  input->mark(i);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::maximum_size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(input->maximum_size());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  input->maximum_size(i);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::insert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  input->insert(text.c_str());
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::replace(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (SCHEME_CHAR_STRINGP(argv[3]))
	{
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[1], a);
	  int b = 0;
	  spark::Utils::int_from_scheme_long(argv[2], b);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  _ret_ = 
	    input->replace(a, b, text.c_str()) ? scheme_true : scheme_false;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::undo(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    _ret_ = input->undo() ? scheme_true : scheme_false;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(input->position());
      else if (argc == 2)
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  input->position(i);
	  _ret_ = scheme_true;
	}
      else if (argc == 2)
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  int j = 0;
	  spark::Utils::int_from_scheme_long(argv[2], j);
	  input->position(i, j);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::wrap(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_* input = _get_fl_input_(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	_ret_ = input->wrap() ? scheme_true : scheme_false;
      else
	{
	  int i = (argv[1] == scheme_true) ? 1 : 0;
	  input->wrap(i);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::down_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Input* input = _get_fl_file_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = input->down_box();
	  int i = static_cast<int>(bt);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
	      input->down_box(bt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::cursor_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  Fl_Color v = input->cursor_color();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Color v = static_cast<Fl_Color>(i);
	      input->cursor_color(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  Fl_Color v = input->textcolor();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Color v = static_cast<Fl_Color>(i);
	      input->textcolor(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  Fl_Font v = input->textfont();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Font v = static_cast<Fl_Font>(i);
	      input->textfont(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  uchar v = input->textsize();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      uchar v = static_cast<uchar>(i);
	      input->textsize(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::index(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      _ret_ = scheme_make_char(static_cast<mzchar>(input->index(i)));
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    _ret_ = scheme_make_integer(input->size());
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  const char* v = input->value();
	  if (v)
	    _ret_ = scheme_make_utf8_string(v);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string text = SCHEME_BYTE_STR_VAL(str);
	      input->value(text.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_input::when(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input* input = _get_fl_input(argc, argv, 0);
  if (input)
    {
      if (argc == 1)
	{
	  Fl_When w = input->when();
	  long i = static_cast<long>(w);
	  _ret_ = scheme_make_integer(i);
	}
      else if (argc == 2)
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      input->when(static_cast<Fl_When>(i));
	      _ret_ = scheme_true;
	    }
	}
      else if (argc == 3)
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      int curr_w = static_cast<int>(input->when());
	      if (argv[2] == scheme_true) // remove
		curr_w &= ~i;
	      else
		curr_w |= i;
	      input->when(static_cast<Fl_When>(curr_w));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Input_*
_get_fl_input_(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Input_*>(widget);
}

Fl_Input*
_get_fl_input(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Input*>(widget);
}

Fl_File_Input*
_get_fl_file_input(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_File_Input*>(widget);
}
