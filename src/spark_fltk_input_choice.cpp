// MzScheme inetrface to the FLTK Input_Choice widget.
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

#include <FL/Fl_Input_Choice.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_input_choice
{
  static Scheme_Object* fl_input_choice(int, Scheme_Object**);
  static Scheme_Object* add(int, Scheme_Object**);
  static Scheme_Object* clear(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
} // namespace spark_fltk_input_choice

spark::Status_code
spark_fltk::_add_input_choice_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_input_choice::fl_input_choice, 
		  "fl-input-choice", 5),
    new Procedure(spark_fltk_input_choice::add, "add-choice", 2),
    new Procedure(spark_fltk_input_choice::clear, "clear-choices", 1),
    new Procedure(spark_fltk_input_choice::value, "current-choice", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Input_Choice* _get_fl_input_choice(int argc, 
					     Scheme_Object** argv, 
					     int index);

Scheme_Object* 
spark_fltk_input_choice::fl_input_choice(int argc, Scheme_Object** argv)
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
  Fl_Input_Choice* ic = new Fl_Input_Choice(x, y, w, h);
  if (title.length() > 0)
    ic->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  ic->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(ic, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_input_choice::add(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_Choice* ic = _get_fl_input_choice(argc, argv, 0);
  if (ic)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("add-choice", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string s = SCHEME_BYTE_STR_VAL(str);
      ic->add(s.c_str());
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_input_choice::clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_Choice* ic = _get_fl_input_choice(argc, argv, 0);
  if (ic)
    {
      ic->clear();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_input_choice::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Input_Choice* ic = _get_fl_input_choice(argc, argv, 0);
  if (ic)
    {
      if (argc == 1)
	{
	  const char* s = ic->value();
	  _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      ic->value(i);
	      _ret_ = scheme_true;
	    }
	  else
	    {
	      if (SCHEME_CHAR_STRINGP(argv[1]))
		{
		  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
		  std::string s = SCHEME_BYTE_STR_VAL(str);
		  ic->value(s.c_str());
		}
	      else
		{
		  int i = 0;
		  spark::Utils::int_from_scheme_long(argv[1], i);
		  ic->value(i);
		}
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Input_Choice*
_get_fl_input_choice(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Input_Choice*>(widget);
}
