// MzScheme inetrface to the FLTK Clock_Output widget.
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

#include <FL/Fl_Clock.H>
#include <FL/Fl_Round_Clock.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_clock_output
{
  static Scheme_Object* fl_clock_output(int, Scheme_Object**);
  static Scheme_Object* fl_clock(int, Scheme_Object**);
  static Scheme_Object* hour(int, Scheme_Object**);
  static Scheme_Object* minute(int, Scheme_Object**);
  static Scheme_Object* second(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
} // namespace spark_fltk_clock_output

spark::Status_code
spark_fltk::_add_clock_output_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_clock_output::fl_clock_output, 
		  "fl-clock-output", 5),
    new Procedure(spark_fltk_clock_output::fl_clock, 
		  "fl-clock", 5, 6),
    new Procedure(spark_fltk_clock_output::hour, "hour", 1),
    new Procedure(spark_fltk_clock_output::minute, "minute", 1),
    new Procedure(spark_fltk_clock_output::second, "second", 1),
    new Procedure(spark_fltk_clock_output::value, "current-time", 1, 4),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Clock_Output* _get_fl_clock_output(int argc, 
					     Scheme_Object** argv, 
					     int index);

Scheme_Object* 
spark_fltk_clock_output::fl_clock_output(int argc, Scheme_Object** argv)
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
  Fl_Clock_Output* clock = new Fl_Clock_Output(x, y, w, h);
  if (title.length() > 0)
    clock->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  clock->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    _ret_ = scheme_make_cptr(clock, tag);
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_clock_output::fl_clock(int argc, Scheme_Object** argv)
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
  std::string s = "square";
  if (argc == 6)
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-clock", "symbol", 5, argc, argv);
      s = SCHEME_SYM_VAL(argv[5]);
    }
      
  Fl_Clock* clock = 0;
  if (s == "round")
    clock = new Fl_Round_Clock(x, y, w, h);
  else
    clock = new Fl_Clock(x, y, w, h);
  if (title.length())
    clock->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  clock->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    _ret_ = scheme_make_cptr(clock, tag);
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_clock_output::hour(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Clock_Output* clock = _get_fl_clock_output(argc, argv, 0);
  if (clock)
    {
      int i = clock->hour();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_clock_output::minute(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Clock_Output* clock = _get_fl_clock_output(argc, argv, 0);
  if (clock)
    {
      int i = clock->minute();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_clock_output::second(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Clock_Output* clock = _get_fl_clock_output(argc, argv, 0);
  if (clock)
    {
      int i = clock->second();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_clock_output::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Clock_Output* clock = _get_fl_clock_output(argc, argv, 0);
  if (clock)
    {
      if (argc == 1)
	{
	  ulong t = clock->value();
	  _ret_ = scheme_make_integer_value_from_unsigned(t);
	}
      else if (argc == 2)
	{
	  long t = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], t))
	    {
	      clock->value(static_cast<ulong>(t));
	      _ret_ = scheme_true;
	    }
	}
      else if (argc == 4)
	{
	  int h, m, s;
	  spark::Utils::int_from_scheme_long(argv[1], h);
	  spark::Utils::int_from_scheme_long(argv[2], m);
	  spark::Utils::int_from_scheme_long(argv[3], s);
	  clock->value(h, m, s);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Clock_Output*
_get_fl_clock_output(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Clock_Output*>(widget);
}
