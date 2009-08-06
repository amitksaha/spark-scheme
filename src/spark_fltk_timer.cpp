// MzScheme inetrface to the FLTK Timer widget.
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

#include <FL/Fl_Timer.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_timer
{
  static Scheme_Object* fl_timer(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* direction(int, Scheme_Object**);
  static Scheme_Object* suspended(int, Scheme_Object**);
} // namespace spark_fltk_timer

spark::Status_code
spark_fltk::_add_timer_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_timer::fl_timer, 
		  "fl-timer", 6),
    new Procedure(spark_fltk_timer::value, "timer-value", 1, 2),
    new Procedure(spark_fltk_timer::direction, "direction", 1, 2),
    new Procedure(spark_fltk_timer::suspended, "suspended", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Timer* _get_fl_timer(int argc, 
					Scheme_Object** argv, 
					int index);

Scheme_Object* 
spark_fltk_timer::fl_timer(int argc, Scheme_Object** argv)
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
  uchar type = 0;
  if (!SCHEME_SYMBOLP(argv[5]))
    scheme_wrong_type("fl-timer", "symbol", 5, argc, argv);
  std::string s = SCHEME_SYM_VAL(argv[5]);
  if (s == "normal")
    type = FL_NORMAL_TIMER;
  else if (s == "value")
    type = FL_VALUE_TIMER;
  else if (s == "hidden")
    type = FL_HIDDEN_TIMER;

  Fl_Timer* p = new Fl_Timer(type, x, y, w, h, "");
  if (title.length() > 0)
    p->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  Widget* widget = new Widget;
  p->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(p, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_timer::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Timer* p = _get_fl_timer(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  float v = p->value();
	  _ret_ = scheme_make_float(v);
	}
      else
	{
	  double x = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  p->value(static_cast<float>(x));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_timer::direction(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Timer* p = _get_fl_timer(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  char c = p->direction();
	  _ret_ = scheme_make_integer(static_cast<int>(c));
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  p->direction(static_cast<char>(i));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_timer::suspended(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Timer* p = _get_fl_timer(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  _ret_ = p->suspended() ? scheme_true : scheme_false;
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  p->suspended(static_cast<char>(i));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Timer*
_get_fl_timer(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Timer*>(widget);
}
