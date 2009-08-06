// MzScheme inetrface to the FLTK Button widget.
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

#include <FL/Fl_Button.H>
#include <FL/Fl_Check_Button.H>
#include <FL/Fl_Repeat_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Light_Button.H>
#include <FL/Fl_Round_Button.H>
#include <FL/Fl_Toggle_Button.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_button
{
  static Scheme_Object* fl_button(int, Scheme_Object**);
  // Fl_Button
  static Scheme_Object* state(int, Scheme_Object**);
  static Scheme_Object* setonly(int, Scheme_Object**);
  static Scheme_Object* down_box(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* when(int, Scheme_Object**);
}

spark::Status_code
spark_fltk::_add_button_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_button::fl_button, "fl-button", 5, 6),
    // Fl_Button
    new Procedure(spark_fltk_button::state, "button-state", 1, 2),
    new Procedure(spark_fltk_button::setonly, "set-only", 1),
    new Procedure(spark_fltk_button::down_box, "down-box", 1, 2),
    new Procedure(spark_fltk_button::type, "button-type", 1, 2),
    new Procedure(spark_fltk_button::when, "button-when", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_button_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("FL-TOGGLE-BUTTON", FL_TOGGLE_BUTTON),
    Constant("FL-RADIO-BUTTON", FL_RADIO_BUTTON),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

Scheme_Object* 
spark_fltk_button::fl_button(int argc, Scheme_Object** argv)
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
  Fl_Button* button = 0;
  if (argc == 5)
    button = new Fl_Button(x, y, w, h);
  else
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-button", "symbol", 5, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[5]);
      if (s == "check")
	button = new Fl_Check_Button(x, y, w, h);
      else if (s == "light")
	button = new Fl_Light_Button(x, y, w, h);
      else if (s == "repeat")
	button = new Fl_Repeat_Button(x, y, w, h);
      else if (s == "return")
	button = new Fl_Return_Button(x, y, w, h);
      else if (s == "radio" || s == "round")
	button = new Fl_Round_Button(x, y, w, h);
      else if (s == "toggle")
	button = new Fl_Toggle_Button(x, y, w, h);
      else
	{
	  DEFAULT_RET_FINISH;
	}
    }
  if (title.length() > 0)
    button->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  button->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(button, tag);
  }

  DEFAULT_RET_FINISH;
}

static Fl_Button* _get_fl_button(int, Scheme_Object**, int);

Scheme_Object* 
spark_fltk_button::state(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Button* button = _get_fl_button(argc, argv, 0);
  if (button)
    {
      if (argc == 1)
	{
	  char c = button->value();
	  _ret_ = c ? scheme_true : scheme_false;
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      button->value(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_button::setonly(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Button* button = _get_fl_button(argc, argv, 0);
  if (button)
    {
      button->setonly();
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_button::down_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Button* button = _get_fl_button(argc, argv, 0);
  if (button)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = button->down_box();
	  int i = static_cast<int>(bt);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
	      button->down_box(bt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_button::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Button* button = _get_fl_button(argc, argv, 0);
  if (button)
    {
      if (argc == 1)
	{
	  int i = button->type();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      button->type(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_button::when(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Button* button = _get_fl_button(argc, argv, 0);
  if (button)
    {
      if (argc == 1)
	{
	  Fl_When w = button->when();
	  long i = static_cast<long>(w);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_When w;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      w = static_cast<Fl_When>(i);
	      button->when(w);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Button*
_get_fl_button(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Button*>(widget);
}
