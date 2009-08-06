// MzScheme inetrface to the FLTK Spinner widget.
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

#include <FL/Fl_Spinner.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_spinner
{
  static Scheme_Object* fl_spinner(int, Scheme_Object**);
  static Scheme_Object* format(int, Scheme_Object**);
  static Scheme_Object* maximum(int, Scheme_Object**);
  static Scheme_Object* minimum(int, Scheme_Object**);
  static Scheme_Object* range(int, Scheme_Object**);
  static Scheme_Object* step(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
} // namespace spark_fltk_spinner

spark::Status_code
spark_fltk::_add_spinner_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_spinner::fl_spinner,
		  "fl-spinner", 5),
    new Procedure(spark_fltk_spinner::format, "spinner-value-format", 1, 2), 
    new Procedure(spark_fltk_spinner::maximum, "spinner-max", 1, 2), 
    new Procedure(spark_fltk_spinner::minimum, "spinner-min", 1, 2), 
    new Procedure(spark_fltk_spinner::range, "spinner-range", 3), 
    new Procedure(spark_fltk_spinner::step, "spinner-step", 1, 2), 
    new Procedure(spark_fltk_spinner::textcolor, "spinner-text-color", 1, 2), 
    new Procedure(spark_fltk_spinner::textfont, "spinner-text-font", 1, 2), 
    new Procedure(spark_fltk_spinner::textsize, "spinner-text-size", 1, 2), 
    new Procedure(spark_fltk_spinner::type, "spinner-type", 1, 2), 
    new Procedure(spark_fltk_spinner::value, "spinner-value", 1, 2), 
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_spinner_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("FL-INT-INPUT", FL_INT_INPUT),
    Constant("FL-FLOAT-INPUT", FL_FLOAT_INPUT),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Spinner* _get_fl_spinner(int argc, 
				   Scheme_Object** argv, 
				   int index);

Scheme_Object* 
spark_fltk_spinner::fl_spinner(int argc, Scheme_Object** argv)
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
  Fl_Spinner* spinner = new Fl_Spinner(x, y, w, h);
  if (title.length() > 0)
    spinner->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  spinner->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(spinner, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::format(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  const char* f = spinner->format();
	  if (f)
	    _ret_ = scheme_make_utf8_string(f);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("spinner-format", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string f = SCHEME_BYTE_STR_VAL(str);
	  spinner->format(f.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::maximum(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  double d = spinner->maximum();
	  _ret_ = scheme_make_double(d);
	}
      else
	{
	  double d = 0.0f;
	  if (spark::Utils::double_from_scheme_double(argv[1], d))
	    {
	      spinner->maximum(d);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::minimum(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  double d = spinner->minimum();
	  _ret_ = scheme_make_double(d);
	}
      else
	{
	  double d = 0.0f;
	  if (spark::Utils::double_from_scheme_double(argv[1], d))
	    {
	      spinner->minimum(d);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::range(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      double min = 0.0f;
      double max = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], min);
      spark::Utils::double_from_scheme_double(argv[2], max);
      spinner->range(min, max);
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::step(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  double d = spinner->step();
	  _ret_ = scheme_make_double(d);
	}
      else
	{
	  double d = 0.0f;
	  if (spark::Utils::double_from_scheme_double(argv[1], d))
	    {
	      spinner->step(d);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  int c = static_cast<int>(spinner->textcolor());
	  _ret_ = scheme_make_integer(c);
	}
      else
	{
	  int c = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], c))
	    {
	      spinner->textcolor(static_cast<Fl_Color>(c));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  int c = static_cast<int>(spinner->textfont());
	  _ret_ = scheme_make_integer(c);
	}
      else
	{
	  int c = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], c))
	    {
	      spinner->textfont(static_cast<Fl_Font>(c));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  int c = static_cast<int>(spinner->textsize());
	  _ret_ = scheme_make_integer(c);
	}
      else
	{
	  int c = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], c))
	    {
	      spinner->textsize(static_cast<uchar>(c));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  int c = static_cast<int>(spinner->type());
	  _ret_ = scheme_make_integer(c);
	}
      else
	{
	  int c = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], c))
	    {
	      spinner->type(static_cast<uchar>(c));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_spinner::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Spinner* spinner = _get_fl_spinner(argc, argv, 0);
  if (spinner)
    {
      if (argc == 1)
	{
	  double d = spinner->value();
	  _ret_ = scheme_make_double(d);
	}
      else
	{
	  double d = 0.0f;
	  if (spark::Utils::double_from_scheme_double(argv[1], d))
	    {
	      spinner->value(d);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Spinner*
_get_fl_spinner(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Spinner*>(widget);
}
