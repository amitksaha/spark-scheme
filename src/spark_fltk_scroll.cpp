// MzScheme inetrface to the FLTK Scroll widget.
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

#include <FL/Fl_Scroll.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_scroll
{
  static Scheme_Object* fl_scroll(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* align(int, Scheme_Object**);
  static Scheme_Object* xposition(int, Scheme_Object**);
  static Scheme_Object* yposition(int, Scheme_Object**);
  static Scheme_Object* position(int, Scheme_Object**);
} // namespace spark_fltk_scroll

spark::Status_code
spark_fltk::_add_scroll_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_scroll::fl_scroll,
		  "fl-scroll", 5),
    new Procedure(spark_fltk_scroll::type, "scroll-type", 2),
    new Procedure(spark_fltk_scroll::align, "scroll-align", 2),
    new Procedure(spark_fltk_scroll::xposition, "scroll-xpos", 1),
    new Procedure(spark_fltk_scroll::yposition, "scroll-ypos", 1),
    new Procedure(spark_fltk_scroll::position, "scroll-pos", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_scroll_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("FL-SCROLL-HORIZONTAL", Fl_Scroll::HORIZONTAL),
    Constant("FL-SCROLL-VERTICAL", Fl_Scroll::VERTICAL),
    Constant("FL-SCROLL-BOTH", Fl_Scroll::BOTH),
    Constant("FL-SCROLL-HORIZONTAL-ALWAYS", Fl_Scroll::HORIZONTAL_ALWAYS),
    Constant("FL-SCROLL-VERTICAL-ALWAYS", Fl_Scroll::VERTICAL_ALWAYS),
    Constant("FL-SCROLL-BOTH-ALWAYS", Fl_Scroll::BOTH_ALWAYS),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Scroll* _get_fl_scroll(int argc, 
				 Scheme_Object** argv, 
				 int index);

Scheme_Object* 
spark_fltk_scroll::fl_scroll(int argc, Scheme_Object** argv)
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
  Fl_Scroll* scroll = new Fl_Scroll(x, y, w, h);
  if (title.length() > 0)
    scroll->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  scroll->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(scroll, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_scroll::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scroll* scroll = _get_fl_scroll(argc, argv, 0);
  if (scroll)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  scroll->type(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_scroll::align(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scroll* scroll = _get_fl_scroll(argc, argv, 0);
  if (scroll)
    {
      if (argc == 1)
	{
	  int i = scroll->align();
	  _ret_ = spark_fltk::align_to_intlist(i);	  
	}
      else
	{
	  int i = spark_fltk::intlist_to_flag(argv[1]);
	  scroll->align(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_scroll::xposition(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scroll* scroll = _get_fl_scroll(argc, argv, 0);
  if (scroll)
    _ret_ = scheme_make_integer(scroll->xposition());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_scroll::yposition(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scroll* scroll = _get_fl_scroll(argc, argv, 0);
  if (scroll)
    _ret_ = scheme_make_integer(scroll->yposition());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_scroll::position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scroll* scroll = _get_fl_scroll(argc, argv, 0);
  if (scroll)
    {
      int x, y;
      spark::Utils::int_from_scheme_long(argv[1], x);
      spark::Utils::int_from_scheme_long(argv[2], y);
      scroll->position(x, y);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Fl_Scroll*
_get_fl_scroll(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Scroll*>(widget);
}
