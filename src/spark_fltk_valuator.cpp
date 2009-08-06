// MzScheme inetrface to the FLTK Valuator widget.
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

#include <FL/Fl_Valuator.H>
#include <FL/Fl_Adjuster.H>
#include <FL/Fl_Counter.H>
#include <FL/Fl_Dial.H>
#include <FL/Fl_Roller.H>
#include <FL/Fl_Slider.H>
#include <FL/Fl_Scrollbar.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/Fl_Value_Input.H>
#include <FL/Fl_Value_Output.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_valuator
{
  static Scheme_Object* fl_valuator(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* int_value(int, Scheme_Object**);
  static Scheme_Object* bounds(int, Scheme_Object**);
  static Scheme_Object* changed(int, Scheme_Object**);
  static Scheme_Object* clear_changed(int, Scheme_Object**);
  static Scheme_Object* clamp(int, Scheme_Object**);
  static Scheme_Object* format(int, Scheme_Object**);
  static Scheme_Object* increment(int, Scheme_Object**);
  static Scheme_Object* maximum(int, Scheme_Object**);
  static Scheme_Object* minimum(int, Scheme_Object**);
  static Scheme_Object* precision(int, Scheme_Object**);
  static Scheme_Object* range(int, Scheme_Object**);
  static Scheme_Object* round(int, Scheme_Object**);
  static Scheme_Object* set_changed(int, Scheme_Object**);
  static Scheme_Object* step(int, Scheme_Object**);
  // Fl_Adjuster
  static Scheme_Object* adjuster_soft(int, Scheme_Object**);
  // Fl_Counter
  static Scheme_Object* counter_lstep(int, Scheme_Object**);
  static Scheme_Object* counter_type(int, Scheme_Object**);
  // Fl_Dial
  static Scheme_Object* dial_angle1(int, Scheme_Object**);
  static Scheme_Object* dial_angle2(int, Scheme_Object**);
  static Scheme_Object* dial_angles(int, Scheme_Object**);
  static Scheme_Object* dial_type(int, Scheme_Object**);
  // Fl_Slider
  static Scheme_Object* scrollvalue(int, Scheme_Object**);
  static Scheme_Object* slider(int, Scheme_Object**);
  static Scheme_Object* slider_size(int, Scheme_Object**);
  static Scheme_Object* slider_type(int, Scheme_Object**);
  // Fl_Value_Slider
  static Scheme_Object* value_slider_textcolor(int, Scheme_Object**);
  static Scheme_Object* value_slider_textfont(int, Scheme_Object**);
  static Scheme_Object* value_slider_textsize(int, Scheme_Object**);
  // Fl_Scrollbar
  static Scheme_Object* linesize(int, Scheme_Object**);
  static Scheme_Object* scrollbar_value(int, Scheme_Object**);
  // Fl_Value_Input
  static Scheme_Object* vi_cursor_color(int, Scheme_Object**);
  static Scheme_Object* vi_textcolor(int, Scheme_Object**);
  static Scheme_Object* vi_textfont(int, Scheme_Object**);
  static Scheme_Object* vi_textsize(int, Scheme_Object**);
  static Scheme_Object* vi_soft(int, Scheme_Object**);
  // Fl_Value_Output
  static Scheme_Object* vo_textcolor(int, Scheme_Object**);
  static Scheme_Object* vo_textfont(int, Scheme_Object**);
  static Scheme_Object* vo_textsize(int, Scheme_Object**);
  static Scheme_Object* vo_soft(int, Scheme_Object**);
} // namespace spark_fltk_valuator

spark::Status_code
spark_fltk::_add_valuator_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_valuator::fl_valuator, 
		  "fl-valuator", 6),
    new Procedure(spark_fltk_valuator::value, "valuator-value", 1, 2),
    new Procedure(spark_fltk_valuator::int_value, "valuator-int-value", 1),
    new Procedure(spark_fltk_valuator::bounds, "valuator-bounds", 3),
    new Procedure(spark_fltk_valuator::changed, "valuator-changed", 1),
    new Procedure(spark_fltk_valuator::clear_changed, 
		  "valuator-clear-changed", 1),
    new Procedure(spark_fltk_valuator::clamp, "clamp", 2),
    new Procedure(spark_fltk_valuator::format, "valuator-format", 1),
    new Procedure(spark_fltk_valuator::increment, "valuator-incr", 3),
    new Procedure(spark_fltk_valuator::maximum, "valuator-max", 1, 2),
    new Procedure(spark_fltk_valuator::minimum, "valuator-min", 1, 2),
    new Procedure(spark_fltk_valuator::precision, "valuator-precision", 2),
    new Procedure(spark_fltk_valuator::range, "valuator-range", 3),
    new Procedure(spark_fltk_valuator::round, "valuator-round", 2),
    new Procedure(spark_fltk_valuator::set_changed, 
		  "valuator-set-changed", 1),
    new Procedure(spark_fltk_valuator::step, "valuator-step", 1, 2),
    // Fl_Adjuster
    new Procedure(spark_fltk_valuator::adjuster_soft, 
		  "adjuster-soft", 1, 2),
    // Fl_Counter
    new Procedure(spark_fltk_valuator::counter_lstep, 
		  "counter-lstep", 2),
    new Procedure(spark_fltk_valuator::counter_type, 
		  "counter-type", 2),
    // Fl_Dial
    new Procedure(spark_fltk_valuator::dial_angle1, 
		  "dial-angle1", 1, 2),
    new Procedure(spark_fltk_valuator::dial_angle2, 
		  "dial-angle2", 1, 2),
    new Procedure(spark_fltk_valuator::dial_angles, 
		  "dial-angles", 3),
    new Procedure(spark_fltk_valuator::dial_type, 
		  "dial-type", 2),
    // Fl_Slider
    new Procedure(spark_fltk_valuator::scrollvalue, 
		  "scrollvalue", 5),
    new Procedure(spark_fltk_valuator::slider, 
		  "slider", 1, 2),
    new Procedure(spark_fltk_valuator::slider_size, 
		  "slider-size", 1, 2),
    new Procedure(spark_fltk_valuator::slider_type, 
		  "slider-type", 1, 2),
    // Fl_Value_Slider
    new Procedure(spark_fltk_valuator::value_slider_textcolor, 
		  "value-slider-textcolor", 1, 2),
    new Procedure(spark_fltk_valuator::value_slider_textfont, 
		  "value-slider-textfont", 1, 2),
    new Procedure(spark_fltk_valuator::value_slider_textsize, 
		  "value-slider-textsize", 1, 2),
    // Fl_Scrollbar
    new Procedure(spark_fltk_valuator::linesize, 
		  "scrollbar-linesize", 1, 2),
    new Procedure(spark_fltk_valuator::scrollbar_value, 
		  "scrollbar-value", 1, 4),
    // Fl_Value_Input
    new Procedure(spark_fltk_valuator::vi_cursor_color, 
		  "vi-cursorcolor", 1, 2),
    new Procedure(spark_fltk_valuator::vi_textcolor, 
		  "vi-textcolor", 1, 2),
    new Procedure(spark_fltk_valuator::vi_textfont, 
		  "vi-textfont", 1, 2),
    new Procedure(spark_fltk_valuator::vi_textsize, 
		  "vi-textsize", 1, 2),
    new Procedure(spark_fltk_valuator::vi_soft, 
		  "vi-soft", 1, 2),
    // Fl_Value_Output
    new Procedure(spark_fltk_valuator::vo_textcolor, 
		  "vo-textcolor", 1, 2),
    new Procedure(spark_fltk_valuator::vo_textfont, 
		  "vo-textfont", 1, 2),
    new Procedure(spark_fltk_valuator::vo_textsize, 
		  "vo-textsize", 1, 2),
    new Procedure(spark_fltk_valuator::vo_soft, 
		  "vo-soft", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_valuator_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("FL-NORMAL-COUNTER", FL_NORMAL_COUNTER),
    Constant("FL-SIMPLE-COUNTER", FL_SIMPLE_COUNTER),
    Constant("FL-NORMAL-DIAL", FL_NORMAL_DIAL),
    Constant("FL-LINE-DIAL", FL_LINE_DIAL),
    Constant("FL-FILL-DIAL", FL_FILL_DIAL),
    Constant("FL-VERTICAL-SLIDER", FL_VERTICAL),
    Constant("FL-HORIZONTAL-SLIDER", FL_HORIZONTAL),
    Constant("FL-VERT-SLIDER", FL_VERT_SLIDER),
    Constant("FL-HOR-SLIDER", FL_HOR_SLIDER),
    Constant("FL-VERT-FILL-SLIDER", FL_VERT_FILL_SLIDER),
    Constant("FL-HOR-FILL-SLIDER", FL_HOR_FILL_SLIDER),
    Constant("FL-VERT-NICE-SLIDER", FL_VERT_NICE_SLIDER),
    Constant("FL-HOR-NICE-SLIDER", FL_HOR_NICE_SLIDER),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Valuator* _get_fl_valuator(int argc, 
					Scheme_Object** argv, 
					int index);
static Fl_Adjuster* _get_fl_adjuster(int argc, 
				     Scheme_Object** argv, 
				     int index);
static Fl_Counter* _get_fl_counter(int argc, 
				   Scheme_Object** argv, 
				   int index);
static Fl_Dial* _get_fl_dial(int argc, 
			     Scheme_Object** argv, 
			     int index);
static Fl_Slider* _get_fl_slider(int argc, 
				 Scheme_Object** argv, 
				 int index);
static Fl_Value_Slider* _get_fl_value_slider(int argc, 
					     Scheme_Object** argv, 
					     int index);
static Fl_Scrollbar* _get_fl_scrollbar(int argc, 
				       Scheme_Object** argv, 
				       int index);
static Fl_Value_Input* _get_fl_value_input(int argc, 
					   Scheme_Object** argv, 
					   int index);
static Fl_Value_Output* _get_fl_value_output(int argc, 
					     Scheme_Object** argv, 
					     int index);

Scheme_Object* 
spark_fltk_valuator::fl_valuator(int argc, Scheme_Object** argv)
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
  Fl_Valuator* p = 0;
  if (!SCHEME_SYMBOLP(argv[5]))
    scheme_wrong_type("fl-valuator", "symbol", 5, argc, argv);
  std::string s = SCHEME_SYM_VAL(argv[5]);
  if (s == "adjuster")
    p = new Fl_Adjuster(x, y, w, h);
  else if (s == "counter")
    p = new Fl_Counter(x, y, w, h);
  else if (s == "dial")
    p = new Fl_Dial(x, y, w, h);
  else if (s == "roller")
    p = new Fl_Roller(x, y, w, h);
  else if (s == "slider")
    p = new Fl_Slider(x, y, w, h);
  else if (s == "value-slider")
    p = new Fl_Value_Slider(x, y, w, h);
  else if (s == "scroll-bar")
    p = new Fl_Scrollbar(x, y, w, h);
  else if (s == "value-input")
    p = new Fl_Value_Input(x, y, w, h);
  else if (s == "value-outut")
    p = new Fl_Value_Output(x, y, w, h);
  else
    {
      DEFAULT_RET_FINISH;
    }
  if (title.length() > 0)
    p->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new Widget;
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
spark_fltk_valuator::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* p = _get_fl_valuator(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  double v = p->value();
	  _ret_ = scheme_make_double(v);
	}
      else
	{
	  double x = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  p->value(x);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::int_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* p = _get_fl_valuator(argc, argv, 0);
  if (p)
    {
      long v = static_cast<long>(p->value());
      _ret_ = scheme_make_integer(v);
    }
     
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::bounds(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      double a = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], a);
      double b = 0.0f;
      spark::Utils::double_from_scheme_double(argv[2], b);
      fv->bounds(a, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    _ret_ = fv->changed() ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::clamp(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      double a = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], a);
      double r = fv->clamp(a);
      _ret_ = scheme_make_double(r);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::clear_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      fv->clear_changed();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::format(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      const int sz = 255;
      char text[sz];
      int r = fv->format(text);
      if (r >= sz)
	r = (sz - 1);
      text[r] = 0;
      _ret_ = scheme_make_utf8_string(text);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::increment(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      double a = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], a);
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[2], i);
      double r = fv->increment(a, i);
      _ret_ = scheme_make_double(r);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::maximum(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  double v = fv->maximum();
	  _ret_ = scheme_make_double(v);
	}
      else
	{
	  double a = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], a);
	  fv->maximum(a);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::minimum(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  double v = fv->minimum();
	  _ret_ = scheme_make_double(v);
	}
      else
	{
	  double a = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], a);
	  fv->minimum(a);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::precision(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      fv->precision(i);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::range(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      double a = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], a);
      double b = 0.0f;
      spark::Utils::double_from_scheme_double(argv[2], b);
      fv->range(a, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::round(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      double a = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], a);
      double r = fv->round(a);
      _ret_ = scheme_make_double(r);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::set_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      fv->set_changed();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::step(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Valuator* fv = _get_fl_valuator(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  double v = fv->step();
	  _ret_ = scheme_make_double(v);
	}
      else
	{
	  double a = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], a);
	  fv->step(a);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::adjuster_soft(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Adjuster* fv = _get_fl_adjuster(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	_ret_ = fv->soft() ? scheme_true : scheme_false;
      else
	{
	  if (argv[1] == scheme_true)
	    fv->soft(1);
	  else
	    fv->soft(0);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::counter_lstep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Counter* fv = _get_fl_counter(argc, argv, 0);
  if (fv)
    {
      double d = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], d);
      fv->lstep(d);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::counter_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Counter* fv = _get_fl_counter(argc, argv, 0);
  if (fv)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      fv->type(static_cast<uchar>(i));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::dial_angle1(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Dial* fv = _get_fl_dial(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  short v = fv->angle1();
	  _ret_ = scheme_make_integer(v);
	}
      else
	{
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[1], a);
	  fv->angle1(a);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::dial_angle2(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Dial* fv = _get_fl_dial(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  short v = fv->angle2();
	  _ret_ = scheme_make_integer(v);
	}
      else
	{
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[1], a);
	  fv->angle2(a);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::dial_angles(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Dial* fv = _get_fl_dial(argc, argv, 0);
  if (fv)
    {
      int a = 0;
      spark::Utils::int_from_scheme_long(argv[1], a);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[2], b);
      fv->angles(a, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::dial_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Dial* fv = _get_fl_dial(argc, argv, 0);
  if (fv)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      fv->type(static_cast<uchar>(i));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::linesize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scrollbar* sb = _get_fl_scrollbar(argc, argv, 0);
  if (sb)
    {
      if (argc == 0)
	_ret_ = scheme_make_integer(sb->linesize());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  sb->linesize(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::scrollbar_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Scrollbar* sb = _get_fl_scrollbar(argc, argv, 0);
  if (sb)
    {
      if (argc == 0)
	_ret_ = scheme_make_integer(sb->value());
      else
	{
	  int p = 0;
	  spark::Utils::int_from_scheme_long(argv[1], p);
	  int size = 0;
	  spark::Utils::int_from_scheme_long(argv[2], size);
	  int top = 0;
	  spark::Utils::int_from_scheme_long(argv[3], top);
	  int total = 0;
	  spark::Utils::int_from_scheme_long(argv[4], total);
	  sb->value(p, size, top, total);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::scrollvalue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Slider* fv = _get_fl_slider(argc, argv, 0);
  if (fv)
    {
      int a = 0;
      spark::Utils::int_from_scheme_long(argv[1], a);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[2], b);
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[3], c);
      int d = 0;
      spark::Utils::int_from_scheme_long(argv[4], d);
      int i = fv->scrollvalue(a, b, c, d);
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::slider(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Slider* fv = _get_fl_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = fv->slider();
	  _ret_ = scheme_make_integer(static_cast<int>(bt));
	}
      else
	{
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[1], a);
	  fv->slider(static_cast<Fl_Boxtype>(a));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::slider_size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Slider* fv = _get_fl_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  float a = fv->slider_size();
	  _ret_ = scheme_make_float(a);
	}
      else
	{
	  double d = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], d);
	  fv->slider_size(static_cast<float>(d));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::slider_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Slider* fv = _get_fl_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->type();
	  _ret_ = scheme_make_integer(static_cast<int>(c));
	}
      else
	{
	  int a = 0;
	  spark::Utils::int_from_scheme_long(argv[1], a);
	  fv->type(static_cast<uchar>(a));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vi_cursor_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Input* fv = _get_fl_value_input(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  Fl_Color c = fv->cursor_color();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Color c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Color>(i);
	      fv->cursor_color(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vi_textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Input* fv = _get_fl_value_input(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  Fl_Color c = fv->textcolor();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Color c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Color>(i);
	      fv->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vi_textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Input* fv = _get_fl_value_input(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textfont();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Font c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Font>(i);
	      fv->textfont(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vi_textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Input* fv = _get_fl_value_input(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      fv->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vi_soft(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Input* fv = _get_fl_value_input(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->soft();
	  _ret_ = c ? scheme_true : scheme_false;
	}
      else
	{
	  if (argv[1] == scheme_true)
	    fv->soft(1);
	  else
	    fv->soft(0);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vo_textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Output* fv = _get_fl_value_output(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  Fl_Color c = fv->textcolor();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Color c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Color>(i);
	      fv->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vo_textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Output* fv = _get_fl_value_output(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textfont();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Font c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Font>(i);
	      fv->textfont(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vo_textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Output* fv = _get_fl_value_output(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      fv->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::vo_soft(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Output* fv = _get_fl_value_output(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->soft();
	  _ret_ = c ? scheme_true : scheme_false;
	}
      else
	{
	  if (argv[1] == scheme_true)
	    fv->soft(1);
	  else
	    fv->soft(0);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::value_slider_textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Slider* fv = _get_fl_value_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  Fl_Color c = fv->textcolor();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Color c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Color>(i);
	      fv->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::value_slider_textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Slider* fv = _get_fl_value_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textfont();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Font c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<Fl_Font>(i);
	      fv->textfont(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_valuator::value_slider_textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Value_Slider* fv = _get_fl_value_slider(argc, argv, 0);
  if (fv)
    {
      if (argc == 1)
	{
	  uchar c = fv->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      fv->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Valuator*
_get_fl_valuator(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Valuator*>(widget);
}

Fl_Adjuster*
_get_fl_adjuster(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Adjuster*>(widget);
}

Fl_Counter*
_get_fl_counter(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Counter*>(widget);
}

Fl_Dial*
_get_fl_dial(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Dial*>(widget);
}

Fl_Scrollbar*
_get_fl_scrollbar(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Scrollbar*>(widget);
}

Fl_Value_Slider*
_get_fl_value_slider(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Value_Slider*>(widget);
}

Fl_Slider*
_get_fl_slider(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Slider*>(widget);
}

Fl_Value_Input*
_get_fl_value_input(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Value_Input*>(widget);
}

Fl_Value_Output*
_get_fl_value_output(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Value_Output*>(widget);
}
