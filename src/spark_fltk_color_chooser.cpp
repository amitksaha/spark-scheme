// MzScheme inetrface to the FLTK Color Chooser widget.
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

#include <FL/Fl_Color_Chooser.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_color_chooser
{
  static Scheme_Object* fl_color_chooser(int, Scheme_Object**);
  static Scheme_Object* blue(int, Scheme_Object**);
  static Scheme_Object* green(int, Scheme_Object**);
  static Scheme_Object* red(int, Scheme_Object**);
  static Scheme_Object* hsb2rgb(int, Scheme_Object**);
  static Scheme_Object* rgb2hsb(int, Scheme_Object**);
  static Scheme_Object* hsb(int, Scheme_Object**);
  static Scheme_Object* rgb(int, Scheme_Object**);
  static Scheme_Object* hue(int, Scheme_Object**);
  static Scheme_Object* saturation(int, Scheme_Object**);
  static Scheme_Object* brightness(int, Scheme_Object**);
  // global
  static Scheme_Object* show(int, Scheme_Object**);
  static Scheme_Object* get_color(int, Scheme_Object**);
  static Scheme_Object* set_color(int, Scheme_Object**);
} // namespace spark_fltk_color_chooser

spark::Status_code
spark_fltk::_add_color_chooser_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_color_chooser::fl_color_chooser, 
		  "fl-color-chooser", 5),
    new Procedure(spark_fltk_color_chooser::blue, "blue", 1),
    new Procedure(spark_fltk_color_chooser::green, "green", 1),
    new Procedure(spark_fltk_color_chooser::red, "red", 1),
    new Procedure(spark_fltk_color_chooser::hsb2rgb, "hsb->rgb", 3),
    new Procedure(spark_fltk_color_chooser::rgb2hsb, "rgb->hsb", 3),
    new Procedure(spark_fltk_color_chooser::rgb, "rgb", 4),
    new Procedure(spark_fltk_color_chooser::hsb, "hsb", 4),
    new Procedure(spark_fltk_color_chooser::hue, "hue", 1),
    new Procedure(spark_fltk_color_chooser::saturation, "saturation", 1),
    new Procedure(spark_fltk_color_chooser::brightness, "brightness", 1),
    new Procedure(spark_fltk_color_chooser::show, "show-color-chooser", 4),
    new Procedure(spark_fltk_color_chooser::get_color, "get-color", 1),
    new Procedure(spark_fltk_color_chooser::set_color, "set-color", 4),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Color_Chooser* _get_fl_color_chooser(int argc, 
					       Scheme_Object** argv, 
					       int index);

Scheme_Object* 
spark_fltk_color_chooser::fl_color_chooser(int argc, Scheme_Object** argv)
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
  Fl_Color_Chooser* cc = new Fl_Color_Chooser(x, y, w, h);
  if (title.length() > 0)
    cc->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  cc->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(cc, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::blue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->b();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::green(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->g();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::red(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->r();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::hsb2rgb(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double h = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], h);
  double s = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], s);
  double br = 0.0f;
  spark::Utils::double_from_scheme_double(argv[2], br);
  double r, g, b;
  Fl_Color_Chooser::hsv2rgb(h, s, br, r, g, b);
  Scheme_Object** elems = new Scheme_Object*[3];
  {
    Scheme_Object* r_obj = NULL;
    Scheme_Object* g_obj = NULL;
    Scheme_Object* b_obj = NULL;
    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, r_obj);
    MZ_GC_VAR_IN_REG(1, g_obj);
    MZ_GC_VAR_IN_REG(2, b_obj);
    MZ_GC_REG();
    r_obj = scheme_make_double(r);
    g_obj = scheme_make_double(g);
    b_obj = scheme_make_double(b);
    elems[0] = r_obj;
    elems[1] = g_obj;
    elems[2] = b_obj;
    MZ_GC_UNREG();
  }
  _ret_ = scheme_build_list(3, elems);      
  delete[] elems;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::rgb2hsb(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double r = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], r);
  double g = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], g);
  double b = 0.0f;
  spark::Utils::double_from_scheme_double(argv[2], b);
  double h, s, br;
  Fl_Color_Chooser::rgb2hsv(r, g, b, h, s, br);
  Scheme_Object** elems = new Scheme_Object*[3];
  {
    Scheme_Object* h_obj = NULL;
    Scheme_Object* s_obj = NULL;
    Scheme_Object* b_obj = NULL;
    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, h_obj);
    MZ_GC_VAR_IN_REG(1, s_obj);
    MZ_GC_VAR_IN_REG(2, b_obj);
    MZ_GC_REG();
    h_obj = scheme_make_double(h);
    s_obj = scheme_make_double(s);
    b_obj = scheme_make_double(br);
    elems[0] = h_obj;
    elems[1] = s_obj;
    elems[2] = b_obj;
    MZ_GC_UNREG();
  }
  _ret_ = scheme_build_list(3, elems);      
  delete[] elems;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::rgb(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double r = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], r);
      double g = 0.0f;
      spark::Utils::double_from_scheme_double(argv[2], g);
      double b = 0.0f;
      spark::Utils::double_from_scheme_double(argv[3], b);
      cc->rgb(r, g, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::hsb(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double h = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], h);
      double s = 0.0f;
      spark::Utils::double_from_scheme_double(argv[2], s);
      double b = 0.0f;
      spark::Utils::double_from_scheme_double(argv[3], b);
      cc->hsv(h, s, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::hue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->hue();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::saturation(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->saturation();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::brightness(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Color_Chooser* cc = _get_fl_color_chooser(argc, argv, 0);
  if (cc)
    {
      double c = cc->value();
      _ret_ = scheme_make_double(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::show(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string title;
  if (argv[0] != scheme_null)
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      title = SCHEME_BYTE_STR_VAL(str);
    }
  int i = 0;
  spark::Utils::int_from_scheme_long(argv[1], i);
  uchar r = static_cast<uchar>(i);    
  i = 0;
  spark::Utils::int_from_scheme_long(argv[2], i);
  uchar g = static_cast<uchar>(i);    
  i = 0;
  spark::Utils::int_from_scheme_long(argv[3], i);
  uchar b = static_cast<uchar>(i);    
  ::fl_color_chooser(title.c_str(), r, g, b);
  Scheme_Object** elems = new Scheme_Object*[3];
  {
    Scheme_Object* r_obj = NULL;
    Scheme_Object* g_obj = NULL;
    Scheme_Object* b_obj = NULL;
    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, r_obj);
    MZ_GC_VAR_IN_REG(1, g_obj);
    MZ_GC_VAR_IN_REG(2, b_obj);
    MZ_GC_REG();
    r_obj = scheme_make_integer(r);
    g_obj = scheme_make_integer(g);
    b_obj = scheme_make_integer(b);
    elems[0] = r_obj;
    elems[1] = g_obj;
    elems[2] = b_obj;
    MZ_GC_UNREG();
  }
  _ret_ = scheme_build_list(3, elems);      
  delete[] elems;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::get_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  Fl_Color c = static_cast<Fl_Color>(i);
  uchar r = 0;
  uchar g = 0;
  uchar b = 0;
  Fl::get_color(c, r, g, b);
  Scheme_Object** elems = new Scheme_Object*[3];
  {
    Scheme_Object* r_obj = NULL;
    Scheme_Object* g_obj = NULL;
    Scheme_Object* b_obj = NULL;
    MZ_GC_DECL_REG(3);
    MZ_GC_VAR_IN_REG(0, r_obj);
    MZ_GC_VAR_IN_REG(1, g_obj);
    MZ_GC_VAR_IN_REG(2, b_obj);
    MZ_GC_REG();
    r_obj = scheme_make_integer(r);
    g_obj = scheme_make_integer(g);
    b_obj = scheme_make_integer(b);
    elems[0] = r_obj;
    elems[1] = g_obj;
    elems[2] = b_obj;
    MZ_GC_UNREG();
  }
  _ret_ = scheme_build_list(3, elems);      
  delete[] elems;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_color_chooser::set_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  Fl_Color c = static_cast<Fl_Color>(i);
  spark::Utils::int_from_scheme_long(argv[1], i);
  uchar r = static_cast<uchar>(i);
  spark::Utils::int_from_scheme_long(argv[2], i);
  uchar g = static_cast<uchar>(i);
  spark::Utils::int_from_scheme_long(argv[3], i);
  uchar b = static_cast<uchar>(i);
  Fl::set_color(c, r, g, b);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Fl_Color_Chooser*
_get_fl_color_chooser(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Color_Chooser*>(widget);
}
