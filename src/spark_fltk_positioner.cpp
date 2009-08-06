// MzScheme inetrface to the FLTK Positioner widget.
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

#include <FL/Fl_Positioner.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_positioner
{
  static Scheme_Object* fl_positioner(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* xbounds(int, Scheme_Object**);
  static Scheme_Object* xstep(int, Scheme_Object**);
  static Scheme_Object* xvalue(int, Scheme_Object**);
  static Scheme_Object* ybounds(int, Scheme_Object**);
  static Scheme_Object* ystep(int, Scheme_Object**);
  static Scheme_Object* yvalue(int, Scheme_Object**);
} // namespace spark_fltk_positioner

spark::Status_code
spark_fltk::_add_positioner_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_positioner::fl_positioner, 
		  "fl-positioner", 5),
    new Procedure(spark_fltk_positioner::value, "positioner-value", 3),
    new Procedure(spark_fltk_positioner::xbounds, "xbounds", 1, 3),
    new Procedure(spark_fltk_positioner::xstep, "xstep", 2),
    new Procedure(spark_fltk_positioner::xvalue, "xvalue", 1, 2),
    new Procedure(spark_fltk_positioner::ybounds, "ybounds", 1, 3),
    new Procedure(spark_fltk_positioner::ystep, "ystep", 2),
    new Procedure(spark_fltk_positioner::yvalue, "yvalue", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Positioner* _get_fl_positioner(int argc, 
					Scheme_Object** argv, 
					int index);

Scheme_Object* 
spark_fltk_positioner::fl_positioner(int argc, Scheme_Object** argv)
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
  Fl_Positioner* p = new Fl_Positioner(x, y, w, h);
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
spark_fltk_positioner::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      double x = 0.0f;
      double y = 0.0f;      
      spark::Utils::double_from_scheme_double(argv[1], x);
      spark::Utils::double_from_scheme_double(argv[2], y);
      p->value(x, y);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::xbounds(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  double x = 0.0f;
	  double y = 0.0f;
	  x = p->xmaximum();
	  y = p->xminimum();
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {	
	    Scheme_Object* obj_x = 0;
	    Scheme_Object* obj_y = 0;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_x);
	    MZ_GC_VAR_IN_REG(1, obj_y);
	    MZ_GC_REG();
	    obj_x = scheme_make_double(x);
	    obj_y = scheme_make_double(y);
	    elems[0] = obj_x;
	    elems[1] = obj_y;
	    _ret_ = scheme_build_list(2, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  }      
	}
      else
	{
	  double x = 0.0f;
	  double y = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  spark::Utils::double_from_scheme_double(argv[2], y);
	  p->xbounds(x, y);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::xstep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      double x = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], x);
      p->xstep(x);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::xvalue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  double x = 0.0f;
	  x = p->xvalue();
	  _ret_ = scheme_make_double(x);
	}
      else
	{
	  double x = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  p->xvalue(x);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::ybounds(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  double x = 0.0f;
	  double y = 0.0f;
	  x = p->ymaximum();
	  y = p->yminimum();
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {	
	    Scheme_Object* obj_x = 0;
	    Scheme_Object* obj_y = 0;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_x);
	    MZ_GC_VAR_IN_REG(1, obj_y);
	    MZ_GC_REG();
	    obj_x = scheme_make_double(x);
	    obj_y = scheme_make_double(y);
	    elems[0] = obj_x;
	    elems[1] = obj_y;
	    _ret_ = scheme_build_list(2, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  }      
	}
      else
	{
	  double x = 0.0f;
	  double y = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  spark::Utils::double_from_scheme_double(argv[2], y);
	  p->ybounds(x, y);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::ystep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      double x = 0.0f;
      spark::Utils::double_from_scheme_double(argv[1], x);
      p->ystep(x);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_positioner::yvalue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Positioner* p = _get_fl_positioner(argc, argv, 0);
  if (p)
    {
      if (argc == 1)
	{
	  double x = 0.0f;
	  x = p->yvalue();
	  _ret_ = scheme_make_double(x);
	}
      else
	{
	  double x = 0.0f;
	  spark::Utils::double_from_scheme_double(argv[1], x);
	  p->yvalue(x);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Positioner*
_get_fl_positioner(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Positioner*>(widget);
}
