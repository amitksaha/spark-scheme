// MzScheme inetrface to the FLTK Chart widget.
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

#include <FL/Fl_Chart.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_chart
{
  static Scheme_Object* fl_chart(int, Scheme_Object**);
  // Fl_Chart
  static Scheme_Object* add(int, Scheme_Object**);
  static Scheme_Object* autosize(int, Scheme_Object**);
  static Scheme_Object* bounds(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* clear(int, Scheme_Object**);
  static Scheme_Object* insert(int, Scheme_Object**);
  static Scheme_Object* maxsize(int, Scheme_Object**);
  static Scheme_Object* replace(int, Scheme_Object**);
  static Scheme_Object* size(int, Scheme_Object**);
} // namespace spark_fltk_chart

spark::Status_code
spark_fltk::_add_chart_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_chart::fl_chart, "fl-chart", 5, 6),
    // Fl_Chart
    new Procedure(spark_fltk_chart::add, "add-chart-value", 2, 4),
    new Procedure(spark_fltk_chart::autosize, "autosize", 1, 2),
    new Procedure(spark_fltk_chart::bounds, "bounds", 1, 3),
    new Procedure(spark_fltk_chart::type, "chart-type", 1, 2),
    new Procedure(spark_fltk_chart::clear, "clear-chart", 1),
    new Procedure(spark_fltk_chart::insert, "insert-chart-value", 4, 6),
    new Procedure(spark_fltk_chart::replace, "replace-chart-value", 4, 6),
    new Procedure(spark_fltk_chart::maxsize, "maxsize", 1, 2),
    new Procedure(spark_fltk_chart::size, "count-chart-values", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_chart_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("FL-BAR-CHART", FL_BAR_CHART),
    Constant("FL-FILLED-CHART", FL_FILLED_CHART),
    Constant("FL-HORBAR-CHART", FL_HORBAR_CHART),
    Constant("FL-LINE-CHART", FL_LINE_CHART),
    Constant("FL-PIE-CHART", FL_PIE_CHART),
    Constant("FL-SPECIALPIE-CHART", FL_SPECIALPIE_CHART),
    Constant("FL-SPIKE-CHART", FL_SPIKE_CHART),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Chart* _get_fl_chart(int argc, Scheme_Object** argv, int index);

Scheme_Object* 
spark_fltk_chart::fl_chart(int argc, Scheme_Object** argv)
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
  Fl_Chart* chart = new Fl_Chart(x, y, w, h);
  if (argc == 6)
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-chart", "symbol", 5, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[5]);
      if (s == "bar")
	chart->type(FL_BAR_CHART);
      else if (s == "filled")
	chart->type(FL_FILLED_CHART);
      else if (s == "horbar")
	chart->type(FL_HORBAR_CHART);
      else if (s == "line")
	chart->type(FL_LINE_CHART);
      else if (s == "pie")
	chart->type(FL_PIE_CHART);
      else if (s == "special-pie")
	chart->type(FL_SPECIALPIE_CHART);
      else if (s == "spike")
	chart->type(FL_SPIKE_CHART);
    }
  if (title.length() > 0)
    chart->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  chart->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    tag = scheme_make_integer(t);
    _ret_ = scheme_make_cptr(chart, tag);
    MZ_GC_REG();
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::add(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      double v = 0.0f;
      if (spark::Utils::double_from_scheme_double(argv[1], v))
	{
	  _ret_ = scheme_true;
	  if (argc == 2)
	    chart->add(v);
	  else
	    {
	      std::string label;
	      if (argv[2] != scheme_null)
		{
		  if (!SCHEME_CHAR_STRINGP(argv[2]))
		    scheme_wrong_type("add-chart-value", "string", 
				      2, argc, argv);
		  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
		  label = SCHEME_BYTE_STR_VAL(str);
		}
	      int i = 0;
	      spark::Utils::int_from_scheme_long(argv[3], i);
	      uchar c = static_cast<uchar>(i);
	      chart->add(v, label.c_str(), c);
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::autosize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      if (argc == 1)
	{
	  uchar c = chart->autosize();
	  _ret_ = c ? scheme_true : scheme_false;
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      chart->autosize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::bounds(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      if (argc == 1)
	{
	  double a, b;
	  chart->bounds(&a, &b);
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {
	    Scheme_Object* obj_a = NULL;
	    Scheme_Object* obj_b = NULL;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_a);
	    MZ_GC_VAR_IN_REG(1, obj_b);
	    MZ_GC_REG();
	    obj_a = scheme_make_double(a);
	    obj_b = scheme_make_double(b);
	    elems[0] = obj_a;
	    elems[1] = obj_b;
	    MZ_GC_UNREG();
	  }
	  _ret_ = scheme_build_list(2, elems);      
	  delete[] elems;
	}
      else
	{
	  double a = 0.0f;
	  double b = 0.0f;
	  if (spark::Utils::double_from_scheme_double(argv[1], a)
	      && spark::Utils::double_from_scheme_double(argv[2], b))
	    {
	      chart->bounds(a, b);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      chart->clear();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::insert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[1], pos);
      double v = 0.0f;
      if (spark::Utils::double_from_scheme_double(argv[2], v))
	{
	  _ret_ = scheme_true;
	  if (argc == 3)
	    chart->insert(pos, v);
	  else
	    {
	      std::string label;
	      if (argv[3] != scheme_null)
		{
		  if (!SCHEME_CHAR_STRINGP(argv[3]))
		    scheme_wrong_type("add-chart-value", "string", 
				      3, argc, argv);
		  Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
		  label = SCHEME_BYTE_STR_VAL(str);
		}
	      int i = 0;
	      spark::Utils::int_from_scheme_long(argv[4], i);
	      uchar c = static_cast<uchar>(i);
	      chart->insert(pos, v, label.c_str(), c);
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::maxsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      if (argc == 1)
	{
	  int i = chart->maxsize();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      chart->maxsize(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::replace(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[1], pos);
      double v = 0.0f;
      if (spark::Utils::double_from_scheme_double(argv[2], v))
	{
	  _ret_ = scheme_true;
	  if (argc == 3)
	    chart->replace(pos, v);
	  else
	    {
	      std::string label;
	      if (argv[3] != scheme_null)
		{
		  if (!SCHEME_CHAR_STRINGP(argv[3]))
		    scheme_wrong_type("add-chart-value", "string", 
				      3, argc, argv);
		  Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
		  label = SCHEME_BYTE_STR_VAL(str);
		}
	      int i = 0;
	      spark::Utils::int_from_scheme_long(argv[4], i);
	      uchar c = static_cast<uchar>(i);
	      chart->replace(pos, v, label.c_str(), c);
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      int i = chart->size();
      _ret_ = scheme_make_integer(i);
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_chart::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Chart* chart = _get_fl_chart(argc, argv, 0);
  if (chart)
    {
      if (argc == 1)
	{
	  uchar i = chart->type();
	  _ret_ = scheme_make_integer(static_cast<int>(i));
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      chart->type(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Chart*
_get_fl_chart(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Chart*>(widget);
}
