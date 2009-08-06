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

#include <FL/Fl_Group.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_group
{
  static Scheme_Object* fl_group(int, Scheme_Object**);
  static Scheme_Object* end(int, Scheme_Object**);
  static Scheme_Object* add(int, Scheme_Object**);
  static Scheme_Object* add_resizable(int, Scheme_Object**);
  static Scheme_Object* array(int, Scheme_Object**);
  static Scheme_Object* child(int, Scheme_Object**);
  static Scheme_Object* find(int, Scheme_Object**);
  static Scheme_Object* resizable(int, Scheme_Object**);
  static Scheme_Object* begin(int, Scheme_Object**);
  static Scheme_Object* clear(int, Scheme_Object**);
  static Scheme_Object* init_sizes(int, Scheme_Object**);
  static Scheme_Object* insert(int, Scheme_Object**);
  static Scheme_Object* remove(int, Scheme_Object**);
  static Scheme_Object* dispose(int, Scheme_Object**);
} // namespace spark_fltk_group

spark::Status_code
spark_fltk::_add_group_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_group::fl_group, 
		  "fl-group", 5),
    new Procedure(spark_fltk_group::end, "end", 1),
    new Procedure(spark_fltk_group::add, "add", 2),
    new Procedure(spark_fltk_group::add_resizable, "add-resizable", 2),
    new Procedure(spark_fltk_group::array, "children", 1),
    new Procedure(spark_fltk_group::child, "child-at", 2),
    new Procedure(spark_fltk_group::find, "index-of-child", 2),
    new Procedure(spark_fltk_group::resizable, "resizable", 1, 2),
    new Procedure(spark_fltk_group::begin, "begin", 1),
    new Procedure(spark_fltk_group::clear, "delete-children", 1),
    new Procedure(spark_fltk_group::init_sizes, "init-sizes", 1),
    new Procedure(spark_fltk_group::insert, "insert-at", 2),
    new Procedure(spark_fltk_group::remove, "remove-child", 2),
    new Procedure(spark_fltk_group::dispose, "group-dispose", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Group* _get_fl_group(int argc, 
			       Scheme_Object** argv, 
			       int index);

Scheme_Object* 
spark_fltk_group::fl_group(int argc, Scheme_Object** argv)
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
  Fl_Group* group = new Fl_Group(x, y, w, h);
  if (title.length() > 0)
    group->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  group->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(group, tag);
  }

  DEFAULT_RET_FINISH;
}

// Any new widgets added to the widget tree will be added to the parent of the group.
Scheme_Object* 
spark_fltk_group::end(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      group->end();
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::add(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget* widget = spark_fltk::_get_widget(argc,
						  argv,
						  1);
      if (widget)
	{
	  group->add(widget);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::add_resizable(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget* widget = spark_fltk::_get_widget(argc,
						  argv,
						  1);
      if (widget)
	{
	  group->add_resizable(*widget);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}


Scheme_Object*
spark_fltk_group::array(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget** widgets = const_cast<Fl_Widget**>(group->array());
      if (!widgets)
	{
	  DEFAULT_RET_FINISH;
	}
      int i = 0;
      Fl_Widget* w = widgets[i];
      std::vector<Fl_Widget*> w_vec;
      while (w)
	{
	  w_vec.push_back(w);
	  ++i;
	  w = widgets[i];
	}
      size_t sz = w_vec.size();
      Scheme_Object** elems = new Scheme_Object*[sz];
      for (size_t i=0; i<sz; ++i)
	{
	  Scheme_Object* obj = NULL;
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(2);
	  MZ_GC_VAR_IN_REG(0, obj);
	  MZ_GC_VAR_IN_REG(1, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_WIDGET_TAG);
	  obj = scheme_make_cptr(w_vec[i], tag);
	  elems[i] = obj;
	  MZ_GC_UNREG();
	}
      _ret_ = scheme_build_list(sz, elems);      
      delete[] elems;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::child(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  if (i < 0 || i >= group->children())
	    {
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Widget* w = group->child(i);
	  if (w)
	    {
	      Scheme_Object* tag = 0;
	      MZ_GC_DECL_REG(1);
	      MZ_GC_VAR_IN_REG(0, tag);
	      MZ_GC_REG();
	      tag = scheme_make_integer(FL_WIDGET_TAG);
	      _ret_ = scheme_make_cptr(w, tag);
	      MZ_GC_UNREG();
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::find(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget* widget = spark_fltk::_get_widget(argc,
						  argv,
						  1);
      if (widget)
	{
	  int i = group->find(*widget);
	  _ret_ = scheme_make_integer(i);
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::resizable(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      if (argc == 1)
	{
	  Fl_Widget* w = group->resizable();
	  if (w)
	    {
	      Scheme_Object* tag = 0;
	      MZ_GC_DECL_REG(1);
	      MZ_GC_VAR_IN_REG(0, tag);
	      MZ_GC_REG();
	      tag = scheme_make_integer(FL_WIDGET_TAG);
	      _ret_ = scheme_make_cptr(w, tag);
	      MZ_GC_UNREG();
	    }
	}
      else
	{
	  Fl_Widget* widget = spark_fltk::_get_widget(argc,
						      argv,
						      1);
	  if (widget)
	    {
	      group->resizable(widget);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::begin(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      group->begin();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      group->clear();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::init_sizes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      group->init_sizes();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::insert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget* widget = spark_fltk::_get_widget(argc,
						  argv,
						  1);
      if (widget)
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[2], i))
	    {
	      group->insert(*widget, i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::remove(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Fl_Widget* widget = spark_fltk::_get_widget(argc,
						  argv,
						  1);
      if (widget)
	{
	  group->remove(*widget);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_group::dispose(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Group* group = _get_fl_group(argc, argv, 0);
  if (group)
    {
      Widget* w = reinterpret_cast<Widget*>(group->argument());
      delete w;
      delete group;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Fl_Group*
_get_fl_group(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Group*>(widget);
}
