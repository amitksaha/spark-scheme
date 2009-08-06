// MzScheme inetrface to the FLTK Pack widget.
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

#include <FL/Fl_Pack.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_pack
{
  static Scheme_Object* fl_pack(int, Scheme_Object**);
  static Scheme_Object* box_type(int, Scheme_Object**);
  static Scheme_Object* spacing(int, Scheme_Object**);
  static Scheme_Object* resizable(int, Scheme_Object**);
} // namespace spark_fltk_pack

spark::Status_code
spark_fltk::_add_pack_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_pack::fl_pack,
		  "fl-pack", 5),
    new Procedure(spark_fltk_pack::box_type, "box-type", 1, 2),
    new Procedure(spark_fltk_pack::resizable, "pack-resizable", 1, 2),
    new Procedure(spark_fltk_pack::spacing, "spacing", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Pack* _get_fl_pack(int argc, 
			     Scheme_Object** argv, 
			     int index);

Scheme_Object* 
spark_fltk_pack::fl_pack(int argc, Scheme_Object** argv)
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
  Fl_Pack* pack = new Fl_Pack(x, y, w, h);
  if (title.length() > 0)
    pack->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  pack->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(pack, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_pack::box_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Pack* pack = _get_fl_pack(argc, argv, 0);
  if (pack)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = pack->box();
	  long i = static_cast<long>(bt);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
	      pack->box(bt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_pack::resizable(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Pack* pack = _get_fl_pack(argc, argv, 0);
  if (pack)
    {
      if (argc == 1)
	{
	  Fl_Widget* w = pack->resizable();
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
	      pack->resizable(widget);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}


Scheme_Object*
spark_fltk_pack::spacing(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Pack* pack = _get_fl_pack(argc, argv, 0);
  if (pack)
    {
      if (argc == 1)
	{
	  int i = pack->spacing();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      pack->spacing(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Pack*
_get_fl_pack(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Pack*>(widget);
}
