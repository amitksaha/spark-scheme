// MzScheme inetrface to the FLTK Help_View widget.
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

#include <FL/Fl_Help_View.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_help_view
{
  static Scheme_Object* fl_help_view(int, Scheme_Object**);
  static Scheme_Object* directory(int, Scheme_Object**);
  static Scheme_Object* filename(int, Scheme_Object**);
  static Scheme_Object* link(int, Scheme_Object**);
  static Scheme_Object* load(int, Scheme_Object**);
  static Scheme_Object* size(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* title(int, Scheme_Object**);
  static Scheme_Object* topline(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
} // namespace spark_fltk_help_view

spark::Status_code
spark_fltk::_add_help_view_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_help_view::fl_help_view, 
		  "fl-help-view", 5),
    new Procedure(spark_fltk_help_view::directory, "help-file-root", 1),
    new Procedure(spark_fltk_help_view::filename, "help-file-name", 1),
    new Procedure(spark_fltk_help_view::link, "link-callback", 2),
    new Procedure(spark_fltk_help_view::load, "load-url", 2),
    new Procedure(spark_fltk_help_view::size, "help-text-length", 1),
    new Procedure(spark_fltk_help_view::textcolor, "help-text-color",1, 2),
    new Procedure(spark_fltk_help_view::textfont, "help-text-font", 1, 2),
    new Procedure(spark_fltk_help_view::textsize, "help-text-size", 1, 2),
    new Procedure(spark_fltk_help_view::topline, "help-topline", 1, 2),
    new Procedure(spark_fltk_help_view::value, "help-text", 1, 2),
    new Procedure(spark_fltk_help_view::title, "help-title", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Help_View* _get_fl_help_view(int argc, 
				       Scheme_Object** argv, 
				       int index);
static const char* _link_cb(Fl_Widget* w, const char* uri);

Scheme_Object* 
spark_fltk_help_view::fl_help_view(int argc, Scheme_Object** argv)
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
  Fl_Help_View* hv = new Fl_Help_View(x, y, w, h);
  if (title.length() > 0)
    hv->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new Widget;
  widget->other_callbacks[CBT_LINK] = scheme_null;
  hv->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(hv, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::directory(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      const char* s = hv->directory();
      _ret_ = scheme_make_utf8_string(s);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::filename(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      const char* s = hv->filename();
      _ret_ = scheme_make_utf8_string(s);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::link(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = 
    dynamic_cast<Fl_Help_View*>(spark_fltk::_get_widget(argc, argv, 0));
  if (hv)
    hv->link(_link_cb);
  Widget* hvw = reinterpret_cast<Widget*>(hv->argument());
  if (hvw)
    {
      hvw->other_callbacks[CBT_LINK] = argv[1];
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::load(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("load-url", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      const char* tt = SCHEME_BYTE_STR_VAL(str);
      hv->load(tt);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      int i = hv->size();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (argc == 1)
	{
	  Fl_Color c = hv->textcolor();
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
	      hv->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (argc == 1)
	{
	  uchar c = hv->textfont();
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
	      hv->textfont(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (argc == 1)
	{
	  uchar c = hv->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      hv->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::title(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      const char* s = hv->title();
      _ret_ = scheme_make_utf8_string(s);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::topline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (argc == 1)
	{
	  int tl = hv->textsize();
	  long i = static_cast<long>(tl);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      hv->topline(i);
	      _ret_ = scheme_true;
	    }
	  else
	    {
	      if (!SCHEME_CHAR_STRINGP(argv[1]))
		scheme_wrong_type("help-topline", "string", 1, argc, argv);
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      const char* tt = SCHEME_BYTE_STR_VAL(str);
	      hv->topline(tt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_view::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_View* hv = _get_fl_help_view(argc, argv, 0);
  if (hv)
    {
      if (argc == 1)
	{
	  const char* s = hv->value();
	  _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("help-text", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  const char* tt = SCHEME_BYTE_STR_VAL(str);
	  hv->value(tt);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Help_View*
_get_fl_help_view(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Help_View*>(widget);
}

const char* 
_link_cb(Fl_Widget* w, const char* uri)
{
  Widget* widget = reinterpret_cast<Widget*>(w->argument());
  if (widget)
    {
      Scheme_Object* cb = widget->other_callbacks[CBT_LINK];
      if (cb == scheme_null)
	return 0;
      const int arg_count = 2;
      Scheme_Object* args[arg_count];
      Scheme_Object* tag = 0;
      Scheme_Object* obj_uri = 0;
      Scheme_Object* obj_ptr = 0;
      MZ_GC_DECL_REG(3);
      MZ_GC_VAR_IN_REG(0, tag);
      MZ_GC_VAR_IN_REG(1, obj_ptr);
      MZ_GC_VAR_IN_REG(2, obj_uri);
      Fltk_tag t = FL_WIDGET_TAG;
      tag = scheme_make_integer(t);
      obj_ptr = scheme_make_cptr(widget, tag);
      args[0] = obj_ptr;
      obj_uri = scheme_make_utf8_string(uri);
      args[1] = obj_uri;
      MZ_GC_REG();	
      Scheme_Object* ret = scheme_apply(cb, arg_count, args);
      MZ_GC_UNREG();
      if (SCHEME_CHAR_STRINGP(ret))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(ret);
	  return SCHEME_BYTE_STR_VAL(str);
	}
    }
  return 0;
}

