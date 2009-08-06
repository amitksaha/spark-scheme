// MzScheme inetrface to the FLTK Window.
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

#include <FL/Fl_Window.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Single_Window.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_window
{
  static Scheme_Object* fl_window(int, Scheme_Object**);
  static Scheme_Object* border(int, Scheme_Object**);
  static Scheme_Object* clear_border(int, Scheme_Object**);
  static Scheme_Object* current(int, Scheme_Object**);
  static Scheme_Object* cursor(int, Scheme_Object**);
  static Scheme_Object* free_position(int, Scheme_Object**);
  static Scheme_Object* fullscreen(int, Scheme_Object**);
  static Scheme_Object* fullscreen_off(int, Scheme_Object**);
  static Scheme_Object* hotspot(int, Scheme_Object**);
  static Scheme_Object* iconize(int, Scheme_Object**);
  static Scheme_Object* iconlabel(int, Scheme_Object**);
  static Scheme_Object* label(int, Scheme_Object**);
  static Scheme_Object* make_current(int, Scheme_Object**);
  static Scheme_Object* modal(int, Scheme_Object**);
  static Scheme_Object* non_modal(int, Scheme_Object**);
  static Scheme_Object* resize(int, Scheme_Object**);
  static Scheme_Object* set_modal(int, Scheme_Object**);
  static Scheme_Object* set_non_modal(int, Scheme_Object**);
  static Scheme_Object* show(int, Scheme_Object**);
  static Scheme_Object* hide(int, Scheme_Object**);
  static Scheme_Object* shown(int, Scheme_Object**);
  static Scheme_Object* size_range(int, Scheme_Object**);
  static Scheme_Object* xclass(int, Scheme_Object**);
} // namespace spark_fltk_window

spark::Status_code
spark_fltk::_add_window_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_window::fl_window, "fl-window", 5, 6),
    new Procedure(spark_fltk_window::border, "border", 1, 2),
    new Procedure(spark_fltk_window::clear_border, "clear-border", 1),
    new Procedure(spark_fltk_window::current, "current-window", 0),
    new Procedure(spark_fltk_window::cursor, "change-cursor", 2, 4),
    new Procedure(spark_fltk_window::free_position, "free-position", 1),
    new Procedure(spark_fltk_window::fullscreen, "fullscreen",1),
    new Procedure(spark_fltk_window::fullscreen_off, "fullscreen-off", 5),
    new Procedure(spark_fltk_window::hotspot, "hotspot", 3),
    new Procedure(spark_fltk_window::iconize, "iconize", 1),
    new Procedure(spark_fltk_window::iconlabel, "icon-label", 1, 2),
    new Procedure(spark_fltk_window::label, "title-bar-label", 1, 2),
    new Procedure(spark_fltk_window::make_current, "make-current", 1),
    new Procedure(spark_fltk_window::modal, "modal", 1),
    new Procedure(spark_fltk_window::non_modal, "non-modal", 1),
    new Procedure(spark_fltk_window::resize, "resize-window", 5),
    new Procedure(spark_fltk_window::set_modal, "set-modal", 1),
    new Procedure(spark_fltk_window::set_non_modal, "set-non-modal", 1),
    new Procedure(spark_fltk_window::show, "show-window", 2),
    new Procedure(spark_fltk_window::hide, "hide-window", 1),
    new Procedure(spark_fltk_window::shown, "shown", 1),
    new Procedure(spark_fltk_window::size_range, "size-range", 3, 8),
    new Procedure(spark_fltk_window::xclass, "xclass", 1, 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Window* _get_fl_window(int argc, 
				 Scheme_Object** argv, 
				 int index);

// Creates and returns a new window.
// Takes 6 arguments:
// 1. x position
// 2. y position
// 3. width. if null window manager will decide the width.
// 4. height. if null window manager will decide the height.
// 5. title. can be null.
// 6. type. should be either 'double or 'single
// Returns a Fl_Window handle on success, null on failure.
Scheme_Object* 
spark_fltk_window::fl_window(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int x = 0;
  if (!spark::Utils::int_from_scheme_long(argv[0], x))
    scheme_wrong_type("fl-window", "int", 0, argc, argv);
  int y = 0;
  if (!spark::Utils::int_from_scheme_long(argv[1], y))
    scheme_wrong_type("fl-window", "int", 1, argc, argv);
  int w = 0;
  if (!spark::Utils::int_from_scheme_long(argv[2], w))
    scheme_wrong_type("fl-window", "int", 2, argc, argv);
  int h = 0;
  if (!spark::Utils::int_from_scheme_long(argv[3], h))
    scheme_wrong_type("fl-window", "int", 3, argc, argv);
  std::string title;
  if (SCHEME_CHAR_STRINGP(argv[4]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
      title = SCHEME_BYTE_STR_VAL(str);
    }
  Fl_Window* fl_window = 0;
  if (argc == 5)
    {
      fl_window = new Fl_Window(x, y, w, h, 0);
    }
  else
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-window", "symbol", 5, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[5]);
      if (s == "double")
	{
	  fl_window = new Fl_Double_Window(x, y, w, h, 0);
	}
      else if (s == "single")
	{
	  fl_window = new Fl_Single_Window(x, y, w, h, 0);
	}
      else
	{
	  DEFAULT_RET_FINISH;
	}
    }
  if (title.length() > 0)
    fl_window->copy_label(title.c_str());
  if (fl_window)
    {
      Fltk_tag t = FL_WIDGET_TAG;
      spark_fltk::Widget* widget = new spark_fltk::Widget;
      fl_window->argument(reinterpret_cast<long>(widget));
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	MZ_GC_UNREG();
	_ret_ = scheme_make_cptr(fl_window, tag);
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::show(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      if (!SCHEME_LISTP(argv[1]))
	scheme_wrong_type("show-window", "list", 1, argc, argv);
      int ac = scheme_list_length(argv[1]);
      char** av;
      if (ac == 0)
	{
	  av = new char*[1];
	  av[0] = new char[6];
	  strcpy(av[0], "spark");
	  ac = 1;
	}
      else
	{
	  Scheme_Object* lst = argv[1];
	  av = new char*[ac];
	  int i = 0;
	  while (lst)
	    {
	      if (lst == scheme_null)
		break;
	      Scheme_Object* obj = SCHEME_CAR(lst);
	      if (obj != scheme_null)
		{
		  if (!SCHEME_CHAR_STRINGP(obj))
		    scheme_wrong_type("show-window", "string", 1, argc, argv);
		  Scheme_Object* str = scheme_char_string_to_byte_string(obj);
		  std::string s = SCHEME_BYTE_STR_VAL(str);
		  av[i] = new char[s.length() + 1];
		  strcpy(av[i], s.c_str());
		}
	      else
		{
		  av[i] = new char[2];
		  strcpy(av[i], "");
		}
	      ++i;
	      lst = SCHEME_CDR(lst);
	    }
	}
      window->show(ac, av);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::hide(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->hide();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::border(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      if (argc == 1)
	{
	  int b = window->border();
	  _ret_ = scheme_make_integer(b);
	}
      else
	{
	  int b = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], b))
	    {
	      window->border(b);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::clear_border(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->clear_border();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::set_modal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->set_modal();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::modal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      _ret_ = window->modal() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::set_non_modal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->set_non_modal();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::non_modal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      _ret_ = window->non_modal() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      if (argc == 1)
	{
	  const char* s = window->label();
	  if (s)
	    _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string s = SCHEME_BYTE_STR_VAL(str);
	      window->label(s.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::iconlabel(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      if (argc == 1)
	{
	  const char* s = window->iconlabel();
	  if (s)
	    _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string s = SCHEME_BYTE_STR_VAL(str);
	      window->iconlabel(s.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::xclass(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      if (argc == 1)
	{
	  const char* s = window->xclass();
	  if (s)
	    _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string s = SCHEME_BYTE_STR_VAL(str);
	      window->xclass(s.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::make_current(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->make_current();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::current(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = Fl_Window::current();
  if (window)
    {
      Fltk_tag t = FL_WIDGET_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	MZ_GC_UNREG();
	_ret_ = scheme_make_cptr(window, tag);
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::cursor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      Fl_Cursor cursor = static_cast<Fl_Cursor>(i);
      if (argc == 2)
	window->cursor(cursor);
      else
	{
	  i = 0;
	  spark::Utils::int_from_scheme_long(argv[2], i);
	  Fl_Color color01 = static_cast<Fl_Color>(i);
	  i = 0;
	  spark::Utils::int_from_scheme_long(argv[3], i);
	  Fl_Color color02 = static_cast<Fl_Color>(i);
	  window->cursor(cursor, color01, color02);
	}
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::free_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->free_position();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::hotspot(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      int offscreen = argv[3] == scheme_true ? 1 : 0;
      spark::Utils::int_from_scheme_long(argv[3], offscreen);
      window->hotspot(x, y, offscreen);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::fullscreen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->fullscreen();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::fullscreen_off(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[3], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[4], h);
      window->fullscreen_off(x, y, w, h);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::iconize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      window->iconize();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::shown(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    _ret_ = window->shown() ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::resize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[3], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[4], h);
      window->resize(x, y, w, h);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_window::size_range(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Window* window = _get_fl_window(argc, argv, 0);
  if (window)
    {
      int minw = 0;
      spark::Utils::int_from_scheme_long(argv[1], minw);
      int minh = 0;
      spark::Utils::int_from_scheme_long(argv[2], minh);
      if (argc == 3)
	window->size_range(minw, minh);
      else
	{
	  int maxw = 0;
	  spark::Utils::int_from_scheme_long(argv[3], maxw);
	  int maxh = 0;
	  spark::Utils::int_from_scheme_long(argv[3], maxh);
	  int dw = 0;
	  spark::Utils::int_from_scheme_long(argv[4], dw);
	  int dh = 0;
	  spark::Utils::int_from_scheme_long(argv[5], dh);
	  int aspect = (argv[6] == scheme_true) ? 1 : 0;
	  window->size_range(minw, minh, maxw, maxh, dw, dh, aspect);
	}
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Fl_Window*
_get_fl_window(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Window*>(widget);
}
