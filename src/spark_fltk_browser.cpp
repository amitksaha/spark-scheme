//  MzScheme inetrface to the FLTK Browser widget.
//  Copyright (C) 2007  Vijay Mathew Pandyalakal
 
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 3 of the License, or
//  (at your option) any later version.
  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
  
//  You should have received a copy of the GNU General Public License along
//  with this program; If not, see <http://www.gnu.org/licenses/>.
  
//  Please contact Vijay Mathew Pandyalakal, Thekkekara House, Thekkemala P.O
//  Kozhenchery, Kerala 689654 India (Electronic mail: vijay.the.schemer@gmail.com)
//  if you need additional information or have any questions.

#include <FL/Fl_Browser.H>
#include <FL/Fl_File_Browser.H>
#include <FL/Fl_Hold_Browser.H>
#include <FL/Fl_Multi_Browser.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Check_Browser.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_browser
{
  static Scheme_Object* fl_browser(int, Scheme_Object**);
  static Scheme_Object* fl_check_browser(int, Scheme_Object**);
  // Fl_Browser_
  static Scheme_Object* has_scrollbar(int, Scheme_Object**);
  static Scheme_Object* hposition(int, Scheme_Object**);
  static Scheme_Object* vscroll_position(int, Scheme_Object**);
  static Scheme_Object* scrollbar_left(int, Scheme_Object**);
  static Scheme_Object* scrollbar_right(int, Scheme_Object**);
  static Scheme_Object* scrollbar_width(int, Scheme_Object**);
  static Scheme_Object* select(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  // Fl_Browser
  static Scheme_Object* add_line(int, Scheme_Object**);
  static Scheme_Object* bottomline(int, Scheme_Object**);
  static Scheme_Object* clear(int, Scheme_Object**);
  static Scheme_Object* column_char(int, Scheme_Object**);
  static Scheme_Object* column_widths(int, Scheme_Object**);
  static Scheme_Object* data(int, Scheme_Object**);
  static Scheme_Object* format_char(int, Scheme_Object**);
  static Scheme_Object* hide_line(int, Scheme_Object**);
  static Scheme_Object* load(int, Scheme_Object**);
  static Scheme_Object* middleline(int, Scheme_Object**);
  static Scheme_Object* move(int, Scheme_Object**);
  static Scheme_Object* remove(int, Scheme_Object**);
  static Scheme_Object* show_line(int, Scheme_Object**);
  static Scheme_Object* count(int, Scheme_Object**);
  static Scheme_Object* swap(int, Scheme_Object**);
  static Scheme_Object* text(int, Scheme_Object**);
  static Scheme_Object* topline(int, Scheme_Object**);
  static Scheme_Object* line_visible(int, Scheme_Object**);
  static Scheme_Object* iconsize(int, Scheme_Object**);
  static Scheme_Object* filter(int, Scheme_Object**);
  static Scheme_Object* filetype(int, Scheme_Object**);
  static Scheme_Object* load_dir(int, Scheme_Object**);
  static Scheme_Object* deselect(int, Scheme_Object**);
  static Scheme_Object* select_item(int, Scheme_Object**);
  static Scheme_Object* selected(int, Scheme_Object**);
  static Scheme_Object* current_selection(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* make_visible(int, Scheme_Object**);
  // Fl_Check_Browser
  static Scheme_Object* cb_add_line(int, Scheme_Object**);
  static Scheme_Object* cb_check_all(int, Scheme_Object**);
  static Scheme_Object* cb_check_none(int, Scheme_Object**);
  static Scheme_Object* cb_checked(int, Scheme_Object**);
  static Scheme_Object* cb_clear(int, Scheme_Object**);
  static Scheme_Object* cb_nchecked(int, Scheme_Object**);
  static Scheme_Object* cb_nitems(int, Scheme_Object**);
  static Scheme_Object* cb_remove(int, Scheme_Object**);
  static Scheme_Object* cb_text(int, Scheme_Object**);
  static Scheme_Object* cb_value(int, Scheme_Object**);
} // namespace spark_fltk_browser


spark::Status_code
spark_fltk::_add_browser_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_browser::fl_browser, "fl-browser", 5, 6),
    new Procedure(spark_fltk_browser::fl_check_browser, "fl-check-browser", 5),
    // Fl_Browser_
    new Procedure(spark_fltk_browser::has_scrollbar, "has-scrollbar", 2),
    new Procedure(spark_fltk_browser::hposition, "hposition", 1, 2),
    new Procedure(spark_fltk_browser::vscroll_position, "vscroll-position", 1, 2),
    new Procedure(spark_fltk_browser::scrollbar_left, "scrollbar-left", 1),
    new Procedure(spark_fltk_browser::scrollbar_right, "scrollbar-right", 1),
    new Procedure(spark_fltk_browser::scrollbar_width, "scrollbar-width", 1, 2),
    new Procedure(spark_fltk_browser::select, "select", 4),
    new Procedure(spark_fltk_browser::textcolor, "text-color", 1, 2),
    new Procedure(spark_fltk_browser::textfont, "text-font", 1, 2),
    new Procedure(spark_fltk_browser::textsize, "text-size", 1, 2),
    // Fl_Browser
    new Procedure(spark_fltk_browser::add_line, "add-line", 2),
    new Procedure(spark_fltk_browser::bottomline, "bottomline", 2),
    new Procedure(spark_fltk_browser::clear, "clear", 1),
    new Procedure(spark_fltk_browser::column_char, "column-char", 1, 2),
    new Procedure(spark_fltk_browser::column_widths, "column-widths", 1, 2),
    new Procedure(spark_fltk_browser::data, "data", 2, 3),
    new Procedure(spark_fltk_browser::format_char, "format-char", 1, 2),
    new Procedure(spark_fltk_browser::hide_line, "hide-line", 2),
    new Procedure(spark_fltk_browser::load, "load", 2),
    new Procedure(spark_fltk_browser::middleline, "middleline", 2),
    new Procedure(spark_fltk_browser::move, "move", 3),
    new Procedure(spark_fltk_browser::remove, "remove", 2),
    new Procedure(spark_fltk_browser::show_line, "show-line", 2),
    new Procedure(spark_fltk_browser::count, "count", 1),
    new Procedure(spark_fltk_browser::swap, "swap", 3),
    new Procedure(spark_fltk_browser::text, "text", 1, 2),
    new Procedure(spark_fltk_browser::topline, "topline", 1, 2),
    new Procedure(spark_fltk_browser::line_visible, "line-visible", 2),
    new Procedure(spark_fltk_browser::type, "browser-type", 1, 2),
    // Fl_File_Browser
    new Procedure(spark_fltk_browser::iconsize, "iconsize", 1, 2),
    new Procedure(spark_fltk_browser::filter, "filter", 1, 2),
    new Procedure(spark_fltk_browser::filetype, "file-type", 1, 2),
    new Procedure(spark_fltk_browser::load_dir, "load-dir", 1, 2),
    // Fl_Select_Browser
    new Procedure(spark_fltk_browser::make_visible, "make-line-visible", 2),
    // Fl_Check_Browser
    new Procedure(spark_fltk_browser::cb_add_line, "cb-add-line", 2, 3),
    new Procedure(spark_fltk_browser::cb_check_all, "cb-check-all", 1),
    new Procedure(spark_fltk_browser::cb_check_none, "cb-check-none", 1),
    new Procedure(spark_fltk_browser::cb_checked, "cb-checked", 2, 3),
    new Procedure(spark_fltk_browser::cb_nchecked, "cb-nchecked", 1),
    new Procedure(spark_fltk_browser::cb_clear, "cb-clear", 1),
    new Procedure(spark_fltk_browser::cb_nitems, "cb-nitems", 1),
    new Procedure(spark_fltk_browser::cb_remove, "cb-remove", 2),
    new Procedure(spark_fltk_browser::cb_text, "cb-text", 2),
    new Procedure(spark_fltk_browser::cb_value, "cb-value", 1),
    // generic
    new Procedure(spark_fltk_browser::deselect, "deselect", 2),
    new Procedure(spark_fltk_browser::select_item, "select-item", 3),
    new Procedure(spark_fltk_browser::selected, "browser-selected", 2),
    new Procedure(spark_fltk_browser::current_selection, "current-selection", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_browser_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("NO-SCROLLS", 0),
    Constant("SCROLL-HORIZONTAL", Fl_Browser_::HORIZONTAL),
    Constant("SCROLL-VERTICAL", Fl_Browser_::VERTICAL),
    Constant("SCROLL-BOTH", Fl_Browser_::BOTH),
    Constant("SCROLL-HORIZONTAL-ALWAYS", Fl_Browser_::HORIZONTAL_ALWAYS),
    Constant("SCROLL-VERTICAL-ALWAYS", Fl_Browser_::VERTICAL_ALWAYS),
    Constant("SCROLL-BOTH-ALWAYS", Fl_Browser_::BOTH_ALWAYS),
    Constant("FL-ALPHASORT", FL_ALPHASORT), 
    Constant("FL-CASEALPHASORT", FL_CASEALPHASORT),
    Constant("FL-CASENUMERICSORT", FL_CASENUMERICSORT),
    Constant("FL-NUMERICSORT", FL_NUMERICSORT),
    Constant("FL-FILES", Fl_File_Browser::FILES),
    Constant("FL-DIRECTORIES", Fl_File_Browser::DIRECTORIES),
    Constant("NORMAL-BROWSER", FL_NORMAL_BROWSER),
    Constant("SELECT-BROWSER", FL_SELECT_BROWSER),
    Constant("HOLD-BROWSER", FL_HOLD_BROWSER),
    Constant("MULTI-BROWSER", FL_MULTI_BROWSER),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Browser_* _get_fl_browser_(int, Scheme_Object**, int);
static Fl_Browser* _get_fl_browser(int, Scheme_Object**, int);
static Fl_Check_Browser* _get_fl_check_browser(int, Scheme_Object**, int);

Scheme_Object* 
spark_fltk_browser::fl_browser(int argc, Scheme_Object** argv)
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
  Fl_Browser* browser = 0;
  if (argc == 5)
    browser = new Fl_Browser(x, y, w, h);
  else
    {
      if (!SCHEME_SYMBOLP(argv[5]))
	scheme_wrong_type("fl-browser", "symbol", 5, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[5]);
      if (s == "file")
	browser = new Fl_File_Browser(x, y, w, h);
      else if (s == "hold")
	browser = new Fl_Hold_Browser(x, y, w, h);
      else if (s == "multi")
	browser = new Fl_Multi_Browser(x, y, w, h);
      else if (s == "select")
	browser = new Fl_Select_Browser(x, y, w, h);
      else
	{
	  DEFAULT_RET_FINISH;
	}
    }
  if (title.length() > 0)
    browser->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  browser->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(browser, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::fl_check_browser(int argc, Scheme_Object** argv)
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
  Fl_Check_Browser* browser = new Fl_Check_Browser(x, y, w, h, title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  browser->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    _ret_ = scheme_make_cptr(browser, tag);
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

// Fl_Browser_

Scheme_Object*
spark_fltk_browser::has_scrollbar(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  int i = browser->has_scrollbar();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      browser->has_scrollbar(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::hposition(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  int i = browser->hposition();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      browser->hposition(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::vscroll_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  int i = browser->position();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      browser->position(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::scrollbar_left(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      browser->scrollbar_left();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::scrollbar_right(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      browser->scrollbar_right();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::scrollbar_width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  int i = browser->scrollbar_width();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      browser->scrollbar_width(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::select(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      void* p = SCHEME_CPTR_VAL(argv[1]);
      if (!p)
	{
	  scheme_wrong_type("select", "cptr-val", 
			    1, argc, argv);
	  DEFAULT_RET_FINISH;
	}
      int s = 0;
      spark::Utils::int_from_scheme_long(argv[2], s);
      int docb = argv[3] == scheme_true ? 1 : 0;
      _ret_ = scheme_false;
      if (browser->select(p, s, docb))
	_ret_ = scheme_true;      
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  Fl_Color c = browser->textcolor();
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
	      browser->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  Fl_Font f = browser->textfont();
	  long i = static_cast<long>(f);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  Fl_Font f;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      f = static_cast<Fl_Font>(i);
	      browser->textfont(f);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_browser::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser_* browser = _get_fl_browser_(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  uchar c = browser->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  uchar c;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      c = static_cast<uchar>(i);
	      browser->textsize(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

// Fl_Browser

Scheme_Object* 
spark_fltk_browser::add_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      std::string text;
      if (argv[1] != scheme_null)
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("add_line", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  text = SCHEME_BYTE_STR_VAL(str);
	}
      browser->add(text.c_str());
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::bottomline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->bottomline(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      browser->clear();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::column_char(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  char c = browser->column_char();
	  _ret_ = scheme_make_ascii_character(static_cast<mzchar>(c));
	}
      else
	{
	  if (!SCHEME_CHARP(argv[1]))
	    scheme_wrong_type("column-char", "char", 1, argc, argv);
	  mzchar c = SCHEME_CHAR_VAL(argv[1]);
	  browser->column_char(static_cast<char>(c));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::column_widths(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  const int* w = browser->column_widths();
	  size_t sz = 0;
	  std::vector<int> widths;
	  while (w)
	    {
	      if ((*w) == 0)
		break;
	      widths.push_back(*w);
	      ++sz;
	      ++w;
	    }
	  Scheme_Object** elems = new Scheme_Object*[sz];
	  for (size_t i=0; i<sz; ++i)
	    {
	      Scheme_Object* obj = NULL;
	      MZ_GC_DECL_REG(1);
	      MZ_GC_VAR_IN_REG(0, obj);
	      MZ_GC_REG();
	      obj = scheme_make_integer(widths[i]);
	      elems[i] = obj;
	      MZ_GC_UNREG();
	    }
	  _ret_ = scheme_build_list(sz, elems);     
	  delete[] elems;
	}
      else
	{
	  if (!SCHEME_LISTP(argv[1]))
	    scheme_wrong_type("column-widths", "list", 1, argc, argv);
	  std::vector<int> widths;
	  Scheme_Object* lst = argv[1];
	  while (lst)
	    {
	      Scheme_Object* obj = SCHEME_CAR(lst);
	      if (obj != scheme_null)
		{
		  int i = 0;
		  if (spark::Utils::int_from_scheme_long(obj, i))
		    widths.push_back(i);
		}
	      lst = SCHEME_CDR(lst);
	      if (lst == scheme_null)
		break;
	    }
	  int* w = 0;
	  size_t sz = widths.size();
	  if (sz)
	    {
	      w = new int[sz + 1];
	      for (size_t i=0; i<sz; ++i)
		w[i] = widths[i];
	      w[sz] = 0;
	    }
	  else
	    {
	      w = new int[1];
	      w[0] = 0;
	    }
	  browser->column_widths(w);
	  delete[] w;
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::data(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      if (argc == 2)
	{
	  void* p = browser->data(i);
	  if (p)
	    _ret_ = reinterpret_cast<Scheme_Object*>(p);	  
	}
      else
	{
	  if (argv[2] != scheme_null)
	    browser->data(i, reinterpret_cast<void*>(argv[2]));
	  else
	    browser->data(i, 0);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::format_char(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  char c = browser->format_char();
	  char s[2]; s[0] = c; s[1] = 0;
	  _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("column-char", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  browser->format_char(s[0]);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::hide_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->hide(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::load(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("load", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string s = SCHEME_BYTE_STR_VAL(str);
      browser->load(s.c_str());
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::middleline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->middleline(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::move(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int to = 0;
      spark::Utils::int_from_scheme_long(argv[1], to);
      int from = 0;
      spark::Utils::int_from_scheme_long(argv[2], from);
      browser->move(to, from);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::remove(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->remove(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::show_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->show(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::count(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int c = browser->size();
      _ret_ = scheme_make_integer(c);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::swap(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int a = 0;
      spark::Utils::int_from_scheme_long(argv[1], a);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[2], b);
      browser->swap(a, b);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      if (argc == 2)
	{
	  const char* text = browser->text(i);
	  if (text)
	    {
	      _ret_ = scheme_make_utf8_string(text); 
	    }
	}
      else
	{
	  if (argv[2] != scheme_null)
	    {
	      if (!SCHEME_CHAR_STRINGP(argv[2]))
		scheme_wrong_type("text", "string", 2, argc, argv);
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	      std::string text = SCHEME_BYTE_STR_VAL(str);
	      browser->text(i, text.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::topline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  browser->topline(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::line_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      _ret_ = scheme_false;
      if (browser->visible(i))
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::make_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_Select_Browser* select_browser = 
	dynamic_cast<Fl_Select_Browser*>(browser);
      if (select_browser)
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  select_browser->make_visible(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

// Fl_File_Browser

Scheme_Object* 
spark_fltk_browser::iconsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (!file_browser)
	{
	  DEFAULT_RET_FINISH;
	}
      if (argc == 1)
	{
	  uchar c = file_browser->iconsize();
	  _ret_ = scheme_make_integer(static_cast<int>(c));
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      file_browser->iconsize(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::filter(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (!file_browser)
	{
	  DEFAULT_RET_FINISH;
	}
      if (argc == 1)
	{
	  const char* filter = file_browser->filter();
	  if (filter)
	    _ret_ = scheme_make_utf8_string(filter); 
	}
      else
	{
	  if (argv[1] != scheme_null)
	    {
	      if (!SCHEME_CHAR_STRINGP(argv[1]))
		scheme_wrong_type("filter", "string", 1, argc, argv);
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string filter = SCHEME_BYTE_STR_VAL(str);
	      file_browser->filter(filter.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::filetype(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (!file_browser)
	{
	  DEFAULT_RET_FINISH;
	}
      if (argc == 1)
	{
	  int i = file_browser->filetype();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      file_browser->filetype(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::load_dir(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (!file_browser)
	{
	  DEFAULT_RET_FINISH;
	}
      if (argv[1] != scheme_null)
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("filter", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string dir = SCHEME_BYTE_STR_VAL(str);
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[2], i);
	  Fl_File_Sort_F* sort = 0;
	  switch (i)
	    {
	    case FL_ALPHASORT:
	      sort = fl_alphasort;
	      break;
	    case FL_CASEALPHASORT:
	      sort = fl_casealphasort;
	      break;
	    case FL_CASENUMERICSORT:
	      sort = fl_casenumericsort;
	      break;
	    case FL_NUMERICSORT:
	    default:
	      sort = fl_numericsort;
	      break;
	    }
	  i = file_browser->load(dir.c_str(), sort);
	  _ret_ = scheme_make_integer(i);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      if (argc == 1)
	{
	  int i = browser->type();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  browser->type(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

// Fl_Check_Browser

Scheme_Object* 
spark_fltk_browser::cb_add_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("cb-add-line", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);	  
      std::string text = SCHEME_BYTE_STR_VAL(str);
      int sel = 0;
      if (argc == 3)
	sel = argv[2] == scheme_true ? 1 : 0;
      browser->add(text.c_str(), sel);
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_check_all(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      browser->check_all();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_check_none(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      browser->check_none();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_checked(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      if (argc == 2)
	_ret_ = (browser->checked(i) == 1) ? scheme_true : scheme_false;
      else
	{
	  int sel = (argv[2] == scheme_true) ? 1 : 0;
	  browser->checked(i, sel);
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      browser->clear();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_nchecked(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = browser->nchecked();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_nitems(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = browser->nitems();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_remove(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      _ret_ = scheme_make_integer(browser->remove(i));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_text(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      _ret_ = scheme_make_utf8_string(browser->text(i));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::cb_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Check_Browser* browser = _get_fl_check_browser(argc, argv, 0);
  if (browser)
    {
      int i = browser->value();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

// generic

Scheme_Object* 
spark_fltk_browser::deselect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  int docb = 0;
  if (argv[1] == scheme_true)
    docb = 1;
  if (browser)
    {
      _ret_ = scheme_true;
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (file_browser)
	{
	  file_browser->deselect(docb);
	  DEFAULT_RET_FINISH;
	}
      Fl_Hold_Browser* hold_browser = dynamic_cast<Fl_Hold_Browser*>(browser);
      if (hold_browser)
	{
	  hold_browser->deselect(docb);
	  DEFAULT_RET_FINISH;
	}
      Fl_Multi_Browser* multi_browser = dynamic_cast<Fl_Multi_Browser*>(browser);
      if (multi_browser)
	{
	  multi_browser->deselect(docb);
	  DEFAULT_RET_FINISH;
	}
      Fl_Select_Browser* select_browser = dynamic_cast<Fl_Select_Browser*>(browser);
      if (select_browser)
	{
	  select_browser->deselect(docb);
	  DEFAULT_RET_FINISH;
	}
      _ret_ = scheme_null;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::select_item(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  int sel = argv[2] == scheme_true ? 1 : 0;
	  _ret_ = scheme_true;
	  Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
	  if (file_browser)
	    {
	      file_browser->select(i, sel);
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Hold_Browser* hold_browser = dynamic_cast<Fl_Hold_Browser*>(browser);
	  if (hold_browser)
	    {
	      hold_browser->select(i, sel);
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Multi_Browser* multi_browser = dynamic_cast<Fl_Multi_Browser*>(browser);
	  if (multi_browser)
	    {
	      multi_browser->select(i, sel);
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Select_Browser* select_browser = dynamic_cast<Fl_Select_Browser*>(browser);
	  if (select_browser)
	    {
	      select_browser->select(i, sel);
	      DEFAULT_RET_FINISH;
	    }
	  _ret_ = scheme_null;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::selected(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  _ret_ = scheme_false;
	  Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
	  if (file_browser)
	    {
	      if (file_browser->selected(i))
		_ret_ = scheme_true;
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Hold_Browser* hold_browser = dynamic_cast<Fl_Hold_Browser*>(browser);
	  if (hold_browser)
	    {
	      if (hold_browser->selected(i))
		_ret_ = scheme_true;
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Multi_Browser* multi_browser = dynamic_cast<Fl_Multi_Browser*>(browser);
	  if (multi_browser)
	    {
	      if (multi_browser->selected(i))
		_ret_ = scheme_true;
	      DEFAULT_RET_FINISH;
	    }
	  Fl_Select_Browser* select_browser = dynamic_cast<Fl_Select_Browser*>(browser);
	  if (select_browser)
	    {
	      if (select_browser->selected(i))
		_ret_ = scheme_true;
	      DEFAULT_RET_FINISH;
	    }
	  _ret_ = scheme_null;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_browser::current_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Browser* browser = _get_fl_browser(argc, argv, 0);
  if (browser)
    {
      Fl_File_Browser* file_browser = dynamic_cast<Fl_File_Browser*>(browser);
      if (file_browser)
	{
	  _ret_ = scheme_make_integer(file_browser->value());
	  DEFAULT_RET_FINISH;
	}
      Fl_Hold_Browser* hold_browser = dynamic_cast<Fl_Hold_Browser*>(browser);
      if (hold_browser)
	{
	  _ret_ = scheme_make_integer(hold_browser->value());
	  DEFAULT_RET_FINISH;
	}
      Fl_Multi_Browser* multi_browser = dynamic_cast<Fl_Multi_Browser*>(browser);
      if (multi_browser)
	{
	  _ret_ = scheme_make_integer(multi_browser->value());
	  DEFAULT_RET_FINISH;
	}
      Fl_Select_Browser* select_browser = dynamic_cast<Fl_Select_Browser*>(browser);
      if (select_browser)
	{
	  _ret_ = scheme_make_integer(select_browser->value());
	  DEFAULT_RET_FINISH;
	}
      _ret_ = scheme_null;
    }

  DEFAULT_RET_FINISH;
}

Fl_Browser_*
_get_fl_browser_(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Browser_*>(widget);
}

Fl_Browser*
_get_fl_browser(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Browser*>(widget);
}

Fl_Check_Browser*
_get_fl_check_browser(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Check_Browser*>(widget);
}
