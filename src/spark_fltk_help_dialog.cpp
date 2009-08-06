// MzScheme inetrface to the FLTK Help_Dialog widget.
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

#include <FL/Fl_Help_Dialog.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_help_dialog
{
  static Scheme_Object* fl_help_dialog(int, Scheme_Object**);
  static Scheme_Object* hide(int, Scheme_Object**);
  static Scheme_Object* load(int, Scheme_Object**);
  static Scheme_Object* position(int, Scheme_Object**);
  static Scheme_Object* resize(int, Scheme_Object**);
  static Scheme_Object* show(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* topline(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* visible(int, Scheme_Object**);
  static Scheme_Object* width(int, Scheme_Object**);
  static Scheme_Object* height(int, Scheme_Object**);
  static Scheme_Object* xpos(int, Scheme_Object**);
  static Scheme_Object* ypos(int, Scheme_Object**);
  static Scheme_Object* dispose(int, Scheme_Object**);
} // namespace spark_fltk_help_dialog

spark::Status_code
spark_fltk::_add_help_dialog_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_help_dialog::fl_help_dialog, 
		  "fl-help-dialog", 0, 1),
    new Procedure(spark_fltk_help_dialog::hide, 
		  "help-dialog-hide", 1),
    new Procedure(spark_fltk_help_dialog::load, 
		  "help-dialog-load", 2),
    new Procedure(spark_fltk_help_dialog::position, 
		  "help-dialog-position", 3),
    new Procedure(spark_fltk_help_dialog::resize, 
		  "help-dialog-resize", 5),
    new Procedure(spark_fltk_help_dialog::show, 
		  "help-dialog-show", 1),
    new Procedure(spark_fltk_help_dialog::textsize, 
		  "help-dialog-text-size",1, 2),
    new Procedure(spark_fltk_help_dialog::topline, 
		  "help-dialog-topline", 2),
    new Procedure(spark_fltk_help_dialog::value, 
		  "help-dialog-value", 1, 2),
    new Procedure(spark_fltk_help_dialog::visible, 
		  "help-dialog-visible", 1),
    new Procedure(spark_fltk_help_dialog::width, 
		  "help-dialog-width", 1),
    new Procedure(spark_fltk_help_dialog::height, 
		  "help-dialog-height", 1),
    new Procedure(spark_fltk_help_dialog::xpos, 
		  "help-dialog-xpos", 1),
    new Procedure(spark_fltk_help_dialog::ypos, 
		  "help-dialog-ypos", 1),
    new Procedure(spark_fltk_help_dialog::dispose, 
		  "help-dialog-dispose", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Help_Dialog* _get_fl_help_dialog(int argc, 
					   Scheme_Object** argv, 
					   int index);
Scheme_Object* 
spark_fltk_help_dialog::fl_help_dialog(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = new Fl_Help_Dialog();
  if (argc == 1)
    {
      if (!SCHEME_CHAR_STRINGP(argv[0]))
	scheme_wrong_type("fl-help-dialog", "string", 0, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      std::string f = SCHEME_BYTE_STR_VAL(str);
      hd->load(f.c_str());
    }
  Fltk_tag t = FL_HELP_DIALOG_TAG;
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(hd, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::height(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    _ret_ = scheme_make_integer(hd->h());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    _ret_ = scheme_make_integer(hd->w());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::xpos(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    _ret_ = scheme_make_integer(hd->x());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::ypos(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    _ret_ = scheme_make_integer(hd->y());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::hide(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      hd->hide();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::show(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      hd->show();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::load(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("load-url", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string tt = SCHEME_BYTE_STR_VAL(str);
      hd->load(tt.c_str());
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      hd->position(x, y);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::resize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[3], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[4], h);
      hd->resize(x, y, w, h);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
      _ret_ = hd->visible() ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      if (argc == 1)
	{
	  uchar c = hd->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      hd->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::topline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      int i = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], i))
	{
	  hd->topline(i);
	  _ret_ = scheme_true;
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("help-topline", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string tt = SCHEME_BYTE_STR_VAL(str);
	  hd->topline(tt.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      if (argc == 1)
	{
	  const char* s = hd->value();
	  _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("help-text", "string", 1, argc, argv);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string tt = SCHEME_BYTE_STR_VAL(str);
	  hd->value(tt.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_help_dialog::dispose(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Help_Dialog* hd = _get_fl_help_dialog(argc, argv, 0);
  if (hd)
    {
      delete hd;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Fl_Help_Dialog*
_get_fl_help_dialog(int argc, Scheme_Object** argv, int index)
{
  Fltk_tag tag = _get_tag(argc, argv, index);
  if (tag != FL_HELP_DIALOG_TAG)
      {
	scheme_wrong_type("_get_fl_help_dialog", 
			  "tag", 
			  index, 
			  argc, 
			  argv);
	return 0;
      }
  void* p = SCHEME_CPTR_VAL(argv[index]);
  if (!p)
    {
      scheme_wrong_type("_get_fl_help_dialog", 
			"cptr-val", 
			index, 
			argc, 
			argv);
      return 0;
    }
  return reinterpret_cast<Fl_Help_Dialog*>(p);
}


