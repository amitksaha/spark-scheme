// MzScheme inetrface to the FLTK Fl_File_Chooser classes.
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

#include <FL/Fl_File_Chooser.H>
#include <FL/filename.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_file_chooser
{
  static Scheme_Object* fl_file_chooser(int, Scheme_Object**);
  static Scheme_Object* color(int, Scheme_Object**);
  static Scheme_Object* count(int, Scheme_Object**);
  static Scheme_Object* directory(int, Scheme_Object**);
  static Scheme_Object* filter(int, Scheme_Object**);
  static Scheme_Object* filter_value(int, Scheme_Object**);
  static Scheme_Object* hide(int, Scheme_Object**);
  static Scheme_Object* iconsize(int, Scheme_Object**);
  static Scheme_Object* label(int, Scheme_Object**);
  static Scheme_Object* ok_label(int, Scheme_Object**);
  static Scheme_Object* preview(int, Scheme_Object**);
  static Scheme_Object* rescan(int, Scheme_Object**);
  static Scheme_Object* show(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* value(int, Scheme_Object**);
  static Scheme_Object* visible(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* dispose(int, Scheme_Object**);
  // global
  static Scheme_Object* static_label(int, Scheme_Object**);
  static Scheme_Object* sort(int, Scheme_Object**);
  static Scheme_Object* show_file_chooser(int, Scheme_Object**);
  static Scheme_Object* file_chooser_ok_label(int, Scheme_Object**);
  static Scheme_Object* filename_absolute(int, Scheme_Object**);
  static Scheme_Object* filename_expand(int, Scheme_Object**);
  static Scheme_Object* filename_ext(int, Scheme_Object**);
  static Scheme_Object* filename_isdir(int, Scheme_Object**);
  static Scheme_Object* filename_match(int, Scheme_Object**);
  static Scheme_Object* filename_name(int, Scheme_Object**);
  static Scheme_Object* filename_relative(int, Scheme_Object**);
  static Scheme_Object* filename_name(int, Scheme_Object**);
  static Scheme_Object* filename_setext(int, Scheme_Object**);
} // namespace spark_fltk_file_chooser


spark::Status_code
spark_fltk::_add_file_chooser_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_file_chooser::fl_file_chooser, 
		  "fl-file-chooser", 0, 4),
    new Procedure(spark_fltk_file_chooser::color, 
		  "file-chooser-color", 1, 2),
    new Procedure(spark_fltk_file_chooser::count, 
		  "file-chooser-count", 1),
    new Procedure(spark_fltk_file_chooser::directory, 
		  "file-chooser-directory", 1, 2),
    new Procedure(spark_fltk_file_chooser::filter, 
		  "file-chooser-filter", 1, 2),
    new Procedure(spark_fltk_file_chooser::filter_value, 
		  "file-chooser-filter-value", 1, 2),
    new Procedure(spark_fltk_file_chooser::hide, 
		  "file-chooser-hide", 1),
    new Procedure(spark_fltk_file_chooser::iconsize, 
		  "file-chooser-icon-size", 1, 2),
    new Procedure(spark_fltk_file_chooser::label, 
		  "file-chooser-label", 1, 2),
    new Procedure(spark_fltk_file_chooser::ok_label, 
		  "file-chooser-ok-label", 1, 2),
    new Procedure(spark_fltk_file_chooser::preview, 
		  "file-chooser-preview", 1, 2),
    new Procedure(spark_fltk_file_chooser::rescan, 
		  "file-chooser-rescan", 1),
    new Procedure(spark_fltk_file_chooser::show, 
		  "file-chooser-show", 1),
    new Procedure(spark_fltk_file_chooser::textcolor, 
		  "file-chooser-text-color", 1, 2),
    new Procedure(spark_fltk_file_chooser::textfont, 
		  "file-chooser-text-font", 1, 2),
    new Procedure(spark_fltk_file_chooser::textsize, 
		  "file-chooser-text-size", 1, 2),
    new Procedure(spark_fltk_file_chooser::value, 
		  "file-chooser-value", 1, 2),
    new Procedure(spark_fltk_file_chooser::visible, 
		  "file-chooser-visible", 1),
    new Procedure(spark_fltk_file_chooser::type, 
		  "file-chooser-type", 1, 2),
    new Procedure(spark_fltk_file_chooser::dispose, 
		  "file-chooser-dispose", 1),
    // global
    new Procedure(spark_fltk_file_chooser::static_label, 
		  "file-chooser-static-label", 1, 2),
    new Procedure(spark_fltk_file_chooser::sort, 
		  "file-chooser-sort", 1, 2),
    new Procedure(spark_fltk_file_chooser::show_file_chooser, 
		  "show-file-chooser", 3, 4),
    new Procedure(spark_fltk_file_chooser::filename_match, 
		  "filename-match", 2),
    new Procedure(spark_fltk_file_chooser::file_chooser_ok_label, 
		  "fl-file-chooser-ok-label", 1),
    new Procedure(spark_fltk_file_chooser::filename_absolute, 
		  "filename-absolute", 1),
    new Procedure(spark_fltk_file_chooser::filename_relative, 
		  "filename-relative", 1),
    new Procedure(spark_fltk_file_chooser::filename_expand, 
		  "filename-expand", 1),
    new Procedure(spark_fltk_file_chooser::filename_ext, 
		  "filename-ext", 1),
    new Procedure(spark_fltk_file_chooser::filename_setext, 
		  "filename-setext", 2),
    new Procedure(spark_fltk_file_chooser::filename_name, 
		  "filename-name", 1),
    new Procedure(spark_fltk_file_chooser::filename_isdir, 
		  "filename-isdir", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_file_chooser_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("FC-SINGLE", Fl_File_Chooser::SINGLE),
    Constant("FC-MULTI", Fl_File_Chooser::MULTI),
    Constant("FC-CREATE", Fl_File_Chooser::CREATE),
    Constant("FC-DIRECTORY", Fl_File_Chooser::DIRECTORY),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_File_Chooser* _get_fl_file_chooser(int, Scheme_Object**, 
					     int);

Scheme_Object* 
spark_fltk_file_chooser::fl_file_chooser(int argc, 
					 Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = 0;
  std::string path = ".";
  std::string pattern = "*.*";
  int type = Fl_File_Chooser::SINGLE;
  std::string title = "";
  if (argc >= 1)
    {
      if (!SCHEME_CHAR_STRINGP(argv[0]))
	scheme_wrong_type("fl-file-chooser", "string", 
			  0, argc, argv);	
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      path = SCHEME_BYTE_STR_VAL(str);
    }
  if (argc >= 2)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("fl-file-chooser", "string", 
			  1, argc, argv);	
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      pattern = SCHEME_BYTE_STR_VAL(str);
    }
  if (argc >= 3)
    spark::Utils::int_from_scheme_long(argv[2], type);
  if (argc >= 4)
    {
      if (!SCHEME_CHAR_STRINGP(argv[3]))
	scheme_wrong_type("fl-file-chooser", "string", 
			  3, argc, argv);	
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      title = SCHEME_BYTE_STR_VAL(str);
    }
  fc = new Fl_File_Chooser(path.c_str(), pattern.c_str(), 
			   type, title.c_str());
  Fltk_tag t = FL_FILE_CHOOSER_TAG;
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(fc, tag);
  }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->color());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Color c = static_cast<Fl_Color>(i);
	  fc->color(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::count(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    _ret_ = scheme_make_integer(fc->count());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::hide(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      fc->hide();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::show(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      fc->show();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::rescan(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      fc->rescan();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}


Scheme_Object*
spark_fltk_file_chooser::preview(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = fc->preview() ? scheme_true : scheme_false;
      else
	{
	  int i = 0;
	  if (argv[1] == scheme_true)
	    i = 1;
	  fc->preview(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::directory(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	{
	  const char* d = fc->directory();
	  if (d)
	    _ret_ = scheme_make_utf8_string(d);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("file-chooser-directory", 
			      "string", 1, argc, argv);
	  Scheme_Object* str = 
	    scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  fc->directory(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filter(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	{
	  const char* d = fc->filter();
	  if (d)
	    _ret_ = scheme_make_utf8_string(d);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("file-chooser-filter", 
			      "string", 1, argc, argv);
	  Scheme_Object* str = 
	    scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  fc->filter(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	{
	  const char* lbl = fc->label();
	  if (lbl)
	    _ret_ = scheme_make_utf8_string(lbl);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("file-chooser-label", 
			      "string", 1, argc, argv);
	  Scheme_Object* str = 
	    scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  size_t len = s.length();
	  if (len > 0)
	    {	      
	      char* title = new char[len + 1];
	      strcpy(title, s.c_str());
	      const char* tmp = fc->label();
	      if (tmp)
		delete[] tmp;
	      fc->label(title);
	    }
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::ok_label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	{
	  const char* lbl = fc->ok_label();
	  if (lbl)
	    _ret_ = scheme_make_utf8_string(lbl);
	}
      else
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("file-chooser-label", 
			      "string", 1, argc, argv);
	  Scheme_Object* str = 
	    scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  fc->ok_label(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	{
	  const char* v = fc->value();
	  if (v)
	    _ret_ = scheme_make_utf8_string(v);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = 
		scheme_char_string_to_byte_string(argv[1]);
	      std::string s = SCHEME_BYTE_STR_VAL(str);
	      fc->value(s.c_str());
	    }
	  else
	    {
	      int i = 0;
	      spark::Utils::int_from_scheme_long(argv[1], i);
	      fc->value(i);
	    }
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filter_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->filter_value());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  fc->filter_value(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->type());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  fc->type(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    _ret_ = fc->visible() ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::dispose(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      const char* s = fc->label();
      if (s)
	delete[] s;
      delete fc;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->textcolor());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Color c = static_cast<Fl_Color>(i);
	  fc->textcolor(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->textfont());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Font c = static_cast<Fl_Font>(i);
	  fc->textfont(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->textsize());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  uchar c = static_cast<uchar>(i);
	  fc->textsize(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::iconsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_File_Chooser* fc = _get_fl_file_chooser(argc, argv, 0);
  if (fc)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(fc->iconsize());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  uchar c = static_cast<uchar>(i);
	  fc->iconsize(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

// global

Scheme_Object*
spark_fltk_file_chooser::static_label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("file-chooser-static-label", 
		      "symbol", 0, argc, argv);
  std::string s = SCHEME_SYM_VAL(argv[0]);
  if (argc == 1)
    {
      const char* lbl = 0;
      if (s == "add-favorites")
	lbl = Fl_File_Chooser::add_favorites_label;
      else if (s == "all-files")
	lbl = Fl_File_Chooser::all_files_label;
      else if (s == "custom-filter")
	lbl = Fl_File_Chooser::custom_filter_label;
      else if (s == "existing-file")
	lbl = Fl_File_Chooser::existing_file_label;
      else if (s == "favorites")
	lbl = Fl_File_Chooser::favorites_label;
      else if (s == "filename")
	lbl = Fl_File_Chooser::filename_label;
      else if (s == "filesystems")
	lbl = Fl_File_Chooser::filesystems_label;
      else if (s == "manage-favorites")
	lbl = Fl_File_Chooser::manage_favorites_label;
      else if (s == "new-directory")
	lbl = Fl_File_Chooser::new_directory_label;
      else if (s == "new-directory-tooltip")
	lbl = Fl_File_Chooser::new_directory_tooltip;
      else if (s == "preview")
	lbl = Fl_File_Chooser::preview_label;
      else if (s == "save")
	lbl = Fl_File_Chooser::save_label;
      else if (s == "show")
	lbl = Fl_File_Chooser::show_label;
      if (lbl)
	_ret_ = scheme_make_utf8_string(lbl);
    }
  else
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("file-chooser-static-label", 
			  "string", 1, argc, argv);
      Scheme_Object* str = 
	scheme_char_string_to_byte_string(argv[1]);
      std::string lbl = SCHEME_BYTE_STR_VAL(str);
      _ret_ = scheme_true;
      if (s == "add-favorites")
	Fl_File_Chooser::add_favorites_label = lbl.c_str();
      else if (s == "all-files")
	Fl_File_Chooser::all_files_label = lbl.c_str();
      else if (s == "custom-filter")
	Fl_File_Chooser::custom_filter_label = lbl.c_str();
      else if (s == "existing-file")
	Fl_File_Chooser::existing_file_label = lbl.c_str();
      else if (s == "favorites")
	Fl_File_Chooser::favorites_label = lbl.c_str();
      else if (s == "filename")
	Fl_File_Chooser::filename_label = lbl.c_str();
      else if (s == "filesystems")
	Fl_File_Chooser::filesystems_label = lbl.c_str();
      else if (s == "manage-favorites")
	Fl_File_Chooser::manage_favorites_label = lbl.c_str();
      else if (s == "new-directory")
	Fl_File_Chooser::new_directory_label = lbl.c_str();
      else if (s == "new-directory-tooltip")
	Fl_File_Chooser::new_directory_tooltip = lbl.c_str();
      else if (s == "preview")
	Fl_File_Chooser::preview_label = lbl.c_str();
      else if (s == "save")
	Fl_File_Chooser::save_label = lbl.c_str();
      else if (s == "show")
	Fl_File_Chooser::show_label = lbl.c_str();
      else
	_ret_ = scheme_null;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::sort(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    {
      int s = 0;
      if (Fl_File_Chooser::sort == fl_alphasort)
	s = FL_ALPHASORT;
      else if (Fl_File_Chooser::sort == fl_casealphasort)
	s = FL_CASEALPHASORT;
      else if (Fl_File_Chooser::sort == fl_casenumericsort)
	s = FL_CASENUMERICSORT;
      else if (Fl_File_Chooser::sort == fl_numericsort)
	s = FL_NUMERICSORT;
      _ret_ = scheme_make_integer(s);
    }
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[2], i);
      switch (i)
	{
	case FL_ALPHASORT:
	  Fl_File_Chooser::sort = fl_alphasort;
	  break;
	case FL_CASEALPHASORT:
	  Fl_File_Chooser::sort = fl_casealphasort;
	  break;
	case FL_CASENUMERICSORT:
	  Fl_File_Chooser::sort = fl_casenumericsort;
	  break;
	case FL_NUMERICSORT:
	default:
	  Fl_File_Chooser::sort = fl_numericsort;
	  break;
	}
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::show_file_chooser(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  if (!SCHEME_CHAR_STRINGP(argv[i]))
    scheme_wrong_type("show-file-chooser", "string", 
		      i, argc, argv);	
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[i++]);
  std::string message = SCHEME_BYTE_STR_VAL(str);
  if (!SCHEME_CHAR_STRINGP(argv[i]))
    scheme_wrong_type("show-file-chooser", "string", 
		      i, argc, argv);	
  str = scheme_char_string_to_byte_string(argv[i++]);
  std::string pattern = SCHEME_BYTE_STR_VAL(str);
  if (!SCHEME_CHAR_STRINGP(argv[i]))
    scheme_wrong_type("show-file-chooser", "string", 
		      i, argc, argv);	
  str = scheme_char_string_to_byte_string(argv[i++]);
  std::string def_filename = SCHEME_BYTE_STR_VAL(str);

  int relative = 0;
  if (argc >= 4)
    relative = (argv[3] == scheme_true) ? 1 : 0;

  char* f = ::fl_file_chooser(message.c_str(), pattern.c_str(),
			      def_filename.c_str(), relative);
  if (f)
    _ret_ = scheme_make_utf8_string(f);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::file_chooser_ok_label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string label;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      label = SCHEME_BYTE_STR_VAL(str);
    }
  ::fl_file_chooser_ok_label(label.c_str());
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_absolute(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string from;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      from = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  char to[FL_PATH_MAX + 1];
  if (::fl_filename_absolute(to, from.c_str()))
    _ret_ = scheme_make_utf8_string(to);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_relative(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string from;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      from = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  char to[FL_PATH_MAX + 1];
  if (::fl_filename_relative(to, from.c_str()))
    _ret_ = scheme_make_utf8_string(to);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_expand(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string from;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      from = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  char to[FL_PATH_MAX + 1];
  if (::fl_filename_expand(to, from.c_str()))
    _ret_ = scheme_make_utf8_string(to);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_ext(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string f;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      f = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  const char* ext = ::fl_filename_ext(f.c_str());
  if (ext)
    _ret_ = scheme_make_utf8_string(ext);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string f;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      f = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  const char* name = ::fl_filename_name(f.c_str());
  if (name)
    _ret_ = scheme_make_utf8_string(name);

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_match(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string f;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      f = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  std::string pattern;
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      pattern = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = ::fl_filename_match(f.c_str(), pattern.c_str()) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_isdir(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string f;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      f = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = (::fl_filename_isdir(f.c_str())) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_file_chooser::filename_setext(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string f;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      f = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  char* ext = new char[FL_PATH_MAX + 1];
  if (SCHEME_CHAR_STRINGP(argv[1]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      ext = SCHEME_BYTE_STR_VAL(str);
    }
  else
    {
      DEFAULT_RET_FINISH;
    }
  ext = ::fl_filename_setext(ext, f.c_str());
  _ret_ = scheme_make_utf8_string(ext);
  delete[] ext;
  
  DEFAULT_RET_FINISH;
}

Fl_File_Chooser*
_get_fl_file_chooser(int argc, Scheme_Object** argv, 
		     int index)
{
  Fltk_tag tag = _get_tag(argc, argv, index);
  if (tag == FL_FILE_CHOOSER_TAG)
  {
    void* p = SCHEME_CPTR_VAL(argv[index]);    
    return reinterpret_cast<Fl_File_Chooser*>(p);
  }
  else
    {
      scheme_wrong_type("_get_fl_file_chooser", "tag", 
			index, argc, argv);
      return 0;
    }
}
