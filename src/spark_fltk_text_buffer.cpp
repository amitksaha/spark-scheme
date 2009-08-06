// MzScheme inetrface to the FLTK Text_Buffer class.
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

#include <vector>
#include "spark_fltk_text_buffer.h"
using namespace spark_fltk;
using namespace spark_fltk_text_buffer;

namespace spark_fltk_text_buffer
{
  static Scheme_Object* fl_text_buffer(int, Scheme_Object**);
  static Scheme_Object* fill(int, Scheme_Object**);
  static Scheme_Object* add_modify_callback(int, Scheme_Object**);
  static Scheme_Object* append(int, Scheme_Object**);
  static Scheme_Object* appendfile(int, Scheme_Object**);
  static Scheme_Object* call_modify_callbacks(int, Scheme_Object**);
  static Scheme_Object* character(int, Scheme_Object**);
  static Scheme_Object* character_width(int, Scheme_Object**);
  static Scheme_Object* clear_rectangular(int, Scheme_Object**);
  static Scheme_Object* copy(int, Scheme_Object**);
  static Scheme_Object* count_displayed_characters(int, Scheme_Object**);
  static Scheme_Object* count_lines(int, Scheme_Object**);
  static Scheme_Object* expand_character(int, Scheme_Object**);
  static Scheme_Object* findchar_backward(int, Scheme_Object**);
  static Scheme_Object* findchar_forward(int, Scheme_Object**);
  static Scheme_Object* findchars_backward(int, Scheme_Object**);
  static Scheme_Object* findchars_forward(int, Scheme_Object**);
  static Scheme_Object* highlight(int, Scheme_Object**);
  static Scheme_Object* highlight_position(int, Scheme_Object**);
  static Scheme_Object* highlight_rectangular(int, Scheme_Object**);
  static Scheme_Object* highlight_selection(int, Scheme_Object**);
  static Scheme_Object* highlight_text(int, Scheme_Object**);
  static Scheme_Object* insert_column(int, Scheme_Object**);
  static Scheme_Object* insertfile(int, Scheme_Object**);
  static Scheme_Object* insert(int, Scheme_Object**);
  static Scheme_Object* length(int, Scheme_Object**);
  static Scheme_Object* line_end(int, Scheme_Object**);
  static Scheme_Object* line_start(int, Scheme_Object**);
  static Scheme_Object* line_text(int, Scheme_Object**);
  static Scheme_Object* loadfile(int, Scheme_Object**);
  static Scheme_Object* null_substitution_character(int, Scheme_Object**);
  static Scheme_Object* outputfile(int, Scheme_Object**);
  static Scheme_Object* overlay_rectangular(int, Scheme_Object**);
  static Scheme_Object* primary_selection(int, Scheme_Object**);
  static Scheme_Object* remove_modify_callback(int, Scheme_Object**);
  static Scheme_Object* remove_rectangular(int, Scheme_Object**);
  static Scheme_Object* remove(int, Scheme_Object**);
  static Scheme_Object* remove_secondary_selection(int, Scheme_Object**);
  static Scheme_Object* remove_selection(int, Scheme_Object**);
  static Scheme_Object* replace_rectangular(int, Scheme_Object**);
  static Scheme_Object* replace(int, Scheme_Object**);
  static Scheme_Object* replace_secondary_selection(int, Scheme_Object**);
  static Scheme_Object* replace_selection(int, Scheme_Object**);
  static Scheme_Object* rewind_lines(int, Scheme_Object**);
  static Scheme_Object* savefile(int, Scheme_Object**);
  static Scheme_Object* search_backward(int, Scheme_Object**);
  static Scheme_Object* search_forward(int, Scheme_Object**);
  static Scheme_Object* secondary_selection_position(int, Scheme_Object**);
  static Scheme_Object* secondary_selection(int, Scheme_Object**);
  static Scheme_Object* secondary_selection_text(int, Scheme_Object**);
  static Scheme_Object* secondary_select_rectangular(int, Scheme_Object**);
  static Scheme_Object* secondary_select(int, Scheme_Object**);
  static Scheme_Object* secondary_unselect(int, Scheme_Object**);
  static Scheme_Object* selected(int, Scheme_Object**);
  static Scheme_Object* selection_position(int, Scheme_Object**);
  static Scheme_Object* selection_text(int, Scheme_Object**);
  static Scheme_Object* select(int, Scheme_Object**);
  static Scheme_Object* select_rectangular(int, Scheme_Object**);
  static Scheme_Object* skip_displayed_characters(int, Scheme_Object**);
  static Scheme_Object* skip_lines(int, Scheme_Object**);
  static Scheme_Object* substitute_null_characters(int, Scheme_Object**);
  static Scheme_Object* unsubstitute_null_characters(int, Scheme_Object**);
  static Scheme_Object* tab_distance(int, Scheme_Object**);
  static Scheme_Object* text_in_rectangle(int, Scheme_Object**);
  static Scheme_Object* text_range(int, Scheme_Object**);
  static Scheme_Object* text(int, Scheme_Object**);
  static Scheme_Object* unhighlight(int, Scheme_Object**);
  static Scheme_Object* unselect(int, Scheme_Object**);
  static Scheme_Object* word_end(int, Scheme_Object**);
  static Scheme_Object* word_start(int, Scheme_Object**);
} // namespace spark_fltk_text_buffer

spark::Status_code
spark_fltk::_add_text_buffer_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_text_buffer::fl_text_buffer,
		  "fl-text-buffer", 1),
    new Procedure(spark_fltk_text_buffer::fill, "text-buffer-fill", 2),
    new Procedure(spark_fltk_text_buffer::add_modify_callback, 
		  "add-modify-callback", 3), 
    new Procedure(spark_fltk_text_buffer::append, "append", 2), 
    new Procedure(spark_fltk_text_buffer::appendfile, "append-file", 2), 
    new Procedure(spark_fltk_text_buffer::call_modify_callbacks, 
		  "call-modify-callbacks", 1), 
    new Procedure(spark_fltk_text_buffer::character, "character", 2),
    new Procedure(spark_fltk_text_buffer::character_width, 
		  "character-width", 4), // static 
    new Procedure(spark_fltk_text_buffer::clear_rectangular, 
		  "clear-rectangular", 5), 
    new Procedure(spark_fltk_text_buffer::copy, 
		  "copy-buffer", 5),
    new Procedure(spark_fltk_text_buffer::count_displayed_characters, 
		  "count-displayed-characters", 3), 
    new Procedure(spark_fltk_text_buffer::count_lines, "count-lines", 3), 
    new Procedure(spark_fltk_text_buffer::expand_character, 
		  "expand-character", 3, 4), // if argc == 4, call static 
    new Procedure(spark_fltk_text_buffer::findchar_backward, 
		  "findchar-backward", 2), 
    new Procedure(spark_fltk_text_buffer::findchar_forward, 
		  "findchar-forward", 2), 
    new Procedure(spark_fltk_text_buffer::findchars_backward, 
		  "findchars-backward", 2), 
    new Procedure(spark_fltk_text_buffer::findchars_forward, 
		  "findchars-forward", 2), 
    new Procedure(spark_fltk_text_buffer::highlight, "highlight", 3),
    new Procedure(spark_fltk_text_buffer::highlight_rectangular, 
		  "highlight-rectangular", 5),
    new Procedure(spark_fltk_text_buffer::highlight_position, 
		  "highlight-position", 1),
    new Procedure(spark_fltk_text_buffer::highlight_selection, 
		  "highlight-selection", 1),
    new Procedure(spark_fltk_text_buffer::highlight_text, 
		  "highlight-text", 1),
    new Procedure(spark_fltk_text_buffer::insert_column, 
		  "insert-column", 4),
    new Procedure(spark_fltk_text_buffer::insertfile, 
		  "insert-file", 3),
    new Procedure(spark_fltk_text_buffer::insert, 
		  "text-buffer-insert-text", 3), 
    new Procedure(spark_fltk_text_buffer::length, "text-buffer-length", 1), 
    new Procedure(spark_fltk_text_buffer::line_end, "line-end", 2), 
    new Procedure(spark_fltk_text_buffer::line_start, "line-start", 2), 
    new Procedure(spark_fltk_text_buffer::line_text, "line-text", 2), 
    new Procedure(spark_fltk_text_buffer::loadfile, "load-file", 2), 
    new Procedure(spark_fltk_text_buffer::null_substitution_character, 
		  "null-substitution-character", 1),
    new Procedure(spark_fltk_text_buffer::outputfile, "output-file", 4),
    new Procedure(spark_fltk_text_buffer::overlay_rectangular, 
		  "overlay-rectangular", 5), 
    new Procedure(spark_fltk_text_buffer::primary_selection, "primary-selection", 1), 
    new Procedure(spark_fltk_text_buffer::remove_modify_callback, 
		  "remove-modify-callback", 3), 
    new Procedure(spark_fltk_text_buffer::remove_rectangular, 
		  "remove-rectangular", 5), 
    new Procedure(spark_fltk_text_buffer::remove, "remove-buffer-text", 3),
    new Procedure(spark_fltk_text_buffer::remove_secondary_selection, 
		  "remove-secondary-selection", 1),  
    new Procedure(spark_fltk_text_buffer::remove_selection, 
		  "remove-selection", 1),  
    new Procedure(spark_fltk_text_buffer::replace_rectangular, 
		  "replace-rectangular", 6), 
    new Procedure(spark_fltk_text_buffer::replace, "replace-buffer-text", 4),
    new Procedure(spark_fltk_text_buffer::replace_secondary_selection, 
		  "replace-secondary-selection", 2),  
    new Procedure(spark_fltk_text_buffer::replace_selection, 
		  "replace-selection", 2),  
    new Procedure(spark_fltk_text_buffer::rewind_lines, 
		  "rewind-lines", 3),  
    new Procedure(spark_fltk_text_buffer::savefile, 
		  "save-file", 2),  
    new Procedure(spark_fltk_text_buffer::search_backward, 
		  "search-backward", 4),  
    new Procedure(spark_fltk_text_buffer::search_forward, 
		  "search-forward", 4),  
    new Procedure(spark_fltk_text_buffer::secondary_selection_position, 
		  "secondary-selection-position", 1),  
    new Procedure(spark_fltk_text_buffer::secondary_selection, 
		  "secondary-selection", 1),  
    new Procedure(spark_fltk_text_buffer::secondary_selection_text, 
		  "secondary-selection-text", 1),  
    new Procedure(spark_fltk_text_buffer::secondary_select_rectangular, 
		  "secondary-select-rectangular", 5),  
    new Procedure(spark_fltk_text_buffer::secondary_select, 
		  "secondary-select", 3),
    new Procedure(spark_fltk_text_buffer::secondary_unselect,
		  "secondary-unselect", 1),    
    new Procedure(spark_fltk_text_buffer::selected,
		  "selected", 1),    
    new Procedure(spark_fltk_text_buffer::selection_position,
		  "selection-position", 2), // if second arg is true, call the second form    
    new Procedure(spark_fltk_text_buffer::selection_text,
		  "selection-text", 1),
    new Procedure(spark_fltk_text_buffer::select_rectangular,
		  "select-rectangular", 5),
    new Procedure(spark_fltk_text_buffer::select, "text-buffer-select", 3),
    new Procedure(spark_fltk_text_buffer::skip_displayed_characters, 
		  "skip-displayed-characters", 3),
    new Procedure(spark_fltk_text_buffer::skip_lines, 
		  "skip-lines", 3),
    new Procedure(spark_fltk_text_buffer::substitute_null_characters, 
		  "substitute-null-characters", 2),
    new Procedure(spark_fltk_text_buffer::tab_distance, "tab-distance", 1, 2),
    new Procedure(spark_fltk_text_buffer::text_in_rectangle, 
		  "text-in-rectangle", 5),
    new Procedure(spark_fltk_text_buffer::text_range, 
		  "text-range", 3),
    new Procedure(spark_fltk_text_buffer::text, 
		  "text-buffer-text", 1, 2),
    new Procedure(spark_fltk_text_buffer::unhighlight, "unhighlight", 1),
    new Procedure(spark_fltk_text_buffer::unselect, "unselect", 1),
    new Procedure(spark_fltk_text_buffer::unsubstitute_null_characters, 
		  "unsubstitute-null-characters", 2),
    new Procedure(spark_fltk_text_buffer::word_start, "text-buffer-word-start", 2), 
    new Procedure(spark_fltk_text_buffer::word_end, "text-buffer-word-end", 2), 
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

Text_buffer_map Text_buffer_holder::_active_buffers;

static Fl_Text_Buffer* _get_fl_text_buffer(int argc, 
					   Scheme_Object** argv, 
					   int index);
static void _modify_callback(int pos, int num_inserted,
			     int num_deleted, int num_restyled,
			     const char* deleted_text,
			     void* cb_param);

Scheme_Object* 
spark_fltk_text_buffer::fl_text_buffer(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int size = 0;
  spark::Utils::int_from_scheme_long(argv[0], size);
  Fl_Text_Buffer* tb = new Fl_Text_Buffer(size);
  Fltk_tag t = FL_TEXT_BUFFER_TAG;
  Text_buffer* text_buffer = new Text_buffer(tb);
  Text_buffer_holder::put(tb, text_buffer);
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(text_buffer, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_text_buffer::fill(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHARP(argv[1]))
      {
	char ch = static_cast<char>(SCHEME_CHAR_VAL(argv[1]));
	int len = tb->length();
	char* s = new char[len + 1];
	memset(s, ch, len);
	s[len] = 0;
	tb->text(s);
	delete[] s;
	_ret_ = scheme_true;
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::add_modify_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  Text_buffer* text_buffer = Text_buffer_holder::get(tb);
  if (tb && text_buffer)
    {
      text_buffer->modify_cbs.push_back(argv[1]);
      text_buffer->cb_params.push_back(argv[2]);
      if (text_buffer->modify_cbs.size() == 1)
	{
	  tb->add_modify_callback(_modify_callback, 
				  reinterpret_cast<void*>(text_buffer));
	}
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::append(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  tb->append(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::appendfile(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  tb->appendfile(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::call_modify_callbacks(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->call_modify_callbacks();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::character(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int pos = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], pos))
	{
	  spark::Char c = tb->character(pos);
	  _ret_ = scheme_make_char(static_cast<mzchar>(c));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::character_width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  char c = static_cast<char>(i);
  int indent = 0;
  spark::Utils::int_from_scheme_long(argv[1], indent);
  int tab_dist = 0;
  spark::Utils::int_from_scheme_long(argv[2], tab_dist);
  i = 0;
  spark::Utils::int_from_scheme_long(argv[3], i);
  char null_subs_char = static_cast<char>(i);
  _ret_ = scheme_make_integer(Fl_Text_Buffer::character_width(c,
							      indent,
							      tab_dist,
							      null_subs_char));
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::clear_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      tb->clear_rectangular(start, end, r_start, r_end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::copy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  Fl_Text_Buffer* tb_from = _get_fl_text_buffer(argc, argv, 1);
  if (tb && tb_from)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[2], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[3], end);
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[4], pos);
      tb->copy(tb_from, start, end, pos);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::count_displayed_characters(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int target = 0;
      spark::Utils::int_from_scheme_long(argv[2], target);
      _ret_ = 
	scheme_make_integer(tb->count_displayed_characters(start, target));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::count_lines(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      _ret_ = 
	scheme_make_integer(tb->count_lines(start, end));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::expand_character(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 3)
    {
      Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
      if (tb)
	{
	  int pos = 0;
	  spark::Utils::int_from_scheme_long(argv[1], pos);
	  int indent = 0;
	  spark::Utils::int_from_scheme_long(argv[2], indent);
	  char out[5];
	  strcpy(out, "");
	  tb->expand_character(pos, indent, out);
	  _ret_ = scheme_make_utf8_string(out);
	}
    }
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[0], i);
      char c = static_cast<char>(i);
      int indent = 0;
      spark::Utils::int_from_scheme_long(argv[1], indent);
      int tab_dist = 0;
      spark::Utils::int_from_scheme_long(argv[2], tab_dist);
      i = 0;
      spark::Utils::int_from_scheme_long(argv[3], i);
      char nsc = static_cast<char>(i);
      char out[5];
      Fl_Text_Buffer::expand_character(c, indent, out, tab_dist, nsc);
      _ret_ = scheme_make_utf8_string(out);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::findchar_backward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      spark::Char c = 0;
      spark::Utils::char_from_scheme_char(argv[2], c);
      int fp = 0;
      tb->findchar_backward(start, c, &fp);
      _ret_ = scheme_make_integer(fp);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::findchar_forward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      spark::Char c = 0;
      spark::Utils::char_from_scheme_char(argv[2], c);
      int fp = 0;
      tb->findchar_forward(start, c, &fp);
      _ret_ = scheme_make_integer(fp);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::findchars_backward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  int fp = 0;
	  tb->findchars_backward(start, s.c_str(), &fp);
	  _ret_ = scheme_make_integer(fp);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::findchars_forward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  int fp = 0;
	  tb->findchars_forward(start, s.c_str(), &fp);
	  _ret_ = scheme_make_integer(fp);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::highlight(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      tb->highlight(start, end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::highlight_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      int end = 0;
      int is_rect = 0;
      int r_start = 0;
      int r_end = 0;
      tb->highlight_position(&start, &end, &is_rect, &r_start, &r_end);
      Scheme_Object** elems = new Scheme_Object*[5];
      {	
	Scheme_Object* obj_start = 0;
	Scheme_Object* obj_end = 0;
	Scheme_Object* obj_is_rect = 0;
	Scheme_Object* obj_r_start = 0;
	Scheme_Object* obj_r_end = 0;
	MZ_GC_DECL_REG(5);
	MZ_GC_VAR_IN_REG(0, obj_start);
	MZ_GC_VAR_IN_REG(1, obj_end);
	MZ_GC_VAR_IN_REG(2, obj_is_rect);
	MZ_GC_VAR_IN_REG(3, obj_r_start);
	MZ_GC_VAR_IN_REG(4, obj_r_end);
	MZ_GC_REG();
	obj_start = scheme_make_integer(start);
	obj_end = scheme_make_integer(end);
	obj_is_rect = is_rect ? scheme_true : scheme_false;
	obj_r_start = scheme_make_integer(r_start);
	obj_r_end = scheme_make_integer(r_end);
	elems[0] = obj_start;
	elems[1] = obj_end;
	elems[2] = obj_is_rect;
	elems[3] = obj_r_start;
	elems[4] = obj_r_end;
	_ret_ = scheme_build_list(5, elems);            
	MZ_GC_UNREG();
	delete[] elems;
      }      
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::highlight_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      tb->highlight_rectangular(start, end, r_start, r_end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::highlight_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      Fl_Text_Selection* s = tb->highlight_selection();
      Fltk_tag t = FL_SELECTION_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(s, tag);
	MZ_GC_REG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::highlight_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      const char* s = tb->highlight_text();
      if (s)
	{
	  _ret_ = scheme_make_utf8_string(s);
	  free(reinterpret_cast<void*>(const_cast<char*>(s)));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::insert_column(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[1], c);
      int s = 0;
      spark::Utils::int_from_scheme_long(argv[2], s);
      if (SCHEME_CHAR_STRINGP(argv[3]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  int i = 0;
	  int d = 0;
	  tb->insert_column(c, s, text.c_str(), &i, &d);
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {	
	    Scheme_Object* obj_i = 0;
	    Scheme_Object* obj_d = 0;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_i);
	    MZ_GC_VAR_IN_REG(1, obj_d);
	    MZ_GC_REG();
	    obj_i = scheme_make_integer(i);
	    obj_d = scheme_make_integer(d);
	    elems[0] = obj_i;
	    elems[1] = obj_d;
	    _ret_ = scheme_build_list(2, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  }      
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::insertfile(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string file = SCHEME_BYTE_STR_VAL(str);
	  int p = 0;
	  spark::Utils::int_from_scheme_long(argv[2], p);
	  int r = tb->insertfile(file.c_str(), p);
	  if (r == 0)
	    _ret_ = scheme_true;
	  else
	    _ret_ = scheme_make_integer(r);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::insert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  int p = 0;
	  spark::Utils::int_from_scheme_long(argv[1], p);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  tb->insert(p, text.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::length(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      _ret_ = scheme_make_integer(tb->length());
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::line_end(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int pos = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], pos))
	_ret_ = scheme_make_integer(tb->line_end(pos));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::line_start(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int pos = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], pos))
	_ret_ = scheme_make_integer(tb->line_start(pos));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::line_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int pos = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], pos))
	{
	  const char* text = tb->line_text(pos);
	  if (text)
	    {
	      _ret_ = scheme_make_utf8_string(text);
	      free(reinterpret_cast<void*>(const_cast<char*>(text)));
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::loadfile(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string file = SCHEME_BYTE_STR_VAL(str);
	  int r = tb->loadfile(file.c_str());
	  if (r == 0)
	    _ret_ = scheme_true;
	  else
	    _ret_ = scheme_make_integer(r);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::null_substitution_character(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      _ret_ = 
	scheme_make_char(static_cast<mzchar>(tb->null_substitution_character()));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::outputfile(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string file = SCHEME_BYTE_STR_VAL(str);
	  int start = 0;
	  spark::Utils::int_from_scheme_long(argv[2], start);
	  int end = 0;
	  spark::Utils::int_from_scheme_long(argv[3], end);
	  _ret_ = scheme_make_integer(tb->outputfile(file.c_str(), start, end));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::overlay_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[4]))
	{
	  int start = 0;
	  spark::Utils::int_from_scheme_long(argv[1], start);
	  int r_start = 0;
	  spark::Utils::int_from_scheme_long(argv[2], r_start);
	  int r_end = 0;
	  spark::Utils::int_from_scheme_long(argv[3], r_end);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  int i = 0 ;
	  int d = 0;
	  tb->overlay_rectangular(start, r_start, r_end, text.c_str(), &i, &d);
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {	
	    Scheme_Object* obj_i = 0;
	    Scheme_Object* obj_d = 0;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_i);
	    MZ_GC_VAR_IN_REG(1, obj_d);
	    MZ_GC_REG();
	    obj_i = scheme_make_integer(i);
	    obj_d = scheme_make_integer(d);
	    elems[0] = obj_i;
	    elems[1] = obj_d;
	    _ret_ = scheme_build_list(2, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  } 
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::primary_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      Fl_Text_Selection* s = tb->primary_selection();
      Fltk_tag t = FL_SELECTION_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(s, tag);
	MZ_GC_REG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::remove_modify_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  Text_buffer* text_buffer = Text_buffer_holder::get(tb);
  if (tb && text_buffer)
    {
      if (text_buffer->remove_callback(argv[1]))
	{
	  if (text_buffer->modify_cbs.size() == 0)
	    tb->remove_modify_callback(_modify_callback, 
				       reinterpret_cast<void*>(text_buffer));
	  _ret_ = scheme_true;
	}      
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::remove_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      tb->remove_rectangular(start, end, r_start, r_end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::remove(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      tb->remove(start, end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::remove_secondary_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->remove_secondary_selection();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::remove_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->remove_selection();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::replace_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      if (SCHEME_CHAR_STRINGP(argv[5]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[5]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  tb->replace_rectangular(start, end, r_start, r_end, text.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::replace(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      if (SCHEME_CHAR_STRINGP(argv[3]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  tb->replace(start, end, text.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::replace_secondary_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  tb->replace_secondary_selection(text.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::replace_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  tb->replace_selection(text.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::rewind_lines(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      tb->rewind_lines(start, end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::savefile(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string file = SCHEME_BYTE_STR_VAL(str);
	  int r = tb->savefile(file.c_str());
	  if (r == 0)
	    _ret_ = scheme_true;
	  else
	    _ret_ = scheme_make_integer(r);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::search_backward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  int start = 0;
	  spark::Utils::int_from_scheme_long(argv[1], start);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  int match_case = argv[3] == scheme_true ? 1 : 0;
	  int fp = -1;
	  tb->search_backward(start, text.c_str(), &fp, match_case);
	  _ret_ = scheme_make_integer(fp);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::search_forward(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  int start = 0;
	  spark::Utils::int_from_scheme_long(argv[1], start);
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string text = SCHEME_BYTE_STR_VAL(str);
	  int match_case = argv[3] == scheme_true ? 1 : 0;
	  int fp = -1;
	  tb->search_forward(start, text.c_str(), &fp, match_case);
	  _ret_ = scheme_make_integer(fp);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_selection_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      int end = 0;
      int is_rect = 0;
      int r_start = 0;
      int r_end = 0;
      tb->secondary_selection_position(&start, &end, &is_rect, &r_start, &r_end);
      Scheme_Object** elems = new Scheme_Object*[5];
      {	
	Scheme_Object* obj_start = 0;
	Scheme_Object* obj_end = 0;
	Scheme_Object* obj_is_rect = 0;
	Scheme_Object* obj_r_start = 0;
	Scheme_Object* obj_r_end = 0;
	MZ_GC_DECL_REG(5);
	MZ_GC_VAR_IN_REG(0, obj_start);
	MZ_GC_VAR_IN_REG(1, obj_end);
	MZ_GC_VAR_IN_REG(2, obj_is_rect);
	MZ_GC_VAR_IN_REG(3, obj_r_start);
	MZ_GC_VAR_IN_REG(4, obj_r_end);
	MZ_GC_REG();
	obj_start = scheme_make_integer(start);
	obj_end = scheme_make_integer(end);
	obj_is_rect = is_rect ? scheme_true : scheme_false;
	obj_r_start = scheme_make_integer(r_start);
	obj_r_end = scheme_make_integer(r_end);
	elems[0] = obj_start;
	elems[1] = obj_end;
	elems[2] = obj_is_rect;
	elems[3] = obj_r_start;
	elems[4] = obj_r_end;
	_ret_ = scheme_build_list(5, elems);            
	MZ_GC_UNREG();
	delete[] elems;
      }      
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      Fl_Text_Selection* s = tb->secondary_selection();
      Fltk_tag t = FL_SELECTION_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(s, tag);
	MZ_GC_REG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_selection_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      const char* s = tb->secondary_selection_text();
      if (s)
	{
	  _ret_ = scheme_make_utf8_string(s);
	  free(reinterpret_cast<void*>(const_cast<char*>(s)));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::select(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      tb->select(start, end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_select_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      tb->secondary_select_rectangular(start, end, r_start, r_end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_select(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      tb->secondary_select(start, end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::secondary_unselect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->secondary_unselect();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::selected(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      _ret_ = scheme_make_integer(tb->selected()) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::selection_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (argv[1] == scheme_true)
	{
	  int start = 0;
	  int end = 0;
	  int is_rect = 0;
	  int r_start = 0;
	  int r_end = 0;
	  tb->selection_position(&start, &end, &is_rect, &r_start, &r_end);
	  Scheme_Object** elems = new Scheme_Object*[5];
	  {	
	    Scheme_Object* obj_start = 0;
	    Scheme_Object* obj_end = 0;
	    Scheme_Object* obj_is_rect = 0;
	    Scheme_Object* obj_r_start = 0;
	    Scheme_Object* obj_r_end = 0;
	    MZ_GC_DECL_REG(5);
	    MZ_GC_VAR_IN_REG(0, obj_start);
	    MZ_GC_VAR_IN_REG(1, obj_end);
	    MZ_GC_VAR_IN_REG(2, obj_is_rect);
	    MZ_GC_VAR_IN_REG(3, obj_r_start);
	    MZ_GC_VAR_IN_REG(4, obj_r_end);
	    MZ_GC_REG();
	    obj_start = scheme_make_integer(start);
	    obj_end = scheme_make_integer(end);
	    obj_is_rect = is_rect ? scheme_true : scheme_false;
	    obj_r_start = scheme_make_integer(r_start);
	    obj_r_end = scheme_make_integer(r_end);
	    elems[0] = obj_start;
	    elems[1] = obj_end;
	    elems[2] = obj_is_rect;
	    elems[3] = obj_r_start;
	    elems[4] = obj_r_end;
	    _ret_ = scheme_build_list(5, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  }      
	}
      else
	{
	  int start = 0;
	  int end = 0;
	  tb->selection_position(&start, &end);
	  Scheme_Object** elems = new Scheme_Object*[2];
	  {	
	    Scheme_Object* obj_start = 0;
	    Scheme_Object* obj_end = 0;
	    MZ_GC_DECL_REG(2);
	    MZ_GC_VAR_IN_REG(0, obj_start);
	    MZ_GC_VAR_IN_REG(1, obj_end);
	    MZ_GC_REG();
	    obj_start = scheme_make_integer(start);
	    obj_end = scheme_make_integer(end);
	    elems[0] = obj_start;
	    elems[1] = obj_end;
	    _ret_ = scheme_build_list(2, elems);            
	    MZ_GC_UNREG();
	    delete[] elems;
	  }      
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::selection_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      const char* s = tb->selection_text();
      if (s)
	{
	  _ret_ = scheme_make_utf8_string(s);
	  free(reinterpret_cast<void*>(const_cast<char*>(s)));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::select_rectangular(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      tb->select_rectangular(start, end, r_start, r_end);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::skip_displayed_characters(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int n_chars = 0;
      spark::Utils::int_from_scheme_long(argv[2], n_chars);
      _ret_ = scheme_make_integer(tb->skip_displayed_characters(start, n_chars));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::skip_lines(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int n_lines = 0;
      spark::Utils::int_from_scheme_long(argv[2], n_lines);
      _ret_ = scheme_make_integer(tb->skip_lines(start, n_lines));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::substitute_null_characters(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string text = const_cast<char*>(SCHEME_BYTE_STR_VAL(str));
	  size_t length = text.length();
	  tb->substitute_null_characters(const_cast<char*>(text.c_str()),
					 static_cast<int>(length));
	  _ret_ = scheme_make_utf8_string(text.c_str());
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::unsubstitute_null_characters(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string text = const_cast<char*>(SCHEME_BYTE_STR_VAL(str));
	  tb->unsubstitute_null_characters(const_cast<char*>(text.c_str()));
	  _ret_ = scheme_make_utf8_string(text.c_str());
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::tab_distance(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (argc == 1)
	{
	  _ret_ = scheme_make_integer(tb->tab_distance());
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  tb->tab_distance(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::text_in_rectangle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      int r_start = 0;
      spark::Utils::int_from_scheme_long(argv[3], r_start);
      int r_end = 0;
      spark::Utils::int_from_scheme_long(argv[4], r_end);
      const char* text = tb->text_in_rectangle(start, end,
					       r_start, r_end);
      if (text)
	{
	  _ret_ = scheme_make_utf8_string(text);
	  free(reinterpret_cast<void*>(const_cast<char*>(text)));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::text_range(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int start = 0;
      spark::Utils::int_from_scheme_long(argv[1], start);
      int end = 0;
      spark::Utils::int_from_scheme_long(argv[2], end);
      const char* text = tb->text_range(start, end);
      if (text)
	{
	  _ret_ = scheme_make_utf8_string(text);
	  free(reinterpret_cast<void*>(const_cast<char*>(text)));
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      if (argc == 1)
	{
	  char* text = tb->text();
	  if (text)
	    {
	      _ret_ = scheme_make_utf8_string(text);
	      free(text);
	    }
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string text = const_cast<char*>(SCHEME_BYTE_STR_VAL(str));
	      tb->text(text.c_str());
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::unhighlight(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->unhighlight();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::unselect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      tb->unselect();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::word_end(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      int e = tb->word_end(i);
      _ret_ = scheme_make_integer(e);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_buffer::word_start(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Buffer* tb = _get_fl_text_buffer(argc, argv, 0);
  if (tb)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      int e = tb->word_start(i);
      _ret_ = scheme_make_integer(e);
    }

  DEFAULT_RET_FINISH;
}

Text_buffer*
spark_fltk_text_buffer::_get_text_buffer(int argc, 
					 Scheme_Object** argv, 
					 int index)
{
  spark_fltk::Fltk_tag tag = _get_tag(argc, argv, index);
  if (tag != FL_TEXT_BUFFER_TAG)
      {
	scheme_wrong_type("_get_text_buffer", "tag", 
			  index, argc, argv);
	return 0;
      }
  void* p = SCHEME_CPTR_VAL(argv[index]);
  if (!p)
    {
      scheme_wrong_type("_get_text_buffer", "cptr-val", 
			index, argc, argv);
      return 0;
    }
  return reinterpret_cast<Text_buffer*>(p);
}

Fl_Text_Buffer*
_get_fl_text_buffer(int argc, Scheme_Object** argv, int index)
{
 Text_buffer* text_buffer;
 if ((text_buffer = 
      spark_fltk_text_buffer::_get_text_buffer(argc, argv, index)) == 0)
   return 0;
  return text_buffer->fl_text_buffer;
}

void
_modify_callback(int pos, int num_inserted,
		 int num_deleted, int num_restyled,
		 const char* deleted_text,
		 void* cb_param)
{
  Text_buffer* text_buffer = reinterpret_cast<Text_buffer*>(cb_param);
  if (text_buffer)
    {
      const int arg_count = 6;
      size_t cb_count = text_buffer->modify_cbs.size();
      for (size_t i=0; i<cb_count; ++i)
	{
	  Scheme_Object* args[arg_count];
	  Scheme_Object* obj_pos = 0;
	  Scheme_Object* obj_ninserted = 0;
	  Scheme_Object* obj_ndeleted = 0;
	  Scheme_Object* obj_nrestyled = 0;
	  Scheme_Object* obj_deleted_text = 0;
	  MZ_GC_DECL_REG(5);
	  MZ_GC_VAR_IN_REG(0, obj_pos);
	  MZ_GC_VAR_IN_REG(1, obj_ninserted);
	  MZ_GC_VAR_IN_REG(2, obj_ndeleted);
	  MZ_GC_VAR_IN_REG(3, obj_nrestyled);
	  MZ_GC_VAR_IN_REG(4, obj_deleted_text);
	  MZ_GC_REG();	  
	  obj_pos = scheme_make_integer(pos);
	  obj_ninserted = scheme_make_integer(num_inserted);
	  obj_ndeleted = scheme_make_integer(num_deleted);
	  obj_nrestyled = scheme_make_integer(num_restyled);
	  if (deleted_text)
	    obj_deleted_text = scheme_make_utf8_string(deleted_text);
	  else
	    obj_deleted_text = scheme_null;
	  args[0] = obj_pos;
	  args[1] = obj_ninserted;
	  args[2] = obj_ndeleted;
	  args[3] = obj_nrestyled;
	  args[4] = obj_deleted_text;
	  args[5] = text_buffer->cb_params[i];	  
	  scheme_apply(text_buffer->modify_cbs[i],
		       arg_count, args);
	  MZ_GC_UNREG();
	}
    }
}

