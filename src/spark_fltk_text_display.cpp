// MzScheme inetrface to the FLTK Text_Display widget.
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

#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Text_Editor.H>
#include "spark_fltk_text_buffer.h"
using namespace spark_fltk;
using namespace spark_fltk_text_buffer;

namespace spark_fltk_text_display
{
  // text display
  static Scheme_Object* fl_text_display(int, Scheme_Object**);
  static Scheme_Object* buffer(int, Scheme_Object**);
  static Scheme_Object* cursor_color(int, Scheme_Object**);
  static Scheme_Object* cursor_style(int, Scheme_Object**);
  static Scheme_Object* hide_cursor(int, Scheme_Object**);
  static Scheme_Object* highlight_data(int, Scheme_Object**);
  static Scheme_Object* in_selection(int, Scheme_Object**);
  static Scheme_Object* insert(int, Scheme_Object**);
  static Scheme_Object* insert_position(int, Scheme_Object**);
  static Scheme_Object* move_down(int, Scheme_Object**);
  static Scheme_Object* move_left(int, Scheme_Object**);
  static Scheme_Object* move_right(int, Scheme_Object**);
  static Scheme_Object* move_up(int, Scheme_Object**);
  static Scheme_Object* next_word(int, Scheme_Object**);
  static Scheme_Object* overstrike(int, Scheme_Object**);
  static Scheme_Object* position_style(int, Scheme_Object**);
  static Scheme_Object* previous_word(int, Scheme_Object**);
  static Scheme_Object* redisplay_range(int, Scheme_Object**);
  static Scheme_Object* scrollbar_align(int, Scheme_Object**);
  static Scheme_Object* scrollbar_width(int, Scheme_Object**);
  static Scheme_Object* scroll(int, Scheme_Object**);
  static Scheme_Object* show_cursor(int, Scheme_Object**);
  static Scheme_Object* show_insert_position(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* word_end(int, Scheme_Object**);
  static Scheme_Object* word_start(int, Scheme_Object**);
  static Scheme_Object* wrap_mode(int, Scheme_Object**);
  // text_editor
  static Scheme_Object* fl_text_editor(int, Scheme_Object**);
  static Scheme_Object* add_key_binding(int, Scheme_Object**);
  static Scheme_Object* remove_key_binding(int, Scheme_Object**);
  static Scheme_Object* kf_key_event(int, Scheme_Object**);
} // namespace spark_fltk_text_display

spark::Status_code
spark_fltk::_add_text_display_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = {
    new Procedure(spark_fltk_text_display::fl_text_display,
		  "fl-text-display", 5),
    new Procedure(spark_fltk_text_display::buffer, "text-display-buffer", 1, 2),
    new Procedure(spark_fltk_text_display::cursor_color, "cursor-color", 1, 2),
    new Procedure(spark_fltk_text_display::cursor_style, "cursor-style", 2),
    new Procedure(spark_fltk_text_display::hide_cursor, "hide-cursor", 1),
    new Procedure(spark_fltk_text_display::highlight_data, "highlight-data", 5),
    new Procedure(spark_fltk_text_display::in_selection, "in-selection", 3),
    new Procedure(spark_fltk_text_display::insert, "insert", 2),
    new Procedure(spark_fltk_text_display::insert_position,
		  "insert-position", 1, 2),
    new Procedure(spark_fltk_text_display::move_down, "move-down", 1),
    new Procedure(spark_fltk_text_display::move_left, "move-left", 1),
    new Procedure(spark_fltk_text_display::move_right, "move-right", 1),
    new Procedure(spark_fltk_text_display::move_up, "move-up", 1),
    new Procedure(spark_fltk_text_display::next_word, "next-word", 1),
    new Procedure(spark_fltk_text_display::overstrike, "overstrike", 2),
    new Procedure(spark_fltk_text_display::position_style, "position-style", 5),
    new Procedure(spark_fltk_text_display::previous_word, "previous-word", 1),
    new Procedure(spark_fltk_text_display::redisplay_range,
		  "redisplay-range", 3),
    new Procedure(spark_fltk_text_display::scrollbar_align,
		  "scrollbar-align", 1, 2),
    new Procedure(spark_fltk_text_display::scrollbar_width,
		  "scrollbar-width", 1, 2),
    new Procedure(spark_fltk_text_display::scroll, "scroll", 3),
    new Procedure(spark_fltk_text_display::show_cursor, "show-cursor", 1),
    new Procedure(spark_fltk_text_display::show_insert_position,
		  "show-insert-position", 1),
    new Procedure(spark_fltk_text_display::textcolor,
		  "text-display-text-color", 1, 2),
    new Procedure(spark_fltk_text_display::textfont,
		  "text-display-text-font", 1, 2),
    new Procedure(spark_fltk_text_display::textsize,
		  "text-display-text-size", 1, 2),
    new Procedure(spark_fltk_text_display::word_end, "word-end", 2),
    new Procedure(spark_fltk_text_display::word_start, "word-start", 2),
    new Procedure(spark_fltk_text_display::wrap_mode, "wrap-mode", 3),
    // text_editor
    new Procedure(spark_fltk_text_display::fl_text_editor,
		  "fl-text-editor", 5),
    new Procedure(spark_fltk_text_display::add_key_binding,
		  "add-key-binding", 4),
    new Procedure(spark_fltk_text_display::remove_key_binding,
		  "remove-key-binding", 3),
    new Procedure(spark_fltk_text_display::kf_key_event,
		  "kf-key-event", 3),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_text_display_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = {
    Constant("NORMAL-CURSOR", Fl_Text_Display::NORMAL_CURSOR),
    Constant("CARET-CURSOR", Fl_Text_Display::CARET_CURSOR),
    Constant("DIM-CURSOR", Fl_Text_Display::DIM_CURSOR),
    Constant("BLOCK-CURSOR", Fl_Text_Display::BLOCK_CURSOR),
    Constant("HEAVY-CURSOR", Fl_Text_Display::HEAVY_CURSOR),
    Constant("ANY-STATE", FL_TEXT_EDITOR_ANY_STATE),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

static Fl_Text_Display* _get_fl_text_display(int argc,
					     Scheme_Object** argv,
					     int index);
static Fl_Text_Editor* _get_fl_text_editor(int argc,
					    Scheme_Object** argv,
					    int index);

struct Key_binding
{
  int key;
  int state;
  Scheme_Object* cb;

  Key_binding()
    : key(0), state(0), cb(0)
  { }
  Key_binding(int k, int s, Scheme_Object* c)
    : key(k), state(s), cb(c)
  { }
}; // struct Key_binding

typedef std::vector<Key_binding> Key_binding_list;

class Text_display_widget : public Widget
{
public:
  Key_binding_list kbs;

  Text_display_widget()
    : Widget()
  {
    other_callbacks[CBT_STYLE_UNFINISHED] = scheme_null;
    other_arguments[CBT_STYLE_UNFINISHED] = scheme_null;
  }

  void remove_key_binding(int key, int state)
  {
    Key_binding_list::iterator it_curr = kbs.begin();
    Key_binding_list::iterator it_end = kbs.end();
    while (it_curr != it_end)
      {
	Key_binding kb = *it_curr;
	if (kb.key == key && kb.state == state)
	  {
	    kbs.erase(it_curr);
	    break;
	  }
	it_curr++;
      }
  }
}; // class Text_display_widget : public Widget

Scheme_Object*
spark_fltk_text_display::fl_text_display(int argc, Scheme_Object** argv)
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
  Fl_Text_Display* td = new Fl_Text_Display(x, y, w, h);
  if (title.length() > 0)
    td->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new Text_display_widget;
  td->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(td, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::buffer(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  Fl_Text_Buffer* tb = td->buffer();
	  if (tb)
	    {
	      Text_buffer* text_buffer = Text_buffer_holder::get(tb);
	      if (text_buffer)
		{
		  Scheme_Object* tag = 0;
		  MZ_GC_DECL_REG(1);
		  MZ_GC_VAR_IN_REG(0, tag);
		  MZ_GC_REG();
		  tag = scheme_make_integer(FL_TEXT_BUFFER_TAG);
		  _ret_ = scheme_make_cptr(text_buffer, tag);
		  MZ_GC_UNREG();
		}
	    }
	}
      else
	{
	  Text_buffer* text_buffer =
	    spark_fltk_text_buffer::_get_text_buffer(argc,
						     argv,
						     1);
	  if (text_buffer)
	    {
	      td->buffer(text_buffer->fl_text_buffer);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::cursor_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  int c = static_cast<int>(td->cursor_color());
	  _ret_ = scheme_make_integer(c);
	}
      else
	{
	  int c = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], c))
	    {
	      td->cursor_color(static_cast<Fl_Color>(c));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::cursor_style(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int c = 0;
      if (spark::Utils::int_from_scheme_long(argv[1], c))
	{
	  td->cursor_style(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::hide_cursor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      td->hide_cursor();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

typedef std::vector<Fl_Text_Display::Style_Table_Entry> Ste_list;

static bool _create_ste_list(int argc, Scheme_Object** argv,
			     int index, Ste_list& out);
static void _style_unfinished_cb(int a, void* p);

// Arguments:
// 0. widget
// 1. A list of styles in the following format:
// ((color font size) (color font size) ...)
// 2. Style unfinished callback or null. This has the following
// signature:
// (define (cb integer object))
// 3. Object that is the second argument of the callback or null
Scheme_Object*
spark_fltk_text_display::highlight_data(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  Widget* tdw = reinterpret_cast<Widget*>(td->argument());
  Text_display_widget* widget = 0;
  if (tdw)
    widget = dynamic_cast<Text_display_widget*>(tdw);
  if (td && widget)
    {
      spark_fltk_text_buffer::Text_buffer* style_buf_tb
	= spark_fltk_text_buffer::_get_text_buffer(argc, argv, 1);
      if (!style_buf_tb)
	{
	  DEFAULT_RET_FINISH;
	}
      Fl_Text_Buffer* style_buf = style_buf_tb->fl_text_buffer;
      if (!style_buf)
	{
	  DEFAULT_RET_FINISH;
	}
      Ste_list stes;
      if (!_create_ste_list(argc, argv, 2, stes))
	{
	  DEFAULT_RET_FINISH;
	}
      size_t sz = stes.size();
      if (!sz)
	{
	  DEFAULT_RET_FINISH;
	}
      Fl_Text_Display::Style_Table_Entry* st_entries =
	new Fl_Text_Display::Style_Table_Entry[sz];
      for (size_t i=0; i<sz; ++i)
	st_entries[i] = stes[i];
      td->highlight_data(style_buf, st_entries,
			 static_cast<int>(sz), 'A',
			 _style_unfinished_cb,
			 reinterpret_cast<void*>(widget));
      widget->other_callbacks[CBT_STYLE_UNFINISHED] = argv[3];
      widget->other_arguments[CBT_STYLE_UNFINISHED] = argv[4];
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::in_selection(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int x = 0;
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      spark::Utils::int_from_scheme_long(argv[2], y);
      _ret_ = td->in_selection(x, y) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::insert(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  td->insert(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::insert_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  int i = td->insert_position();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      td->insert_position(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::move_down(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int i = td->move_down();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::move_left(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int i = td->move_left();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::move_right(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int i = td->move_right();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::move_up(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int i = td->move_up();
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::next_word(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      td->next_word();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::overstrike(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string s = SCHEME_BYTE_STR_VAL(str);
	  td->overstrike(s.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::position_style(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int line_start_pos = 0;
      int line_len = 0;
      int line_index = 0;
      int disp_index = 0;
      spark::Utils::int_from_scheme_long(argv[1], line_start_pos);
      spark::Utils::int_from_scheme_long(argv[2], line_len);
      spark::Utils::int_from_scheme_long(argv[3], line_index);
      spark::Utils::int_from_scheme_long(argv[4], disp_index);
      int r = td->position_style(line_start_pos, line_len,
				 line_index, disp_index);
      _ret_ = scheme_make_integer(r);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::previous_word(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      td->previous_word();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::redisplay_range(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
     int start = 0;
     int end = 0;
     spark::Utils::int_from_scheme_long(argv[1], start);
     spark::Utils::int_from_scheme_long(argv[2], end);
     td->redisplay_range(start, end);
     _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::scrollbar_align(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  int a = static_cast<int>(td->scrollbar_align());
	  _ret_ = scheme_make_integer(a);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Align a = static_cast<Fl_Align>(i);
	      td->scrollbar_align(a);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::scrollbar_width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  int a = td->scrollbar_width();
	  _ret_ = scheme_make_integer(a);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      td->scrollbar_width(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::scroll(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int top_line_num = 0;
      int horiz_offset = 0;
      spark::Utils::int_from_scheme_long(argv[1], top_line_num);
      spark::Utils::int_from_scheme_long(argv[2], horiz_offset);
      td->scroll(top_line_num, horiz_offset);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::show_cursor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      td->show_cursor();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::show_insert_position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      td->show_insert_position();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  Fl_Color c = td->textcolor();
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
	      td->textcolor(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  uchar c = td->textfont();
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
	      td->textfont(c);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      if (argc == 1)
	{
	  uchar c = td->textsize();
	  long i = static_cast<long>(c);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  long i = 0;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    {
	      td->textsize(static_cast<uchar>(i));
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::word_end(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[1], pos);
      td->word_end(pos);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::word_start(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[1], pos);
      td->word_start(pos);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::wrap_mode(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Display* td = _get_fl_text_display(argc, argv, 0);
  if (td)
    {
      int pos = 0;
      spark::Utils::int_from_scheme_long(argv[2], pos);
      if (argv[1] == scheme_true)
	td->wrap_mode(1, pos);
      else
	td->wrap_mode(0, pos);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::fl_text_editor(int argc, Scheme_Object** argv)
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
  Fl_Text_Editor* td = new Fl_Text_Editor(x, y, w, h, title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new Text_display_widget;
  td->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    _ret_ = scheme_make_cptr(td, tag);
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

static int _default_kb_cb(int key, Fl_Text_Editor* e);

Scheme_Object*
spark_fltk_text_display::add_key_binding(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Editor* te = _get_fl_text_editor(argc, argv, 0);
  Text_display_widget* td = 0;
  Widget* tdw = reinterpret_cast<Widget*>(te->argument());
  if (tdw)
    td = dynamic_cast<Text_display_widget*>(tdw);
  if (te && td)
    {
      int key = 0;
      spark::Utils::int_from_scheme_long(argv[1], key);
      int state = 0;
      spark::Utils::int_from_scheme_long(argv[2], state);
      Key_binding kb(key, state, argv[3]);
      td->kbs.push_back(kb);
      te->add_key_binding(key, state, _default_kb_cb);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::remove_key_binding(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Editor* te = _get_fl_text_editor(argc, argv, 0);
  Text_display_widget* td = 0;
  Widget* tdw = reinterpret_cast<Widget*>(te->argument());
  if (tdw)
    td = dynamic_cast<Text_display_widget*>(tdw);
  if (te && td)
    {
      int key = 0;
      spark::Utils::int_from_scheme_long(argv[1], key);
      int state = 0;
      spark::Utils::int_from_scheme_long(argv[2], state);
      td->remove_key_binding(key, state);
      te->remove_key_binding(key, state);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_text_display::kf_key_event(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Text_Editor* te = _get_fl_text_editor(argc, argv, 0);
  if (te)
    {
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[1], c);
      if (!SCHEME_SYMBOLP(argv[2]))
	scheme_wrong_type("kf-key-event", "symbol", 2, argc, argv);
      std::string s = SCHEME_SYM_VAL(argv[2]);
      int i = -1;
      if (s == "backspace")
	i = Fl_Text_Editor::kf_backspace(c, te);
      else if (s == "ignore")
	i = Fl_Text_Editor::kf_ignore(c, te);
      else if (s == "default")
	i = Fl_Text_Editor::kf_default(c, te);
      else if (s == "enter")
	i = Fl_Text_Editor::kf_enter(c, te);
      else if (s == "move")
	i = Fl_Text_Editor::kf_move(c, te);
      else if (s == "shift-move")
	i = Fl_Text_Editor::kf_shift_move(c, te);
      else if (s == "ctrl-move")
	i = Fl_Text_Editor::kf_ctrl_move(c, te);
      else if (s == "c-s-move")
	i = Fl_Text_Editor::kf_c_s_move(c, te);
      else if (s == "home")
	i = Fl_Text_Editor::kf_home(c, te);
      else if (s == "end")
	i = Fl_Text_Editor::kf_end(c, te);
      else if (s == "left")
	i = Fl_Text_Editor::kf_left(c, te);
      else if (s == "up")
	i = Fl_Text_Editor::kf_up(c, te);
      else if (s == "right")
	i = Fl_Text_Editor::kf_right(c, te);
      else if (s == "down")
	i = Fl_Text_Editor::kf_down(c, te);
      else if (s == "page-up")
	i = Fl_Text_Editor::kf_page_up(c, te);
      else if (s == "page-down")
	i = Fl_Text_Editor::kf_page_down(c, te);
      else if (s == "insert")
	i = Fl_Text_Editor::kf_insert(c, te);
      else if (s == "delete")
	i = Fl_Text_Editor::kf_delete(c, te);
      else if (s == "copy")
	i = Fl_Text_Editor::kf_copy(c, te);
      else if (s == "cut")
	i = Fl_Text_Editor::kf_cut(c, te);
      else if (s == "paste")
	i = Fl_Text_Editor::kf_paste(c, te);
      else if (s == "select-all")
	i = Fl_Text_Editor::kf_select_all(c, te);
      else if (s == "undo")
	i = Fl_Text_Editor::kf_undo(c, te);
      _ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Fl_Text_Display*
_get_fl_text_display(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Text_Display*>(widget);
}

Fl_Text_Editor*
_get_fl_text_editor(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Text_Editor*>(widget);
}

bool
_create_ste_list(int argc, Scheme_Object** argv,
		    int index, Ste_list& out)
{
  if (argv[index] == scheme_null)
    return false;
  if (!SCHEME_LISTP(argv[index]))
    {
      scheme_wrong_type("_create_ste_list",
			"list", index,
			argc, argv);
      return false;
    }
  Scheme_Object* lst = argv[index];
  while (lst)
    {
      Scheme_Object* obj = SCHEME_CAR(lst);
      if (!SCHEME_LISTP(obj))
	{
	  scheme_wrong_type("_create_ste_list",
			    "sub-list", index,
			    argc, argv);
	  return false;
	}
      if (scheme_list_length(obj) != 3)
	{
	  scheme_wrong_type("_create_ste_list",
			    "sub-list-length", index,
			    argc, argv);
	  return false;
	}
      Scheme_Object* obj_color = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);
      Scheme_Object* obj_font = SCHEME_CAR(obj);
      obj = SCHEME_CDR(obj);
      Scheme_Object* obj_size = SCHEME_CAR(obj);
      Fl_Text_Display::Style_Table_Entry ste;
      int i = 0;
      if (spark::Utils::int_from_scheme_long(obj_color, i))
	ste.color = static_cast<Fl_Color>(i);
      i = 0;
      if (spark::Utils::int_from_scheme_long(obj_font, i))
	ste.font = static_cast<Fl_Font>(i);
      i = 0;
      if (spark::Utils::int_from_scheme_long(obj_size, i))
	ste.size = i;
      out.push_back(ste);

      lst = SCHEME_CDR(lst);
      if (lst == scheme_null)
	break;
    }
  return true;
}

void
_style_unfinished_cb(int a, void* p)
{
  Text_display_widget* widget = reinterpret_cast<Text_display_widget*>(p);
  if (widget)
    {
      Scheme_Object* cb = widget->other_callbacks[CBT_STYLE_UNFINISHED];
      if (cb != scheme_null)
	{
	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  Scheme_Object* obj_i = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, obj_i);
	  obj_i = scheme_make_integer(a);
	  args[0] = obj_i;
	  args[1] = widget->other_arguments[CBT_STYLE_UNFINISHED];
	  MZ_GC_REG();
	  scheme_apply(cb, arg_count, args);
	  MZ_GC_UNREG();
	}
    }
}

int
_default_kb_cb(int key, Fl_Text_Editor* e)
{
  Text_display_widget* td = 0;
  Widget* tdw = reinterpret_cast<Widget*>(e->argument());
  if (tdw)
    td = dynamic_cast<Text_display_widget*>(tdw);
  if (td)
    {
      size_t sz = td->kbs.size();
      for (size_t i=0; i<sz; ++i)
	{
	  Key_binding kb = td->kbs[i];
	  if (kb.key == key)
	    {
	      if (!kb.cb)
		continue;
	      if (kb.cb != scheme_null)
		{
		  const int arg_count = 2;
		  Scheme_Object* args[arg_count];
		  Scheme_Object* obj_i = 0;
		  Scheme_Object* obj_e = 0;
		  Scheme_Object* tag = 0;
		  MZ_GC_DECL_REG(3);
		  MZ_GC_VAR_IN_REG(0, obj_i);
		  MZ_GC_VAR_IN_REG(1, obj_e);
		  MZ_GC_VAR_IN_REG(3, tag);
		  obj_i = scheme_make_integer(key);
		  args[0] = obj_i;
		  tag = scheme_make_integer(FL_WIDGET_TAG);
		  obj_e = scheme_make_cptr(td, tag);
		  args[1] = obj_e;
		  MZ_GC_REG();
		  Scheme_Object* r = scheme_apply(kb.cb, arg_count, args);
		  int i = 0;
		  spark::Utils::int_from_scheme_long(r, i);
		  MZ_GC_UNREG();
		  return i;
		}
	    }
	}
    }
  return 0;
}

