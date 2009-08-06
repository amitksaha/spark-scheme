// MzScheme inetrface to the FLTK event functions.
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

#include "spark_fltk_common.h"
using namespace spark_fltk;

static Scheme_Object* _event_state_to_scheme_list(int s);

namespace spark_fltk_event
{
  static Scheme_Object* fl_event_button(int, Scheme_Object**);
  static Scheme_Object* fl_event_clicks(int, Scheme_Object**);
  static Scheme_Object* fl_event_dx(int, Scheme_Object**);
  static Scheme_Object* fl_event_dy(int, Scheme_Object**);
  static Scheme_Object* fl_event_inside(int, Scheme_Object**);
  static Scheme_Object* fl_event_is_click(int, Scheme_Object**);
  static Scheme_Object* fl_event_key(int, Scheme_Object**);
  static Scheme_Object* fl_event_length(int, Scheme_Object**);
  static Scheme_Object* fl_event_state(int, Scheme_Object**);
  static Scheme_Object* fl_event_text(int, Scheme_Object**);
  static Scheme_Object* fl_event_x(int, Scheme_Object**);
  static Scheme_Object* fl_event_x_root(int, Scheme_Object**);
  static Scheme_Object* fl_event_y(int, Scheme_Object**);
  static Scheme_Object* fl_event_y_root(int, Scheme_Object**);
  static Scheme_Object* fl_event_shift(int, Scheme_Object**);
  static Scheme_Object* fl_event_ctrl(int, Scheme_Object**);
  static Scheme_Object* fl_event_is_click(int, Scheme_Object**);
  static Scheme_Object* fl_event_original_key(int, Scheme_Object**);
  static Scheme_Object* fl_last_event(int, Scheme_Object**);
  static Scheme_Object* fl_get_key(int, Scheme_Object**);
  static Scheme_Object* fl_get_mouse(int, Scheme_Object**);
  static Scheme_Object* fl_test_shortcut(int, Scheme_Object**);
  static Scheme_Object* fl_copy(int, Scheme_Object**);
  static Scheme_Object* fl_dnd(int, Scheme_Object**);
}

spark::Status_code
spark_fltk::_add_event_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_event::fl_event_button, 
		  "fl-event-button", 0),
    new Procedure(spark_fltk_event::fl_event_clicks, 
		  "fl-event-clicks", 0, 1),
    new Procedure(spark_fltk_event::fl_event_dx, 
		  "fl-event-dx", 0),
    new Procedure(spark_fltk_event::fl_event_dy, 
		  "fl-event-dy", 0),
    new Procedure(spark_fltk_event::fl_event_inside, 
		  "fl-event-inside", 1, 4),
    new Procedure(spark_fltk_event::fl_event_is_click, 
		  "fl-event-is-click", 0, 1),
    new Procedure(spark_fltk_event::fl_event_key, 
		  "fl-event-key", 0, 1),
    new Procedure(spark_fltk_event::fl_event_length, 
		  "fl-event-length", 0),
    new Procedure(spark_fltk_event::fl_event_original_key, 
		  "fl-event-original-key", 0),
    new Procedure(spark_fltk_event::fl_event_state, 
		  "fl-event-state", 0, 1),
    new Procedure(spark_fltk_event::fl_event_x, 
		  "fl-event-x", 0),
    new Procedure(spark_fltk_event::fl_event_x_root, 
		  "fl-event-x-root", 0),
    new Procedure(spark_fltk_event::fl_event_y, 
		  "fl-event-y", 0),
    new Procedure(spark_fltk_event::fl_event_y_root, 
		  "fl-event-y-root", 0),
    new Procedure(spark_fltk_event::fl_event_text, 
		  "fl-event-text", 0),
    new Procedure(spark_fltk_event::fl_event_shift, 
		  "fl-event-shift", 0),
    new Procedure(spark_fltk_event::fl_event_ctrl, 
		  "fl-event-ctrl", 0),
    new Procedure(spark_fltk_event::fl_last_event, 
		  "fl-last-event", 0),
    new Procedure(spark_fltk_event::fl_get_key, 
		  "fl-get-key", 0),
    new Procedure(spark_fltk_event::fl_get_mouse, 
		  "fl-get-mouse", 0),
    new Procedure(spark_fltk_event::fl_test_shortcut, 
		  "fl-test-shortcut", 1),
    new Procedure(spark_fltk_event::fl_copy, 
		  "fl-copy", 2),
    new Procedure(spark_fltk_event::fl_dnd, 
		  "fl-dnd", 0),

    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_event_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    // mouse events
    Constant("FL-PUSH", FL_PUSH),
    Constant("FL-DRAG", FL_DRAG),
    Constant("FL-RELEASE", FL_RELEASE),
    Constant("FL-MOVE", FL_MOVE),
    Constant("FL-MOUSEWHEEL", FL_MOUSEWHEEL),
    // focus events
    Constant("FL-ENTER", FL_ENTER),
    Constant("FL-LEAVE", FL_LEAVE),
    Constant("FL-FOCUS", FL_FOCUS),
    Constant("FL-UNFOCUS", FL_UNFOCUS),
    // Keyboard events
    Constant("FL-KEYUP", FL_KEYUP),
    Constant("FL-KEYDOWN", FL_KEYDOWN),
    Constant("FL-SHORTCUT", FL_SHORTCUT),
    // Widget events
    Constant("FL-DEACTIVATE", FL_DEACTIVATE),
    Constant("FL-ACTIVATE", FL_ACTIVATE),
    Constant("FL-HIDE", FL_HIDE),
    Constant("FL-SHOW", FL_SHOW),
    // Clipboard event
    Constant("FL-PASTE", FL_PASTE),
    Constant("FL-SELECTIONCLEAR", FL_SELECTIONCLEAR),
    // Drag-n-Drop events
    Constant("FL-DND-ENTER", FL_DND_ENTER),
    Constant("FL-DND-DRAG", FL_DND_DRAG),
    Constant("FL-DND-LEAVE", FL_DND_LEAVE),
    Constant("FL-DND-RELEASE", FL_DND_RELEASE),
    // event button
    Constant("FL-LEFT-MOUSE", FL_LEFT_MOUSE),
    Constant("FL-MIDDLE-MOUSE", FL_MIDDLE_MOUSE),
    Constant("FL-RIGHT-MOUSE", FL_RIGHT_MOUSE),
    // event state
    Constant("FL-SHIFT", FL_SHIFT),
    Constant("FL-CAPS-LOCK", FL_CAPS_LOCK),
    Constant("FL-CTRL", FL_CTRL),
    Constant("FL-ALT", FL_ALT),
    Constant("FL-NUM-LOCK", FL_NUM_LOCK),
    Constant("FL-META", FL_META),
    Constant("FL-SCROLL-LOCK", FL_SCROLL_LOCK),
    Constant("FL-BUTTON1", FL_BUTTON1),
    Constant("FL-BUTTON2", FL_BUTTON2),
    Constant("FL-BUTTON3", FL_BUTTON3),
    Constant("FL-BUTTONS", FL_BUTTONS),
    Constant("FL-COMMAND", FL_COMMAND),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

Scheme_Object* 
spark_fltk_event::fl_event_button(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_button());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_clicks(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    _ret_ = scheme_make_integer(Fl::event_clicks());
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[0], i);
      Fl::event_clicks(i);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_dx(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_dx());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_dy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_dy());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_inside(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 1)
    {
      Fl_Widget* widget;
      if ((widget = _get_widget(argc, argv, 0)) == 0)
	{
	  DEFAULT_RET_FINISH;
	}
      _ret_ = (Fl::event_inside(widget) > 0) ? scheme_true : scheme_false;
    }
  else
    {
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[0], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[1], y);
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[2], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[3], h);
      _ret_ = (Fl::event_inside(x, y, w, h) > 0) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_is_click(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    _ret_ = (Fl::event_is_click() > 0) ? scheme_true : scheme_false;
  else
    {
      Fl::event_is_click(0);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_key(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    _ret_ = scheme_make_integer(Fl::event_key());
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[0], i);
      _ret_ = Fl::event_key(i) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_length(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_length());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_state(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    _ret_ = _event_state_to_scheme_list(Fl::event_state());
  else
    {
      int i = spark_fltk::intlist_to_flag(argv[0]);
      Fl::event_state(i);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  const char* s = Fl::event_text();
  if (s)
    _ret_ = scheme_make_utf8_string(s);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_x(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_x());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_x_root(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_x_root());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_y(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_y());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_y_root(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_y_root());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_shift(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = (Fl::event_shift()) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_ctrl(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = (Fl::event_ctrl()) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_event_original_key(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl::event_original_key());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_last_event(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = spark_fltk::int_to_event_symbol(Fl::event());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_get_key(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = scheme_make_integer(Fl::get_key(i));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_get_mouse(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  const int count = 2;
  Scheme_Object** elems = new Scheme_Object*[count];
  {
    MZ_GC_DECL_REG(count);
    for (int i=0; i<count; ++i)
      {
	elems[i] = NULL;
	MZ_GC_VAR_IN_REG(i, elems[i]);
      }
    MZ_GC_REG();
    int x = 0;
    int y = 0;
    Fl::get_mouse(x, y);
    elems[0] = scheme_make_integer(x);
    elems[1] = scheme_make_integer(y);
    MZ_GC_UNREG();
    _ret_ = scheme_build_list(count, elems);
    delete[] elems;
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_test_shortcut(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = (Fl::test_shortcut(i)) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_dnd(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = (Fl::dnd()) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_event::fl_copy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-copy", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string stuff = SCHEME_BYTE_STR_VAL(str);
  int clipboard = 0;
  spark::Utils::int_from_scheme_long(argv[1], clipboard);
  Fl::copy(stuff.c_str(), stuff.length(), clipboard);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
_event_state_to_scheme_list(int s)
{
  const int count = 12;
  Scheme_Object** elems = new Scheme_Object*[count];
  MZ_GC_DECL_REG(count);
  for (int i=0; i<count; ++i)
    {
      elems[i] = NULL;
      MZ_GC_VAR_IN_REG(i, elems[i]);
      elems[i] = scheme_null;
    }
  MZ_GC_REG();
  const int states[count] = {FL_SHIFT, FL_CAPS_LOCK,
			     FL_CTRL, FL_ALT, FL_NUM_LOCK,
			     FL_META, FL_SCROLL_LOCK,
			     FL_BUTTON1, FL_BUTTON2,
			     FL_BUTTON3, FL_BUTTONS,
			     FL_COMMAND};
  for (int i=0; i<count; ++i)
    {
      if ((s & states[i]) == states[i])
	elems[i] = scheme_make_integer(states[i]);
    }
  MZ_GC_UNREG();
  Scheme_Object* ret = scheme_build_list(count, elems);
  delete[] elems;
  return ret;
}

Scheme_Object* 
spark_fltk::int_to_event_symbol(int event)
{
  switch (event)
    {
    case FL_PUSH:
      return scheme_intern_symbol("mouse-push");
    case FL_DRAG:
      return scheme_intern_symbol("mouse-drag");
    case FL_RELEASE:
      return scheme_intern_symbol("mouse-release");
    case FL_MOVE:
      return scheme_intern_symbol("mouse-move");
    case FL_MOUSEWHEEL:
      return scheme_intern_symbol("mouse-wheel");
    case FL_ENTER:
      return scheme_intern_symbol("enter");
    case FL_LEAVE:
      return scheme_intern_symbol("leave");
    case FL_FOCUS:
      return scheme_intern_symbol("focus");
    case FL_UNFOCUS:
      return scheme_intern_symbol("unfocus");
    case FL_KEYUP:
      return scheme_intern_symbol("key-up");
    case FL_KEYDOWN:
      return scheme_intern_symbol("key-down");
    case FL_SHORTCUT:
      return scheme_intern_symbol("shortcut");
    case FL_DEACTIVATE:
      return scheme_intern_symbol("deactivate");
    case FL_ACTIVATE:
      return scheme_intern_symbol("activate");
    case FL_HIDE:
      return scheme_intern_symbol("hide");
    case FL_SHOW:
      return scheme_intern_symbol("show");
    case FL_PASTE:
      return scheme_intern_symbol("paste");
    case FL_SELECTIONCLEAR:
      return scheme_intern_symbol("selection-clear");
    case FL_DND_ENTER:
      return scheme_intern_symbol("dnd-enter");
    case FL_DND_DRAG:
      return scheme_intern_symbol("dnd-drag");
    case FL_DND_LEAVE:
      return scheme_intern_symbol("dnd-leave");
    case FL_DND_RELEASE:
      return scheme_intern_symbol("dnd-release");
    }
  return scheme_null;
}
