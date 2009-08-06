// Provides subclasses of Fl_Widget and Fl_Group that can be
// customized from scheme.
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

#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Browser.H>
#include <FL/Fl_Check_Browser.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Single_Window.H>
#include <FL/Fl_Gl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Check_Button.H>
#include <FL/Fl_Light_Button.H>
#include <FL/Fl_Repeat_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Round_Button.H>
#include <FL/Fl_Toggle_Button.H>
#include <FL/Fl_Chart.H>
#include <FL/Fl_Clock.H>
#include <FL/Fl_Progress.H>
#include <FL/Fl_Color_Chooser.H>
#include <FL/Fl_Input_Choice.H>
#include <FL/Fl_Pack.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Spinner.H>
#include <FL/Fl_Tile.H>
#include <FL/Fl_Choice.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Menu_Button.H>
#include <FL/Fl_Positioner.H>
#include <FL/Fl_Adjuster.H>
#include <FL/Fl_Counter.H>
#include <FL/Fl_Dial.H>
#include <FL/Fl_Roller.H>
#include <FL/Fl_Slider.H>
#include <FL/Fl_Scrollbar.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/Fl_Value_Input.H>
#include <FL/Fl_Value_Output.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Text_Display.H>
#include <FL/Fl_Text_Editor.H>
#include "spark_fltk.h"
#include "spark_fltk_common.h"
#include "spark_fltk_widget.h"
using namespace spark_fltk;

// exported function signatures
namespace spark_fltk_widget
{
  static Scheme_Object* new_custom_widget(int, Scheme_Object**);
  static Scheme_Object* draw_callback(int, Scheme_Object**);
  static Scheme_Object* handle_event_callback(int, Scheme_Object**);
} // namespace spark_fltk_widget

spark::Status_code
spark_fltk::_add_widget_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_widget::new_custom_widget, 
		  "new-widget", 6, 10),
    new Procedure(spark_fltk_widget::draw_callback, 
		  "draw-callback", 2, 3),
    new Procedure(spark_fltk_widget::handle_event_callback, 
		  "handle-event-callback", 2, 3),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

void
spark_fltk_widget::_custom_widget_draw(Fl_Widget* w)
{
  Widget* widget = reinterpret_cast<Widget*>(w->argument());
  if (widget)
    {
      Scheme_Object* draw_callback = widget->other_callbacks[CBT_DRAW];
      if (draw_callback != scheme_null)
	{
	  const int arg_count = 2;
	  Scheme_Object** args = new Scheme_Object*[arg_count];
	  args[0] = NULL;
	  args[2] = NULL;
	  MZ_GC_DECL_REG(2);
	  MZ_GC_VAR_IN_REG(0, args[0]);
	  MZ_GC_VAR_IN_REG(1, args[1]);
	  MZ_GC_REG();	  
	  args[0] = widget->callback_widget;
	  args[1] = widget->other_arguments[CBT_DRAW];
	  scheme_apply(draw_callback, arg_count, args);
	  MZ_GC_UNREG();	  
	  delete[] args;
	}
    }
}

int
spark_fltk_widget::_custom_widget_handle(Fl_Widget* w, int event)
{
  Widget* widget = reinterpret_cast<Widget*>(w->argument());
  if (widget)
    {
      Scheme_Object* handle_callback = widget->other_callbacks[CBT_HANDLE];
      if (handle_callback != scheme_null)
	{
	  const int arg_count = 3;
	  Scheme_Object** args = new Scheme_Object*[arg_count];
	  args[0] = NULL;
	  args[1] = NULL;
	  args[2] = NULL;
	  MZ_GC_DECL_REG(arg_count);
	  for (int i=0; i<arg_count; ++i)
	    MZ_GC_VAR_IN_REG(i, args[i]);
	  MZ_GC_REG();	  
	  args[0] = widget->callback_widget;
	  args[1] = spark_fltk::int_to_event_symbol(event);
	  args[2] = widget->other_arguments[CBT_HANDLE];
	  scheme_apply(handle_callback, arg_count, args);
	  MZ_GC_UNREG();
	  delete[] args;
	  return 1;
	}
    }
  return 0;
}

Scheme_Object* 
spark_fltk_widget::new_custom_widget(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int x = 0; int y = 0; int w = 0; int h = 0;
  spark::Utils::int_from_scheme_long(argv[0], x);
  spark::Utils::int_from_scheme_long(argv[1], y);
  spark::Utils::int_from_scheme_long(argv[2], w);
  spark::Utils::int_from_scheme_long(argv[3], h);
  char* title = 0;
  if (argv[4] != scheme_null)
    {
      if (!SCHEME_CHAR_STRINGP(argv[4]))
	scheme_wrong_type("new-widget", "string", 4, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
      std::string tmp = SCHEME_BYTE_STR_VAL(str);
      size_t len = tmp.size();
      if (len)
	{
	  title = new char[len + 1];
	  strcpy(title, tmp.c_str());
	}
    }
  if (!SCHEME_SYMBOLP(argv[5]))
    scheme_wrong_type("new-custom-widget", "symbol", 5, argc, argv);
  std::string s = SCHEME_SYM_VAL(argv[5]);
  Fl_Widget* fl_widget = 0;
  if (s == "widget")
    fl_widget = new Custom_widget(x, y, w, h, title);
  else if (s == "box" || s == "border")
    fl_widget = new Custom_box(x, y, w, h, title);
  else if (s == "group")
    fl_widget = new Custom_group(x, y, w, h, title);
  else if (s == "window")
    fl_widget = new Custom_window(x, y, w, h, title);
  else if (s == "double-window")
    fl_widget = new Custom_double_window(x, y, w, h, title);
  else if (s == "single-window")
    fl_widget = new Custom_single_window(x, y, w, h, title);				 
  else if (s == "gl-window")
    fl_widget = new Custom_gl_window(x, y, w, h, title);
  else if (s == "button")
    fl_widget = new Custom_button(x, y, w, h, title);
  else if (s == "input")
    fl_widget = new Custom_input(x, y, w, h, title);
  else if (s == "text-display")
    fl_widget = new Custom_text_display(x, y, w, h, title);
  else if (s == "text-editor")
    fl_widget = new Custom_text_editor(x, y, w, h, title);
  else if (s == "browser")
    fl_widget = new Custom_browser(x, y, w, h, title);
  else if (s == "check-browser")
    fl_widget = new Custom_check_browser(x, y, w, h, title);
  else if (s == "check-button")
    fl_widget = new Custom_check_button(x, y, w, h, title);
  else if (s == "light-button")
    fl_widget = new Custom_light_button(x, y, w, h, title);
  else if (s == "repeat-button")
    fl_widget = new Custom_repeat_button(x, y, w, h, title);
  else if (s == "return-button")
    fl_widget = new Custom_return_button(x, y, w, h, title);
  else if (s == "round-button")
    fl_widget = new Custom_round_button(x, y, w, h, title);
  else if (s == "toggle-button")
    fl_widget = new Custom_toggle_button(x, y, w, h, title);
  else if (s == "chart")
    fl_widget = new Custom_chart(x, y, w, h, title);
  else if (s == "clock-output")
    fl_widget = new Custom_clock_output(x, y, w, h, title);
  else if (s == "clock")
    fl_widget = new Custom_clock(x, y, w, h, title);
  else if (s == "choice")
    fl_widget = new Custom_choice(x, y, w, h, title);
  else if (s == "menu-bar")
    fl_widget = new Custom_menu_bar(x, y, w, h, title);
  else if (s == "menu-button")
    fl_widget = new Custom_menu_button(x, y, w, h, title);
  else if (s == "positioner")
    fl_widget = new Custom_positioner(x, y, w, h, title);
  else if (s == "progress")
    fl_widget = new Custom_progress(x, y, w, h, title);
  else if (s == "adjuster")
    fl_widget = new Custom_adjuster(x, y, w, h, title);
  else if (s == "counter")
    fl_widget = new Custom_counter(x, y, w, h, title);
  else if (s == "dial")
    fl_widget = new Custom_dial(x, y, w, h, title);
  else if (s == "roller")
    fl_widget = new Custom_roller(x, y, w, h, title);
  else if (s == "slider")
    fl_widget = new Custom_slider(x, y, w, h, title);
  else if (s == "scrollbar")
    fl_widget = new Custom_scrollbar(x, y, w, h, title);
  else if (s == "value-slider")
    fl_widget = new Custom_value_slider(x, y, w, h, title);
  else if (s == "value-input")
    fl_widget = new Custom_value_input(x, y, w, h, title);
  else if (s == "value-output")
    fl_widget = new Custom_value_output(x, y, w, h, title);
  else if (s == "color-chooser")
    fl_widget = new Custom_color_chooser(x, y, w, h, title);
  else if (s == "input-choice")
    fl_widget = new Custom_input_choice(x, y, w, h, title);
  else if (s == "pack")
    fl_widget = new Custom_pack(x, y, w, h, title);
  else if (s == "scroll")
    fl_widget = new Custom_scroll(x, y, w, h, title);
  else if (s == "tabs")
    fl_widget = new Custom_tabs(x, y, w, h, title);
  else if (s == "spinner")
    fl_widget = new Custom_spinner(x, y, w, h, title);
  else if (s == "tile")
    fl_widget = new Custom_tile(x, y, w, h, title);
  else
    {
      DEFAULT_RET_FINISH;
    }
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  widget->other_callbacks[CBT_DRAW] = scheme_null;
  widget->other_callbacks[CBT_HANDLE] = scheme_null;
  widget->other_arguments[CBT_DRAW] = scheme_null;
  widget->other_arguments[CBT_HANDLE] = scheme_null;
  if (argc >= 7)
    widget->other_callbacks[CBT_DRAW] = argv[6];
  if (argc >= 8)
    widget->other_arguments[CBT_DRAW] = argv[7];
  if (argc >= 9)
    widget->other_callbacks[CBT_HANDLE] = argv[8];
  if (argc >= 10)
    widget->other_arguments[CBT_HANDLE] = argv[9];
  fl_widget->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(fl_widget, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_widget::draw_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget = _get_widget(argc, argv, 0);
  if (widget)
    {
      Widget* w = reinterpret_cast<Widget*>(widget->argument());
      if (w)
	{
	  w->other_callbacks[CBT_DRAW] = argv[1];
	  if (argc >= 3)
	    w->other_arguments[CBT_DRAW] = argv[2];
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_widget::handle_event_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget = _get_widget(argc, argv, 0);
  if (widget)
    {
      Widget* w = reinterpret_cast<Widget*>(widget->argument());
      if (w)
	{
	  w->other_callbacks[CBT_HANDLE] = argv[1];
	  if (argc >= 3)
	    w->other_arguments[CBT_HANDLE] = argv[2];
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}
