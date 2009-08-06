// The generic widget functions.
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

#ifndef _SPARK_FLTK_WIDGET_H_
#define _SPARK_FLTK_WIDGET_H_

namespace spark_fltk_widget
{
  static void _custom_widget_draw(Fl_Widget* w);
  static int _custom_widget_handle(Fl_Widget* w, 
				   int event);

  // Custom widgets

  class Custom_widget  : public Fl_Widget
  {
  public:
    Custom_widget(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Widget(x, y, w, h, label)
    {      
    }
    void draw()
    {
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      return r;
    }
  }; // class Custom_widget : public Fl_Widget

  class Custom_box : public Fl_Box
  {
  public:
    Custom_box(int x, int y,
	       int w, int h,
	       const char* label = 0)
      : Fl_Box(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Box::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Box::handle(event);
      return r;
    }
  }; // class Custom_box : public Fl_Box 

  class Custom_group : public Fl_Group
  {
  public:
    Custom_group(int x, int y,
		 int w, int h,
		 const char* label = 0)
      : Fl_Group(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Group::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Group::handle(event);
      return r;
    }
  }; // class Custom_group : public Fl_Group

  class Custom_browser : public Fl_Browser 
  {
  public:
    Custom_browser(int x, int y,
		   int w, int h,
		   const char* label = 0)
      : Fl_Browser(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Browser::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Browser::handle(event);
      return r;
    }
  }; // class Custom_browser : public Fl_Browser

  class Custom_check_browser : public Fl_Check_Browser
  {
  public:
    Custom_check_browser(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Check_Browser(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Check_Browser::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Check_Browser::handle(event);
      return r;
    }
  }; // class Custom_check_browser : public Fl_Check_Browser

  class Custom_check_button : public Fl_Check_Button
  {
  public:
    Custom_check_button(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Check_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Check_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Check_Button::handle(event);
      return r;
    }
  }; // class Custom_check_button : public Fl_Check_Button

  class Custom_window : public Fl_Window 
  {
  public:
    Custom_window(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Window(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Window::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Window::handle(event);
      return r;
    }
  }; // class Custom_window : public Fl_Window

  class Custom_double_window : public Fl_Double_Window
  {
  public:
    Custom_double_window(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Double_Window(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Double_Window::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Double_Window::handle(event);
      return r;
    }
  }; // class Custom_double_window : public Fl_Double_Window

  class Custom_single_window : public Fl_Single_Window
  {
  public:
    Custom_single_window(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Single_Window(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Single_Window::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Single_Window::handle(event);
      return r;
    }
  }; // class Custom_single_window : public Fl_Single_Window

  class Custom_gl_window : public Fl_Gl_Window
  {
  public:
    Custom_gl_window(int X, int Y, int W, int H, const char* L = 0)
      : Fl_Gl_Window(X, Y, W, H, L)
    {
      mode(FL_RGB | FL_ALPHA | FL_DEPTH | FL_DOUBLE);
    }
    
    // Fl_Gl_Window overrides
    void draw()
    {
      Fl_Gl_Window::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Gl_Window::handle(event);
      return r;
    }
    // :~
  }; // class Custom_gl_window : public Fl_Gl_Window

  class Custom_button : public Fl_Button
  {
  public:
    Custom_button(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Button::handle(event);
      return r;
    }
  }; // class Custom_button : public Fl_Button

  class Custom_light_button : public Fl_Light_Button
  {
  public:
    Custom_light_button(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Light_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Light_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Light_Button::handle(event);
      return r;
    }
  }; // class Custom_light_button : public Fl_Light_Button

  class Custom_repeat_button : public Fl_Repeat_Button
  {
  public:
    Custom_repeat_button(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Repeat_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Repeat_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Repeat_Button::handle(event);
      return r;
    }
  }; // class Custom_repeat_button : public Fl_Repeat_Button

  class Custom_return_button : public Fl_Return_Button
  {
  public:
    Custom_return_button(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Return_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Return_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Return_Button::handle(event);
      return r;
    }
  }; // class Custom_return_button : public Fl_Return_Button

  class Custom_round_button : public Fl_Round_Button
  {
  public:
    Custom_round_button(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Round_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Round_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Round_Button::handle(event);
      return r;
    }
  }; // class Custom_round_button : public Fl_Round_Button

  class Custom_toggle_button : public Fl_Toggle_Button
  {
  public:
    Custom_toggle_button(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Toggle_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Toggle_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Toggle_Button::handle(event);
      return r;
    }
  }; // class Custom_toggle_button : public Fl_Toggle_Button

  class Custom_chart : public Fl_Chart
  {
  public:
    Custom_chart(int x, int y,
		 int w, int h,
		 const char* label = 0)
      : Fl_Chart(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Chart::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Chart::handle(event);
      return r;
    }
  }; // class Custom_chart : public Fl_Chart

  class Custom_clock : public Fl_Clock
  {
  public:
    Custom_clock(int x, int y,
		 int w, int h,
		 const char* label = 0)
      : Fl_Clock(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Clock::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Clock::handle(event);
      return r;
    }
  }; // class Custom_clock : public Fl_Clock

  class Custom_clock_output : public Fl_Clock_Output
  {
  public:
    Custom_clock_output(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Clock_Output(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Clock_Output::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Clock_Output::handle(event);
      return r;
    }
  }; // class Custom_clock_output : public Fl_Clock_Output

  class Custom_progress : public Fl_Progress
  {
  public:
    Custom_progress(int x, int y,
		    int w, int h,
		    const char* label = 0)
      : Fl_Progress(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Progress::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Progress::handle(event);
      return r;
    }
  }; // class Custom_progress : public Fl_Progress

  class Custom_color_chooser : public Fl_Color_Chooser
  {
  public:
    Custom_color_chooser(int x, int y,
			 int w, int h,
			 const char* label = 0)
      : Fl_Color_Chooser(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Color_Chooser::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Color_Chooser::handle(event);
      return r;
    }
  }; // class Custom_color_chooser : public Fl_Color_Chooser

  class Custom_input_choice : public Fl_Input_Choice
  {
  public:
    Custom_input_choice(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Input_Choice(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Input_Choice::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Input_Choice::handle(event);
      return r;
    }
  }; // class Custom_input_choice : public Fl_Input_Choice

  class Custom_pack : public Fl_Pack
  {
  public:
    Custom_pack(int x, int y,
		int w, int h,
		const char* label = 0)
      : Fl_Pack(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Pack::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Pack::handle(event);
      return r;
    }
  }; // class Custom_pack : public Fl_Pack

  class Custom_scroll : public Fl_Scroll
  {
  public:
    Custom_scroll(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Scroll(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Scroll::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Scroll::handle(event);
      return r;
    }
  }; // class Custom_scroll : public Fl_Scroll

  class Custom_tabs : public Fl_Tabs
  {
  public:
    Custom_tabs(int x, int y,
		int w, int h,
		const char* label = 0)
      : Fl_Tabs(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Tabs::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Tabs::handle(event);
      return r;
    }
  }; // class Custom_tabs : public Fl_Tabs

  class Custom_spinner : public Fl_Spinner
  {
  public:
    Custom_spinner(int x, int y,
		   int w, int h,
		   const char* label = 0)
      : Fl_Spinner(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Spinner::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Spinner::handle(event);
      return r;
    }
  }; // class Custom_spinner : public Fl_Spinner

  class Custom_tile : public Fl_Tile
  {
  public:
    Custom_tile(int x, int y,
		int w, int h,
		const char* label = 0)
      : Fl_Tile(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Tile::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Tile::handle(event);
      return r;
    }
  }; // class Custom_tile : public Fl_Tile

  class Custom_choice : public Fl_Choice
  {
  public:
    Custom_choice(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Choice(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Choice::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Choice::handle(event);
      return r;
    }
  }; // class Custom_choice : public Fl_Choice

  class Custom_menu_bar : public Fl_Menu_Bar
  {
  public:
    Custom_menu_bar(int x, int y,
		    int w, int h,
		    const char* label = 0)
      : Fl_Menu_Bar(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Menu_Bar::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Menu_Bar::handle(event);
      return r;
    }
  }; // class Custom_menu_bar : public Fl_Menu_Bar

  class Custom_menu_button : public Fl_Menu_Button
  {
  public:
    Custom_menu_button(int x, int y,
		       int w, int h,
		       const char* label = 0)
      : Fl_Menu_Button(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Menu_Button::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Menu_Button::handle(event);
      return r;
    }
  }; // class Custom_menu_button : public Fl_Menu_Button

  class Custom_positioner : public Fl_Positioner
  {
  public:
    Custom_positioner(int x, int y,
		      int w, int h,
		      const char* label = 0)
      : Fl_Positioner(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Positioner::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Positioner::handle(event);
      return r;
    }
  }; // class Custom_positioner : public Fl_Positioner

  class Custom_adjuster : public Fl_Adjuster
  {
  public:
    Custom_adjuster(int x, int y,
		    int w, int h,
		    const char* label = 0)
      : Fl_Adjuster(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Adjuster::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Adjuster::handle(event);
      return r;
    }
  }; // class Custom_adjuster : public Fl_Adjuster

  class Custom_counter : public Fl_Counter
  {
  public:
    Custom_counter(int x, int y,
		   int w, int h,
		   const char* label = 0)
      : Fl_Counter(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Counter::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Counter::handle(event);
      return r;
    }
  }; // class Custom_counter : public Fl_Counter

  class Custom_dial : public Fl_Dial
  {
  public:
    Custom_dial(int x, int y,
		int w, int h,
		const char* label = 0)
      : Fl_Dial(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Dial::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Dial::handle(event);
      return r;
    }
  }; // class Custom_dial : public Fl_Dial

  class Custom_roller : public Fl_Roller
  {
  public:
    Custom_roller(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Roller(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Roller::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Roller::handle(event);
      return r;
    }
  }; // class Custom_roller : public Fl_Roller

  class Custom_slider : public Fl_Slider
  {
  public:
    Custom_slider(int x, int y,
		  int w, int h,
		  const char* label = 0)
      : Fl_Slider(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Slider::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Slider::handle(event);
      return r;
    }
  }; // class Custom_slider : public Fl_Slider

  class Custom_scrollbar : public Fl_Scrollbar
  {
  public:
    Custom_scrollbar(int x, int y,
		     int w, int h,
		     const char* label = 0)
      : Fl_Scrollbar(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Scrollbar::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Scrollbar::handle(event);
      return r;
    }
  }; // class Custom_scrollbar : public Fl_Scrollbar

  class Custom_value_slider : public Fl_Value_Slider
  {
  public:
    Custom_value_slider(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Value_Slider(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Value_Slider::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Value_Slider::handle(event);
      return r;
    }
  }; // class Custom_value_slider : public Fl_Value_Slider

  class Custom_value_input : public Fl_Value_Input
  {
  public:
    Custom_value_input(int x, int y,
		       int w, int h,
		       const char* label = 0)
      : Fl_Value_Input(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Value_Input::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Value_Input::handle(event);
      return r;
    }
  }; // class Custom_value_input : public Fl_Value_Input

  class Custom_value_output : public Fl_Value_Output
  {
  public:
    Custom_value_output(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Value_Output(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Value_Output::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Value_Output::handle(event);
      return r;
    }
  }; // class Custom_value_output : public Fl_Value_Output

  class Custom_input : public Fl_Input
  {
  public:
    Custom_input(int x, int y,
		 int w, int h,
		 const char* label = 0)
      : Fl_Input(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Input::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Input::handle(event);
      return r;
    }
  }; // class Custom_input : public Fl_Input

  class Custom_text_display : public Fl_Text_Display
  {
  public:
    Custom_text_display(int x, int y,
			int w, int h,
			const char* label = 0)
      : Fl_Text_Display(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Text_Display::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Text_Display::handle(event);
      return r;
    }
  }; // class Custom_text_display : public Fl_Text_Display

  class Custom_text_editor : public Fl_Text_Editor
  {
  public:
    Custom_text_editor(int x, int y,
		       int w, int h,
		       const char* label = 0)
      : Fl_Text_Editor(x, y, w, h, label)
    {      
    }
    void draw()
    {
      Fl_Text_Editor::draw();
      _custom_widget_draw(this);
    }
    int handle(int event)
    {
      int r = _custom_widget_handle(this, event);
      if (r == 0)
	return Fl_Text_Editor::handle(event);
      return r;
    }
  }; // class Custom_text_editor : public Fl_Text_Editor
 
} // namespace spark_fltk_widget

#endif // #ifndef _SPARK_FLTK_WIDGET_H_
