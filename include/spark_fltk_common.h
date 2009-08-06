// MzScheme inetrface to FLTK API.
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

#ifndef _SPARK_FLTK_COMMON_H_
#define _SPARK_FLTK_COMMON_H_

#include <map>
#include <vector>
#include <FL/Fl.H>
#include "spark.h"

namespace spark_fltk
{
  enum Fltk_tag
    {
      FL_INVALID_TAG,
      // widget tags
      FL_WIDGET_TAG,
      FL_BROWSER_TAG,
      FL_CHECK_BROWSER_TAG,
      FL_BUTTON_TAG,
      FL_CHECK_BUTTON_TAG,
      FL_LIGHT_BUTTON_TAG,
      FL_REPEAT_BUTTON_TAG,
      FL_RETURN_BUTTON_TAG,
      FL_ROUND_BUTTON_TAG,
      FL_TOGGLE_BUTTON_TAG,
      FL_CHART_TAG,
      FL_CLOCK_OUTPUT_TAG,
      FL_CLOCK_TAG,
      FL_FREE_TAG,
      FL_INPUT__TAG,
      FL_INPUT_TAG,
      FL_MENU__TAG,
      FL_MENU_ITEM_TAG,
      FL_CHOICE_TAG,
      FL_MENU_BAR_TAG,
      FL_MENU_BUTTON_TAG,
      FL_POSITIONER_TAG,
      FL_PROGRESS_TAG,
      FL_VALUATOR_TAG,
      FL_ADJUSTER_TAG,
      FL_COUNTER_TAG,
      FL_DIAL_TAG,
      FL_ROLLER_TAG,
      FL_SLIDER_TAG,
      FL_VALUE_INPUT_TAG,
      FL_VALUE_OUTPUT_TAG,
      FL_COLOR_CHOOSER_TAG,
      FL_HELP_VIEW_TAG,
      FL_INPUT_CHOICE_TAG,
      FL_PACK_TAG,
      FL_SCROLL_TAG,
      FL_TABS_TAG,
      FL_SPINNER_TAG,
      FL_TEXT_DISPLAY_TAG,
      FL_TEXT_EDITOR_TAG,
      FL_TILE_TAG,
      FL_WINDOW_TAG,
      FL_DOUBLE_WINDOW_TAG,
      FL_SINGLE_WINDOW_TAG,
      FL_WIZARD_TAG,
      FL_GROUP_TAG,

      FL_IMAGE_TAG,
      FL_TEXT_BUFFER_TAG,
      FL_FILE_CHOOSER_TAG,
      FL_SELECTION_TAG,
      FL_HELP_DIALOG_TAG
    }; // enum Fltk_tag

  enum Fltk_sort_type
    {
      FL_ALPHASORT, FL_CASEALPHASORT,
      FL_CASENUMERICSORT, FL_NUMERICSORT
    }; // enum Fltk_sort_type

  enum Callback_type
    {
      CBT_GENERIC,
      CBT_DRAW,
      CBT_HANDLE,
      CBT_LINK,
      CBT_STYLE_UNFINISHED,
      CBT_LAST
    };

  typedef std::map<Callback_type, Scheme_Object*> Callback_map;
  typedef std::map<Callback_type, Scheme_Object*> Argument_map;

  class Widget
  {
  public:
    Scheme_Object* callback;
    //Callback_map other_callbacks;
    Scheme_Object* other_callbacks[CBT_LAST];
    Scheme_Object* callback_widget;
    Scheme_Object* argument;
    Argument_map other_arguments;
    Fltk_tag tag;
    
    Widget() 
      : callback(0), 
	callback_widget(0),
	argument(0), tag(FL_INVALID_TAG)
    { }
    virtual ~Widget() 
    {
    }
  }; // struct Widget

  void _generic_callback(Fl_Widget*, void*);

  typedef std::vector<Scheme_Object*> Scheme_object_list;
  //typedef std::map<void*, Widget*> Widget_map;
  //typedef std::vector<Widget*> Widget_list;

  Fltk_tag _get_tag(int argc, Scheme_Object** argv, int idx);
  Fl_Image* _scheme_object_to_image(int argc, 
				    Scheme_Object** argv,
				    int idx);
  
  Fl_Widget* _get_widget(int argc, 
			 Scheme_Object** argv, 
			 int idx);
  spark::Status_code _add_browser_procedures(Scheme_Env*);
  spark::Status_code _add_browser_constants(Scheme_Env* env);
  spark::Status_code _add_button_procedures(Scheme_Env*);
  spark::Status_code _add_button_constants(Scheme_Env* env);
  spark::Status_code _add_chart_procedures(Scheme_Env*);
  spark::Status_code _add_chart_constants(Scheme_Env* env);
  spark::Status_code _add_clock_output_procedures(Scheme_Env*);
  spark::Status_code _add_group_procedures(Scheme_Env*);
  spark::Status_code _add_color_chooser_procedures(Scheme_Env*);
  spark::Status_code _add_help_view_procedures(Scheme_Env*);
  spark::Status_code _add_input_choice_procedures(Scheme_Env*);
  spark::Status_code _add_pack_procedures(Scheme_Env*);
  spark::Status_code _add_scroll_procedures(Scheme_Env*);
  spark::Status_code _add_scroll_constants(Scheme_Env*);
  spark::Status_code _add_tabs_procedures(Scheme_Env*);
  spark::Status_code _add_tile_procedures(Scheme_Env*);
  spark::Status_code _add_wizard_procedures(Scheme_Env*);
  spark::Status_code _add_spinner_procedures(Scheme_Env*);
  spark::Status_code _add_spinner_constants(Scheme_Env*);
  spark::Status_code _add_text_display_procedures(Scheme_Env*);
  spark::Status_code _add_text_display_constants(Scheme_Env*);
  spark::Status_code _add_text_buffer_procedures(Scheme_Env*);
  spark::Status_code _add_window_procedures(Scheme_Env*);
  spark::Status_code _add_input_procedures(Scheme_Env*);
  spark::Status_code _add_input_constants(Scheme_Env*);
  spark::Status_code _add_menu_procedures(Scheme_Env*);
  spark::Status_code _add_menu_constants(Scheme_Env*);
  spark::Status_code _add_positioner_procedures(Scheme_Env*);
  spark::Status_code _add_progress_procedures(Scheme_Env*);
  spark::Status_code _add_timer_procedures(Scheme_Env*);
  spark::Status_code _add_valuator_procedures(Scheme_Env*);
  spark::Status_code _add_valuator_constants(Scheme_Env*);
  spark::Status_code _add_image_procedures(Scheme_Env*);
  spark::Status_code _add_file_chooser_procedures(Scheme_Env*);
  spark::Status_code _add_file_chooser_constants(Scheme_Env*);
  spark::Status_code _add_help_dialog_procedures(Scheme_Env*);
  spark::Status_code _add_widget_procedures(Scheme_Env*);
  spark::Status_code _add_event_constants(Scheme_Env*);
  spark::Status_code _add_event_procedures(Scheme_Env*);
  spark::Status_code _add_draw_constants(Scheme_Env*);
  spark::Status_code _add_draw_procedures(Scheme_Env*);
  spark::Status_code _add_ask_constants(Scheme_Env*);
  spark::Status_code _add_ask_procedures(Scheme_Env*);

  int intlist_to_flag(Scheme_Object* lst);
  Scheme_Object* align_to_intlist(int a);
  Scheme_Object* int_to_event_symbol(int event);
}// namespace spark_fltk

#endif // #ifndef _SPARK_FLTK_COMMON_H_
