// MzScheme inetrface to the FLTK base widget API.
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

#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Menu_.H>
#include <FL/Fl_Menu_Item.H>
#include "spark_fltk.h"
#include "spark_fltk_common.h"
using namespace spark_fltk;

// Widget_map Widget_holder::_active_widgets;

static const char* MODULE_NAME = "#%spark-fltk";

// exported function signatures
namespace spark_fltk
{
  // general widget methods and some most common functions.
  static Scheme_Object* show(int, Scheme_Object**);
  static Scheme_Object* activate(int, Scheme_Object**);
  static Scheme_Object* deactivate(int, Scheme_Object**);
  static Scheme_Object* active(int, Scheme_Object**);
  static Scheme_Object* active_r(int, Scheme_Object**);
  static Scheme_Object* align(int, Scheme_Object**);
  static Scheme_Object* argument(int, Scheme_Object**);
  static Scheme_Object* box(int, Scheme_Object**);
  static Scheme_Object* callback(int, Scheme_Object**);
  static Scheme_Object* set_callback_widget(int, Scheme_Object**);
  static Scheme_Object* changed(int, Scheme_Object**);
  static Scheme_Object* clear_changed(int, Scheme_Object**);
  static Scheme_Object* clear_output(int, Scheme_Object**);
  static Scheme_Object* clear_visible(int, Scheme_Object**);
  static Scheme_Object* clear_visible_focus(int, Scheme_Object**);
  static Scheme_Object* color(int, Scheme_Object**);
  static Scheme_Object* contains(int, Scheme_Object**);
  static Scheme_Object* copy_label(int, Scheme_Object**);
  static Scheme_Object* damage(int, Scheme_Object**);
  static Scheme_Object* deimage(int, Scheme_Object**);
  static Scheme_Object* do_callback(int, Scheme_Object**);
  static Scheme_Object* height(int, Scheme_Object**);
  static Scheme_Object* hide(int, Scheme_Object**);
  static Scheme_Object* image(int, Scheme_Object**);
  static Scheme_Object* inside(int, Scheme_Object**);
  static Scheme_Object* label(int, Scheme_Object**);
  static Scheme_Object* labelcolor(int, Scheme_Object**);
  static Scheme_Object* labelfont(int, Scheme_Object**);
  static Scheme_Object* labelsize(int, Scheme_Object**);
  static Scheme_Object* labeltype(int, Scheme_Object**);
  static Scheme_Object* output(int, Scheme_Object**);
  static Scheme_Object* parent(int, Scheme_Object**);
  static Scheme_Object* position(int, Scheme_Object**);
  static Scheme_Object* redraw(int, Scheme_Object**);
  static Scheme_Object* redraw_label(int, Scheme_Object**);
  static Scheme_Object* resize(int, Scheme_Object**);
  static Scheme_Object* selection_color(int, Scheme_Object**);
  static Scheme_Object* set_changed(int, Scheme_Object**);
  static Scheme_Object* set_output(int, Scheme_Object**);
  static Scheme_Object* set_visible(int, Scheme_Object**);
  static Scheme_Object* set_visible_focus(int, Scheme_Object**);
  static Scheme_Object* size(int, Scheme_Object**);
  static Scheme_Object* take_focus(int, Scheme_Object**);
  static Scheme_Object* takesevents(int, Scheme_Object**);
  static Scheme_Object* tooltip(int, Scheme_Object**);
  static Scheme_Object* type(int, Scheme_Object**);
  static Scheme_Object* user_data(int, Scheme_Object**);
  static Scheme_Object* visible(int, Scheme_Object**);
  static Scheme_Object* visible_focus(int, Scheme_Object**);
  static Scheme_Object* visible_r(int, Scheme_Object**);
  static Scheme_Object* width(int, Scheme_Object**);
  static Scheme_Object* when(int, Scheme_Object**);
  static Scheme_Object* x_pos(int, Scheme_Object**);
  static Scheme_Object* y_pos(int, Scheme_Object**);
  static Scheme_Object* fl_box(int, Scheme_Object**);
  static Scheme_Object* dispose(int, Scheme_Object**);
  // Fl
  static Scheme_Object* focus(int, Scheme_Object**);
  static Scheme_Object* selection_owner(int, Scheme_Object**);
  static Scheme_Object* run(int, Scheme_Object**);
  static Scheme_Object* awake(int, Scheme_Object**);
  static Scheme_Object* start_thread_manager(int, Scheme_Object**);
  static Scheme_Object* stop_thread_manager(int, Scheme_Object**);
  static Scheme_Object* args(int, Scheme_Object**);
  static Scheme_Object* get_system_colors(int, Scheme_Object**);
  static Scheme_Object* foreground(int, Scheme_Object**);
  static Scheme_Object* background(int, Scheme_Object**);
  static Scheme_Object* contrast(int, Scheme_Object**);
  static Scheme_Object* redraw_all(int, Scheme_Object**);
  static Scheme_Object* scheme(int, Scheme_Object**);
  static Scheme_Object* set_fonts(int, Scheme_Object**);
  static Scheme_Object* get_font_name(int, Scheme_Object**);
  static Scheme_Object* get_font_attributes(int, Scheme_Object**);
  static Scheme_Object* get_font(int, Scheme_Object**);
  static Scheme_Object* get_font_sizes(int, Scheme_Object**);
  static Scheme_Object* box_dx(int, Scheme_Object**);
  static Scheme_Object* box_dy(int, Scheme_Object**);
  static Scheme_Object* box_dw(int, Scheme_Object**);
  static Scheme_Object* box_dh(int, Scheme_Object**);
  static Scheme_Object* rgb_color(int, Scheme_Object**);
  static Scheme_Object* check_threads(int, Scheme_Object**);
} // namespace spark_fltk

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    // alignments
    Constant("FL-ALIGN-BOTTOM", FL_ALIGN_BOTTOM),
    Constant("FL-ALIGN-CENTER", FL_ALIGN_CENTER),
    Constant("FL-ALIGN-CLIP", FL_ALIGN_CLIP),
    Constant("FL-ALIGN-INSIDE", FL_ALIGN_INSIDE),
    Constant("FL-ALIGN-LEFT", FL_ALIGN_LEFT),
    Constant("FL-ALIGN-RIGHT", FL_ALIGN_RIGHT),
    Constant("FL-ALIGN-TEXT-OVER-IMAGE", FL_ALIGN_TEXT_OVER_IMAGE),
    Constant("FL-ALIGN-TOP", FL_ALIGN_TOP),
    Constant("FL-ALIGN-WRAP", FL_ALIGN_WRAP),
    // box types
    Constant("FL-NO-BOX", FL_NO_BOX),
    Constant("FL-FLAT-BOX", FL_FLAT_BOX),
    Constant("FL-UP-BOX", FL_UP_BOX),
    Constant("FL-DOWN-BOX", FL_DOWN_BOX),
    Constant("FL-UP-FRAME", FL_UP_FRAME),	
    Constant("FL-DOWN-FRAME", FL_DOWN_FRAME),
    Constant("FL-THIN-UP-BOX", FL_THIN_UP_BOX),
    Constant("FL-THIN-DOWN-BOX", FL_THIN_DOWN_BOX),
    Constant("FL-THIN-UP-FRAME", FL_THIN_UP_FRAME),
    Constant("FL-THIN-DOWN-FRAME", FL_THIN_DOWN_FRAME),
    Constant("FL-ENGRAVED-BOX", FL_ENGRAVED_BOX),
    Constant("FL-EMBOSSED-BOX", FL_EMBOSSED_BOX),
    Constant("FL-ENGRAVED-FRAME", FL_ENGRAVED_FRAME),
    Constant("FL-EMBOSSED-FRAME", FL_EMBOSSED_FRAME),
    Constant("FL-BORDER-BOX", FL_BORDER_BOX),
    Constant("FL-SHADOW-BOX", FL_SHADOW_BOX),
    Constant("FL-BORDER-FRAME", FL_BORDER_FRAME),
    Constant("FL-SHADOW-FRAME", FL_SHADOW_FRAME),
    Constant("FL-ROUNDED-BOX", FL_ROUNDED_BOX),
    Constant("FL-RSHADOW-BOX", FL_RSHADOW_BOX),
    Constant("FL-ROUNDED-FRAME", FL_ROUNDED_FRAME),
    Constant("FL-RFLAT-BOX", FL_RFLAT_BOX),
    Constant("FL-ROUND-UP-BOX", FL_ROUND_UP_BOX),
    Constant("FL-ROUND-DOWN-BOX", FL_ROUND_DOWN_BOX),
    Constant("FL-DIAMOND-UP-BOX", FL_DIAMOND_UP_BOX),
    Constant("FL-DIAMOND-DOWN-BOX", FL_DIAMOND_DOWN_BOX),
    Constant("FL-OVAL-BOX", FL_OVAL_BOX),
    Constant("FL-OSHADOW-BOX", FL_OSHADOW_BOX),
    Constant("FL-OVAL-FRAME", FL_OVAL_FRAME),
    Constant("FL-OFLAT-BOX", FL_OFLAT_BOX),
    Constant("FL-PLASTIC-UP-BOX", FL_PLASTIC_UP_BOX),	
    Constant("FL-PLASTIC-DOWN-BOX", FL_PLASTIC_DOWN_BOX),
    Constant("FL-PLASTIC-UP-FRAME", FL_PLASTIC_UP_FRAME),
    Constant("FL-PLASTIC-DOWN-FRAME", FL_PLASTIC_DOWN_FRAME),
    Constant("FL-PLASTIC-THIN-UP-BOX", FL_PLASTIC_THIN_UP_BOX),
    Constant("FL-PLASTIC-THIN-DOWN-BOX", FL_PLASTIC_THIN_DOWN_BOX),
    Constant("FL-PLASTIC-ROUND-UP-BOX", FL_PLASTIC_ROUND_UP_BOX),
    Constant("FL-PLASTIC-ROUND-DOWN-BOX", FL_PLASTIC_ROUND_DOWN_BOX),
    Constant("FL-GTK-UP-BOX", FL_GTK_UP_BOX),
    Constant("FL-GTK-DOWN-BOX", FL_GTK_DOWN_BOX),
    Constant("FL-GTK-UP-FRAME", FL_GTK_UP_FRAME),
    Constant("FL-GTK-DOWN-FRAME", FL_GTK_DOWN_FRAME),
    Constant("FL-GTK-THIN-UP-BOX", FL_GTK_THIN_UP_BOX),
    Constant("FL-GTK-THIN-DOWN-BOX", FL_GTK_THIN_DOWN_BOX),
    Constant("FL-GTK-THIN-UP-FRAME", FL_GTK_THIN_UP_FRAME),
    Constant("FL-GTK-THIN-DOWN-FRAME", FL_GTK_THIN_DOWN_FRAME),
    Constant("FL-GTK-ROUND-UP-BOX", FL_GTK_ROUND_UP_BOX),
    Constant("FL-GTK-ROUND-DOWN-BOX", FL_GTK_ROUND_DOWN_BOX),
    Constant("FL-FREE-BOXTYPE", FL_FREE_BOXTYPE),
    // colors
    Constant("FL-BACKGROUND-COLOR", FL_BACKGROUND_COLOR),
    Constant("FL-BACKGROUND2-COLOR", FL_BACKGROUND2_COLOR),
    Constant("FL-FOREGROUND-COLOR", FL_FOREGROUND_COLOR),
    Constant("FL-INACTIVE-COLOR", FL_INACTIVE_COLOR),
    Constant("FL-SELECTION-COLOR", FL_SELECTION_COLOR),
    Constant("FL-BLACK", FL_BLACK),
    Constant("FL-BLUE", FL_BLUE),
    Constant("FL-CYAN", FL_CYAN),
    Constant("FL-DARK-BLUE", FL_DARK_BLUE),
    Constant("FL-DARK-CYAN", FL_DARK_CYAN),
    Constant("FL-DARK-GREEN", FL_DARK_GREEN),
    Constant("FL-DARK-MAGENTA", FL_DARK_MAGENTA),
    Constant("FL-DARK-RED", FL_DARK_RED),
    Constant("FL-DARK-YELLOW", FL_DARK_YELLOW),
    Constant("FL-GREEN", FL_GREEN),
    Constant("FL-MAGENTA", FL_MAGENTA),
    Constant("FL-RED", FL_RED),
    Constant("FL-WHITE", FL_WHITE),
    Constant("FL-YELLOW", FL_YELLOW),
    Constant("FL-GRAY0", FL_GRAY0),
    Constant("FL-DARK3", FL_DARK3),
    Constant("FL-DARK2", FL_DARK2),
    Constant("FL-DARK1", FL_DARK1),
    Constant("FL-LIGHT3", FL_LIGHT3),
    Constant("FL-LIGHT2", FL_LIGHT2),
    Constant("FL-LIGHT1", FL_LIGHT1),
    // fonts
    Constant("FL-HELVETICA", FL_HELVETICA),
    Constant("FL-HELVETICA-BOLD", FL_HELVETICA_BOLD),
    Constant("FL-HELVETICA-ITALIC", FL_HELVETICA_ITALIC),
    Constant("FL-HELVETICA-BOLD-ITALIC", FL_HELVETICA_BOLD_ITALIC),
    Constant("FL-COURIER", FL_COURIER),
    Constant("FL-COURIER-BOLD", FL_COURIER_BOLD),
    Constant("FL-COURIER-ITALIC", FL_COURIER_ITALIC),
    Constant("FL-COURIER-BOLD-ITALIC", FL_COURIER_BOLD_ITALIC),
    Constant("FL-TIMES", FL_TIMES),
    Constant("FL-TIMES-BOLD", FL_TIMES_BOLD),
    Constant("FL-TIMES-ITALIC", FL_TIMES_ITALIC),
    Constant("FL-TIMES-BOLD-ITALIC", FL_TIMES_BOLD_ITALIC),
    Constant("FL-SYMBOL", FL_SYMBOL),
    Constant("FL-SCREEN", FL_SCREEN),
    Constant("FL-SCREEN-BOLD", FL_SCREEN_BOLD),
    Constant("FL-ZAPF-DINGBATS", FL_ZAPF_DINGBATS),
    Constant("FL-FREE-FONT", FL_FREE_FONT),
    Constant("FL-BOLD", FL_BOLD),
    Constant("FL-ITALIC", FL_ITALIC),
    // label types
    Constant("FL-NORMAL-LABEL", FL_NORMAL_LABEL),
    Constant("FL-NO-LABEL", FL_NO_LABEL),
    Constant("FL-SHADOW-LABEL", FL_SHADOW_LABEL),
    Constant("FL-ENGRAVED-LABEL", FL_ENGRAVED_LABEL),
    Constant("FL-EMBOSSED-LABEL", FL_EMBOSSED_LABEL),
    Constant("FL-ICON-LABEL", _FL_ICON_LABEL),
    // when
    Constant("FL-WHEN-NEVER", FL_WHEN_NEVER),
    Constant("FL-WHEN-CHANGED", FL_WHEN_CHANGED),
    Constant("FL-WHEN-RELEASE", FL_WHEN_RELEASE),
    Constant("FL-WHEN-RELEASE-ALWAYS", FL_WHEN_RELEASE_ALWAYS),
    Constant("FL-WHEN-ENTER-KEY", FL_WHEN_ENTER_KEY),
    Constant("FL-WHEN-ENTER-KEY-ALWAYS", FL_WHEN_ENTER_KEY_ALWAYS),
    Constant("FL-WHEN-ENTER-KEY-CHANGED", FL_WHEN_ENTER_KEY_CHANGED),
    Constant("FL-WHEN-NOT-CHANGED", FL_WHEN_NOT_CHANGED),
    // size
    Constant("FL-NORMAL-SIZE", FL_NORMAL_SIZE),
    // keys
    Constant("FL-Button", FL_Button), // use Fl_Button+FL_*_MOUSE
    Constant("FL-BackSpace", FL_BackSpace),
    Constant("FL-Tab", FL_Tab),
    Constant("FL-Enter", FL_Enter),
    Constant("FL-Pause", FL_Pause),
    Constant("FL-Scroll-Lock", FL_Scroll_Lock),
    Constant("FL-Escape", FL_Escape),
    Constant("FL-Home", FL_Home),
    Constant("FL-Left", FL_Left),
    Constant("FL-Up", FL_Up),
    Constant("FL-Right", FL_Right),
    Constant("FL-Down", FL_Down),
    Constant("FL-Page-Up", FL_Page_Up),
    Constant("FL-Page-Down", FL_Page_Down),
    Constant("FL-End", FL_End),
    Constant("FL-Print", FL_Print),
    Constant("FL-Insert", FL_Insert),
    Constant("FL-Menu", FL_Menu),
    Constant("FL-Help", FL_Help),
    Constant("FL-Num-Lock", FL_Num_Lock),
    Constant("FL-KP", FL_KP), // use FL_KP+'x' for 'x' on numeric keypad
    Constant("FL-KP-Enter", FL_KP_Enter), // same as Fl_KP+'\r'
    Constant("FL-KP-Last", FL_KP_Last), // use to range-check keypad
    Constant("FL-F", FL_F), // use FL_F+n for function key n
    Constant("FL-F-Last", FL_F_Last), // use to range-check function keys
    Constant("FL-Shift-L", FL_Shift_L),
    Constant("FL-Shift-R", FL_Shift_R),
    Constant("FL-Control-L", FL_Control_L),
    Constant("FL-Control-R", FL_Control_R),
    Constant("FL-Caps-Lock", FL_Caps_Lock),
    Constant("FL-Meta-L", FL_Meta_L), // the left MSWindows key on XFree86
    Constant("FL-Meta-R", FL_Meta_R), // the right MSWindows key on XFree86
    Constant("FL-Alt-L", FL_Alt_L),
    Constant("FL-Alt-R", FL_Alt_R),
    Constant("FL-Delete", FL_Delete),
    // cursors
    Constant("FL-CURSOR-DEFAULT", FL_CURSOR_DEFAULT),
    Constant("FL-CURSOR-ARROW", FL_CURSOR_ARROW),
    Constant("FL-CURSOR-CROSS", FL_CURSOR_CROSS),
    Constant("FL-CURSOR-WAIT", FL_CURSOR_WAIT),
    Constant("FL-CURSOR-INSERT", FL_CURSOR_INSERT),
    Constant("FL-CURSOR-HAND", FL_CURSOR_HAND),
    Constant("FL-CURSOR-HELP", FL_CURSOR_HELP),
    Constant("FL-CURSOR-MOVE", FL_CURSOR_MOVE),
    Constant("FL-CURSOR-NS", FL_CURSOR_NS),
    Constant("FL-CURSOR-WE", FL_CURSOR_WE),
    Constant("FL-CURSOR-NWSE", FL_CURSOR_NWSE),
    Constant("FL-CURSOR-NESW", FL_CURSOR_NESW),
    Constant("FL-CURSOR-NONE", FL_CURSOR_NONE),
    Constant("FL-CURSOR-N", FL_CURSOR_N),
    Constant("FL-CURSOR-NE", FL_CURSOR_NE),
    Constant("FL-CURSOR-E", FL_CURSOR_E),
    Constant("FL-CURSOR-SE", FL_CURSOR_SE),
    Constant("FL-CURSOR-S", FL_CURSOR_S),
    Constant("FL-CURSOR-SW", FL_CURSOR_SW),
    Constant("FL-CURSOR-W", FL_CURSOR_W),
    Constant("FL-CURSOR-NW", FL_CURSOR_NW),
    // fltk provides bitmaps for these:
    Constant("FL-CURSOR-NS", FL_CURSOR_NS),
    Constant("FL-CURSOR-WE", FL_CURSOR_WE),
    Constant("FL-CURSOR-NWSE", FL_CURSOR_NWSE),
    Constant("FL-CURSOR-NESW", FL_CURSOR_NESW),
    Constant("FL-CURSOR-NONE", FL_CURSOR_NONE),
    // for back compatability (non MSWindows ones):
    Constant("FL-CURSOR-N", FL_CURSOR_N),
    Constant("FL-CURSOR-NE", FL_CURSOR_NE),
    Constant("FL-CURSOR-E", FL_CURSOR_E),
    Constant("FL-CURSOR-SE", FL_CURSOR_SE),
    Constant("FL-CURSOR-S", FL_CURSOR_S),
    Constant("FL-CURSOR-SW", FL_CURSOR_SW),
    Constant("FL-CURSOR-W", FL_CURSOR_W),
    Constant("FL-CURSOR-NW", FL_CURSOR_NW),
    //Constant("FL-CURSOR-NS", FL_CURSOR_NS),
    //Constant("FL-CURSOR-WE", FL_CURSOR_WE),

    Constant("", 0)
  };
  spark::Status_code sc;
  if ((sc = add_constants(env, constants, "spark-fltk"))
      != spark::SUCCESS)
    return sc;
  if ((sc = _add_browser_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_button_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_chart_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_scroll_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_spinner_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_text_display_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_input_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_menu_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_valuator_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_file_chooser_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_event_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_draw_constants(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_ask_constants(env)) != spark::SUCCESS)
    return sc;
  return sc;
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk::show, "show", 1),
    new Procedure(spark_fltk::activate, "activate", 1),
    new Procedure(spark_fltk::deactivate, "deactivate", 1),
    new Procedure(spark_fltk::active, "active", 1),
    new Procedure(spark_fltk::active_r, "active-r", 1),
    new Procedure(spark_fltk::align, "align", 1, 2),
    new Procedure(spark_fltk::argument, "argument", 1, 2),
    new Procedure(spark_fltk::box, "box", 1, 2),
    new Procedure(spark_fltk::callback, "callback", 1, 2),
    new Procedure(spark_fltk::set_callback_widget, "set-callback-widget!", 2),
    new Procedure(spark_fltk::changed, "changed", 1),
    new Procedure(spark_fltk::clear_changed, "clear-changed", 1),
    new Procedure(spark_fltk::clear_output, "clear-output", 1),
    new Procedure(spark_fltk::clear_visible, "clear-visible", 1),
    new Procedure(spark_fltk::clear_visible_focus, "clear-visible-focus", 1),
    new Procedure(spark_fltk::color, "color", 1, 3),
    new Procedure(spark_fltk::contains, "contains", 2),
    new Procedure(spark_fltk::copy_label, "copy-label", 2),
    new Procedure(spark_fltk::damage, "damage", 1, 6),
    new Procedure(spark_fltk::deimage, "deimage", 1, 2),
    new Procedure(spark_fltk::do_callback, "do-callback", 1, 2),
    new Procedure(spark_fltk::height, "height", 1),
    new Procedure(spark_fltk::hide, "hide", 1),
    new Procedure(spark_fltk::image, "image", 1, 2),
    new Procedure(spark_fltk::inside, "inside", 2),
    new Procedure(spark_fltk::label, "label", 1, 2),
    new Procedure(spark_fltk::labelcolor, "label-color", 1, 2),
    new Procedure(spark_fltk::labelfont, "label-font", 1, 2),
    new Procedure(spark_fltk::labelsize, "label-size", 1, 2),
    new Procedure(spark_fltk::labeltype, "label-type", 1, 2),
    new Procedure(spark_fltk::output, "output", 1),
    new Procedure(spark_fltk::parent, "parent", 1),
    new Procedure(spark_fltk::position, "position", 3),
    new Procedure(spark_fltk::redraw, "redraw", 1),
    new Procedure(spark_fltk::redraw_label, "redraw-label", 1),
    new Procedure(spark_fltk::resize, "resize", 5),
    new Procedure(spark_fltk::selection_color, "selection-color", 1, 2),
    new Procedure(spark_fltk::set_changed, "set-changed", 1),
    new Procedure(spark_fltk::set_output, "set-output", 1),
    new Procedure(spark_fltk::set_visible, "set-visible", 1),
    new Procedure(spark_fltk::set_visible_focus, "set-visible-focus", 1),
    new Procedure(spark_fltk::size, "size", 3),
    new Procedure(spark_fltk::take_focus, "take-focus", 1),
    new Procedure(spark_fltk::takesevents, "take-sevents", 1),
    new Procedure(spark_fltk::tooltip, "tooltip", 1, 2),
    new Procedure(spark_fltk::type, "type", 1, 2),
    new Procedure(spark_fltk::user_data, "user-data", 1, 2),
    new Procedure(spark_fltk::visible, "visible", 1),
    new Procedure(spark_fltk::visible_focus, "visible-focus", 1, 2),
    new Procedure(spark_fltk::visible_r, "visible-r", 1),
    new Procedure(spark_fltk::width, "width", 1),
    new Procedure(spark_fltk::when, "when", 1, 3),
    new Procedure(spark_fltk::x_pos, "x-pos", 1),
    new Procedure(spark_fltk::y_pos, "y-pos", 1),
    new Procedure(spark_fltk::fl_box, "fl-box", 5, 6),
    new Procedure(spark_fltk::dispose, "dispose-widget", 1),
    // Fl
    new Procedure(spark_fltk::focus, "focus", 0, 1),
    new Procedure(spark_fltk::selection_owner, "selection-owner", 0, 1),
    new Procedure(spark_fltk::run, "run", 0),
    new Procedure(spark_fltk::awake, "awake", 0),
    new Procedure(spark_fltk::start_thread_manager, 
		  "start-thread-manager", 0),
    new Procedure(spark_fltk::stop_thread_manager, 
		  "stop-thread-manager", 0),
    new Procedure(spark_fltk::args, "args", 1),
    new Procedure(spark_fltk::get_system_colors, "get-system-colors", 0),
    new Procedure(spark_fltk::foreground, "fl-foreground", 3),
    new Procedure(spark_fltk::background, "fl-background", 3),
    new Procedure(spark_fltk::contrast, "contrast", 2),
    new Procedure(spark_fltk::redraw_all, "redraw-all", 0),
    new Procedure(spark_fltk::scheme, "scheme", 0, 1),
    new Procedure(spark_fltk::set_fonts, "fl-set-fonts", 1),
    new Procedure(spark_fltk::get_font, "fl-get-font", 1),
    new Procedure(spark_fltk::get_font_attributes, 
		  "fl-get-font-attributes", 1),
    new Procedure(spark_fltk::get_font_name, 
		  "fl-get-font-name", 1),
    new Procedure(spark_fltk::get_font_sizes, 
		  "fl-get-font-sizes", 1),
    new Procedure(spark_fltk::box_dx, 
		  "box-dx", 1),
    new Procedure(spark_fltk::box_dy, 
		  "box-dy", 1),
    new Procedure(spark_fltk::box_dh, 
		  "box-dh", 1),
    new Procedure(spark_fltk::box_dw, 
		  "box-dw", 1),
    new Procedure(spark_fltk::rgb_color, 
		  "rgb-color", 1, 3),
    new Procedure(spark_fltk::check_threads, 
		  "check-threads", 0),
    0
  };
  spark::Status_code sc = spark::SUCCESS;
  if ((sc = spark::add_procedures(env, procedures, "spark-fltk"))
      != spark::SUCCESS)
    return sc;
  if ((sc = _add_browser_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_button_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_chart_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_clock_output_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_group_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_color_chooser_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_help_view_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_input_choice_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_pack_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_scroll_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_tabs_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_tile_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_wizard_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_spinner_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_text_display_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_text_buffer_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_window_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_input_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_menu_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_positioner_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_progress_procedures(env)) != spark::SUCCESS)
    return sc;
  /*
    if ((sc = _add_timer_procedures(env)) != spark::SUCCESS)
    return sc;
  */
  if ((sc = _add_valuator_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_image_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_file_chooser_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_help_dialog_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_event_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_widget_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_draw_procedures(env)) != spark::SUCCESS)
    return sc;
  if ((sc = _add_ask_procedures(env)) != spark::SUCCESS)
    return sc;
  return sc;
}

spark::Status_code
spark_fltk::initialize(Scheme_Env* env)
{
  Scheme_Object* module_symbol = NULL;
  Scheme_Env* new_env = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, module_symbol);
  MZ_GC_VAR_IN_REG(1, new_env);
  MZ_GC_REG();
  
  module_symbol = scheme_intern_symbol(MODULE_NAME);
  assert_scheme_object(module_symbol, "scheme_intern_symbol");
  new_env = scheme_primitive_module(module_symbol, env);
  assert_scheme_object(new_env, "scheme_primitive_module");
  spark::Status_code status = spark::SUCCESS;
  if ((status = _add_constants(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  if ((status = _add_procedures(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  scheme_finish_primitive_module(new_env);
  scheme_protect_primitive_provide(new_env, 0);

  MZ_GC_UNREG();
  return spark::SUCCESS;
}

// Exported fltk API

// Displays a widget.
Scheme_Object* 
spark_fltk::show(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->show();
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::activate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->activate();

  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::deactivate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->deactivate();

  // deactivate does not work in fltk
  // _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::active(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = scheme_false;
  if (widget->active())
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::active_r(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = scheme_false;
  if (widget->active_r())
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::align(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  if (argc == 1)
    {
      Fl_Align a = widget->align();
      int i = static_cast<int>(a);
      _ret_ = spark_fltk::align_to_intlist(i);
    }
  else
    {
      Fl_Align a = static_cast<Fl_Align>(spark_fltk::intlist_to_flag(argv[1]));
      widget->align(a);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::argument(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  Widget* w = reinterpret_cast<Widget*>(widget->argument());
  if (w)
    {
      if (argc == 1)
	{
	  _ret_ = w->argument;
	}
      else
	{
	  w->argument = argv[1];
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  if (argc == 1)
    {
      Fl_Boxtype b = widget->box();
      long i = static_cast<long>(b);
      _ret_ = scheme_make_integer(i);
    }
  else
    {
      long i = 0;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  Fl_Boxtype b = static_cast<Fl_Boxtype>(i);
	  widget->box(b);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::callback(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  Widget* w = reinterpret_cast<Widget*>(widget->argument());
  if (w)
    {
      if (argc == 1)
	{
	  _ret_ = (Scheme_Object*)w->callback;
	}
      else
	{
	  widget->callback(_generic_callback);
	  w->callback = argv[1];
	}
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_callback_widget(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  Widget* w = reinterpret_cast<Widget*>(widget->argument());
  if (w)
    {
      w->callback_widget = argv[1];
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = scheme_false;
  if (widget->changed())
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::clear_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->clear_changed();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::clear_output(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->clear_output();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::clear_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->clear_visible();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::clear_visible_focus(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  widget->clear_visible_focus();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  if (argc == 1)
    {
      Fl_Color c = widget->color();
      long i = static_cast<long>(c);
      _ret_ = scheme_make_integer(i);
    }
  else
    {
      if (argc == 3)
	{
	  long i = 0;
	  Fl_Color c01 = FL_BLACK, c02 = FL_BLACK;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    c01 = static_cast<Fl_Color>(i);
	  if (spark::Utils::long_from_scheme_long(argv[2], i))
	    c02 = static_cast<Fl_Color>(i);
	  widget->color(c01, c02);
	  _ret_ = scheme_true;
	}
      else
	{
	  long i = 0;
	  Fl_Color c01 = FL_BLACK;
	  if (spark::Utils::long_from_scheme_long(argv[1], i))
	    c01 = static_cast<Fl_Color>(i);
	  widget->color(c01);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::contains(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  Fl_Widget* widget02;
  if ((widget02 = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  _ret_ = scheme_false;
  if (widget->contains(widget02))
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::copy_label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  if (argv[1] != scheme_null)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("copy-label", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string label = SCHEME_BYTE_STR_VAL(str);
      widget->copy_label(label.c_str());
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::damage(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  if (argc == 1)
    {
      _ret_ = scheme_false;
      if (widget->damage())
	_ret_ = scheme_true;
    }
  else if (argc == 2)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      widget->damage(static_cast<uchar>(i));
      _ret_ = scheme_true;
    }
  else if (argc == 6)
    {
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[1], c);
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[2], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[3], y);
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[4], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[5], h);
      widget->damage(static_cast<uchar>(c), x, y, w, h);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::deimage(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  if (argc == 1)
    {
      Fl_Image* i = widget->deimage();
      if (i)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_IMAGE_TAG);
	  MZ_GC_UNREG();
	  _ret_ = scheme_make_cptr(i, tag);
	}
    }
  else
    {
      Fl_Image* i = _scheme_object_to_image(argc, argv, 1);
      if (i)
	{
	  widget->deimage(i);
	  _ret_ = scheme_true;
	}	
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::do_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  if (argc == 1)
    {
      widget->do_callback(widget, reinterpret_cast<void*>(argv[1]));
      _ret_ = scheme_true;
    }
  else
    {
      widget->do_callback(widget);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::height(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  short h = widget->h();
  _ret_ = scheme_make_integer(h);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::hide(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->hide();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::image(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_Image* i = widget->image();
      if (i)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_IMAGE_TAG);
	  _ret_ = scheme_make_cptr(i, tag);
	  MZ_GC_UNREG();
	}
    }
  else
    {
      Fl_Image* i = _scheme_object_to_image(argc, argv, 1);
      if (i)
	{
	  widget->image(i);
	  _ret_ = scheme_true;
	}	
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::inside(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  Fl_Widget* widget02;
  if ((widget02 = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  _ret_ = scheme_false;
  if (widget->inside(widget02))
    _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::label(int argc, Scheme_Object** argv)
{
  if (argc == 1)
    {
      DEFAULT_RET_INIT;
      Fl_Widget* widget;
      if ((widget = _get_widget(argc, argv, 0)) == 0)
	{
	  DEFAULT_RET_FINISH;
	}
      
      const char* lbl = widget->label();
      if (lbl)
	_ret_ = scheme_make_utf8_string(lbl);
      DEFAULT_RET_FINISH;
    }
    
  return copy_label(argc, argv);
}

Scheme_Object* 
spark_fltk::labelcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_Color c = widget->labelcolor();
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
	  widget->labelcolor(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::labelfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_Font f = widget->labelfont();
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
	  widget->labelfont(f);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::labelsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      uchar c = widget->labelsize();
      long i = static_cast<long>(c);
      _ret_ = scheme_make_integer(i);
    }
  else
    {
      long i = 0;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  widget->labelsize(static_cast<uchar>(i));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::labeltype(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_Labeltype lt = widget->labeltype();
      long i = static_cast<long>(lt);
      _ret_ = scheme_make_integer(i);
    }
  else
    {
      long i = 0;
      Fl_Labeltype lt;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  lt = static_cast<Fl_Labeltype>(i);
	  widget->labeltype(lt);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::output(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->output();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::parent(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  Fl_Widget* p = widget->parent();
  if (p)
    {
      Scheme_Object* tag = 0;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, tag);
      MZ_GC_REG();
      tag = scheme_make_integer(FL_WIDGET_TAG);
      _ret_ = scheme_make_cptr(p, tag);
      MZ_GC_UNREG();
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  short x = 0;
  short y = 0;
  long i = 0;
  if (spark::Utils::long_from_scheme_long(argv[1], i))
    x = static_cast<short>(i);
  if (spark::Utils::long_from_scheme_long(argv[2], i))
    y = static_cast<short>(i);
  widget->position(x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::redraw(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->redraw();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::redraw_label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->redraw_label();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::resize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  int x = 0;
  int y = 0;
  int w = 0;
  int h = 0;
  long i = 0;
  if (spark::Utils::long_from_scheme_long(argv[1], i))
    x = static_cast<int>(i);
  if (spark::Utils::long_from_scheme_long(argv[2], i))
    y = static_cast<int>(i);
  if (spark::Utils::long_from_scheme_long(argv[3], i))
    w = static_cast<int>(i);
  if (spark::Utils::long_from_scheme_long(argv[4], i))
    h = static_cast<int>(i);
  widget->resize(x, y, w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::selection_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_Color c = widget->selection_color();
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
	  widget->selection_color(c);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->set_changed();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_output(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->set_output();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->set_visible();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_visible_focus(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  widget->set_visible_focus();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  short w = 0;
  short h = 0;
  long i = 0;
  if (spark::Utils::long_from_scheme_long(argv[1], i))
    w = static_cast<short>(i);
  if (spark::Utils::long_from_scheme_long(argv[2], i))
    h = static_cast<short>(i);
  widget->size(w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::take_focus(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  widget->take_focus();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::takesevents(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  widget->takesevents();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::tooltip(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  if (argc == 1)
    {
      const char* tt = widget->tooltip();
      if (tt)
	_ret_ = scheme_make_utf8_string(tt);
    }
  else
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("tooltip", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string tt = SCHEME_BYTE_STR_VAL(str);
      widget->tooltip(tt.c_str());
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  if (argc == 1)
    {
      int t = static_cast<int>(widget->type());
      _ret_ = scheme_make_integer(t);
    }
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      widget->type(static_cast<uchar>(i));
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::user_data(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  
  if (argc == 1)
    {
      void* ud = widget->user_data();
      if (ud)
	_ret_ = reinterpret_cast<Scheme_Object*>(ud);
    }
  else
    {
      widget->user_data(reinterpret_cast<void*>(argv[1]));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  widget->visible();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::visible_focus(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  if (argc == 1)
    {
      int r = widget->visible_focus();
      _ret_ = scheme_make_integer(r);
    }
  else
    {
      long i = 0;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  widget->visible_focus(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::visible_r(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  widget->visible_r();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }

  short w = widget->w();
  _ret_ = scheme_make_integer(w);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::when(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  if (argc == 1)
    {
      Fl_When w = widget->when();
      long i = static_cast<long>(w);
      _ret_ = scheme_make_integer(i);
    }
  else if (argc == 2)
    {
      long i = 0;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  widget->when(static_cast<Fl_When>(i));
	  _ret_ = scheme_true;
	}
    }
  else if (argc == 3)
    {
      long i = 0;
      if (spark::Utils::long_from_scheme_long(argv[1], i))
	{
	  int curr_w = static_cast<int>(widget->when());
	  if (argv[2] == scheme_true) // remove
	    curr_w &= ~i;
	  else
	    curr_w |= i;
	  widget->when(static_cast<Fl_When>(curr_w));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::x_pos(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  short x = widget->x();
  _ret_ = scheme_make_integer(x);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::y_pos(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  

  short y = widget->y();
  _ret_ = scheme_make_integer(y);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::fl_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Box* box = 0;
  int x = 0; int y = 0; int w = 0; int h = 0;
  spark::Utils::int_from_scheme_long(argv[0], x);
  spark::Utils::int_from_scheme_long(argv[1], y);
  spark::Utils::int_from_scheme_long(argv[2], w);
  spark::Utils::int_from_scheme_long(argv[3], h);
  std::string title;
  if (argv[4] != scheme_null)
    {
      if (!SCHEME_CHAR_STRINGP(argv[4]))
	scheme_wrong_type("fl-box", "string", 4, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[4]);
      title = SCHEME_BYTE_STR_VAL(str);
    }
  if (argc == 5)
    box = new Fl_Box(x, y, w, h);
  else
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[5], i);
      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
      box = new Fl_Box(bt, x, y, w, h, 0);
    }
  if (box)
    {
      if (title.length() > 0)
	{
	  box->copy_label(title.c_str());
	}
      Fltk_tag t = FL_WIDGET_TAG;
      spark_fltk::Widget* widget = new spark_fltk::Widget;
      box->argument(reinterpret_cast<long>(widget));
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(box, tag);
	MZ_GC_UNREG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::dispose(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, 0)) == 0)
    {
      DEFAULT_RET_FINISH;
    }
  Widget* w = reinterpret_cast<Widget*>(widget->argument());
  delete w;
  delete widget;
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// Fl

// Runs the fltk event loop.
Scheme_Object* 
spark_fltk::run(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl::lock();
  Fl::run();
  Fl::unlock();

  Scheme_Env* env = (Scheme_Env*)scheme_make_namespace(0, NULL);
  scheme_eval_string("(exit)", env);

  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

bool _check_threads = false;

static void*
_awake_callback(void*)
{
  Fl::thread_manager_running(true);
  long micros = 20 * 1000;
  while (_check_threads)
    {
      Fl::awake();
      usleep(micros);
    }
  Fl::thread_manager_running(false);
  return 0;
}

Scheme_Object* 
spark_fltk::start_thread_manager(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!_check_threads)
    {
      pthread_t pt = 0;
      _check_threads = true;
      pthread_create(&pt, 0, _awake_callback, 0);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::stop_thread_manager(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (_check_threads)
    {
      _check_threads = false;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::awake(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl::awake();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::focus(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    {
      Fl_Widget* w = Fl::focus();
      if (w)
	{
	  Fltk_tag t = FL_WIDGET_TAG;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(t);
	    _ret_ = scheme_make_cptr(w, tag);
	    MZ_GC_UNREG();
	  }
	}
    }
  else
    {
      Fl_Widget* widget;
      if ((widget = _get_widget(argc, argv, 0)) == 0)
	{
	  DEFAULT_RET_FINISH;
	}
      
      if (widget)
	{
	  Fl::focus(widget);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::selection_owner(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    {
      Fl_Widget* widget = Fl::selection_owner();
      if (widget)
	{
	  Fltk_tag t = FL_WIDGET_TAG;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(t);
	    _ret_ = scheme_make_cptr(widget, tag);
	    MZ_GC_UNREG();
	  }
	}
    }
  else
    {
      Fl_Widget* widget;
      if ((widget = _get_widget(argc, argv, 0)) == 0)
	{
	  DEFAULT_RET_FINISH;
	}
      
      if (widget)
	{
	  Fl::focus(widget);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::args(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_LISTP(argv[0]))
    scheme_wrong_type("args", "list", 0, argc, argv);
  int ac = scheme_list_length(argv[0]);
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
      Scheme_Object* lst = argv[0];
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
		scheme_wrong_type("args", "string", 0, argc, argv);
	      Scheme_Object* str = scheme_char_string_to_byte_string(obj);
	      std::string tmp = SCHEME_BYTE_STR_VAL(str);
	      av[1] = new char[tmp.size() + 1];
	      strcpy(av[1], tmp.c_str());
	    }
	  else
	    {
	      av[1] = new char[2];
	      strcpy(av[1], "");
	    }
	  ++i;
	  lst = SCHEME_CDR(lst);
	}
    }
  Fl::args(ac, av);
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::get_system_colors(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl::get_system_colors();

  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::foreground(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int r = 0;
  spark::Utils::int_from_scheme_long(argv[0], r);
  int g = 0;
  spark::Utils::int_from_scheme_long(argv[1], g);
  int b = 0;
  spark::Utils::int_from_scheme_long(argv[2], b);
  Fl::foreground(static_cast<uchar>(r),
		 static_cast<uchar>(g),
		 static_cast<uchar>(b));

  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::background(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int r = 0;
  spark::Utils::int_from_scheme_long(argv[0], r);
  int g = 0;
  spark::Utils::int_from_scheme_long(argv[1], g);
  int b = 0;
  spark::Utils::int_from_scheme_long(argv[2], b);
  Fl::background(static_cast<uchar>(r),
		 static_cast<uchar>(g),
		 static_cast<uchar>(b));

  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::contrast(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int a = 0;
  spark::Utils::int_from_scheme_long(argv[0], a);
  int b = 0;
  spark::Utils::int_from_scheme_long(argv[1], b);
  int c = static_cast<int>(fl_contrast(static_cast<Fl_Color>(a),
					static_cast<Fl_Color>(b)));
  _ret_ = scheme_make_integer(c);

  DEFAULT_RET_FINISH;
}

// Redraw all widgets.
Scheme_Object* 
spark_fltk::redraw_all(int, Scheme_Object**)
{
  DEFAULT_RET_INIT;


  Fl::redraw();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::scheme(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;


  if (argc == 1)
    {
      std::string s;
      if (SCHEME_CHAR_STRINGP(argv[0]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
	  s = SCHEME_BYTE_STR_VAL(str);
	}
      Fl::scheme(s.c_str());
    }
  else
    {
      const char* s = Fl::scheme();
      if (s)
	_ret_ = scheme_make_utf8_string(s);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::set_fonts(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  std::string s;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      s = SCHEME_BYTE_STR_VAL(str);      
    }
  _ret_ = scheme_make_integer(Fl::set_fonts(s.c_str()));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::get_font_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  const char* n = Fl::get_font_name((Fl_Font)i);
  if (n)
    _ret_ = scheme_make_utf8_string(n);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::get_font_attributes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  int a = 0;
  Fl::get_font_name((Fl_Font)i, &a);
  const int count = 2;
  Scheme_Object** elems = new Scheme_Object*[count];
  {
    MZ_GC_DECL_REG(count);
    for (int i=0; i<count; ++i)
      {
	elems[i] = NULL;
	MZ_GC_VAR_IN_REG(i, elems[i]);
	elems[i] = scheme_null;
      }
    MZ_GC_REG();
    if (a & FL_BOLD)
      elems[0] = scheme_intern_symbol("bold");
    if (a & FL_ITALIC)
      elems[0] = scheme_intern_symbol("italic");
    _ret_ = scheme_build_list(count, elems);
    delete[] elems;
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::get_font_sizes(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  int* a;
  const int count = Fl::get_font_sizes((Fl_Font)i, a);
  if (count > 0)
    {
      Scheme_Object** elems = new Scheme_Object*[count];
      {
	MZ_GC_DECL_REG(count);
	for (int i=0; i<count; ++i)
	  {
	    elems[i] = NULL;
	    MZ_GC_VAR_IN_REG(i, elems[i]);
	    elems[i] = scheme_make_integer(a[i]);
	  }
	MZ_GC_REG();
	_ret_ = scheme_build_list(count, elems);
	MZ_GC_UNREG();
	delete[] elems;
      }
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::get_font(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  const char* n = Fl::get_font((Fl_Font)i);
  if (n)
    _ret_ = scheme_make_utf8_string(n);

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::box_dx(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = scheme_make_integer(Fl::box_dx(static_cast<Fl_Boxtype>(i)));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::box_dy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = scheme_make_integer(Fl::box_dy(static_cast<Fl_Boxtype>(i)));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::box_dh(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = scheme_make_integer(Fl::box_dh(static_cast<Fl_Boxtype>(i)));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::box_dw(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  _ret_ = scheme_make_integer(Fl::box_dw(static_cast<Fl_Boxtype>(i)));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::rgb_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 1)
    {
      int g = 0;
      spark::Utils::int_from_scheme_long(argv[0], g);
      Fl_Color c = fl_rgb_color(static_cast<uchar>(g));
      _ret_ = scheme_make_integer(static_cast<int>(c));
    }
  else
    {
      int r = 0;
      spark::Utils::int_from_scheme_long(argv[0], r);
      int g = 0;
      spark::Utils::int_from_scheme_long(argv[1], g);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[2], b);
      Fl_Color c = fl_rgb_color(static_cast<uchar>(r),
				static_cast<uchar>(g),
				static_cast<uchar>(b));
      _ret_ = scheme_make_integer(static_cast<int>(c));
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk::check_threads(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  scheme_check_threads();
  
  DEFAULT_RET_FINISH;
}


spark_fltk::Fltk_tag
spark_fltk::_get_tag(int argc, Scheme_Object** argv, 
		     int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_get_tag", "cptr", 
			idx, argc, argv);
      return FL_INVALID_TAG;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_get_tag", "tag", 
			idx, argc, argv);
      return FL_INVALID_TAG;
    }  
  Fltk_tag tag = static_cast<Fltk_tag>(i);
  return tag;
}

Fl_Image*
spark_fltk::_scheme_object_to_image(int argc, 
				    Scheme_Object** argv,
				    int idx)	
{
 
  Fltk_tag tag = _get_tag(argc, argv, idx);
  if (tag != FL_IMAGE_TAG)
      {
	scheme_wrong_type("_scheme_object_to_image", "tag", 
			  idx, argc, argv);
	return 0;
      }
  void* p = SCHEME_CPTR_VAL(argv[idx]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_image", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<Fl_Image*>(p);
}

Fl_Widget*
spark_fltk::_get_widget(int argc, Scheme_Object** argv, 
			int idx)
{
  void* p = SCHEME_CPTR_VAL(argv[idx]);
  if (!p)
    {
      scheme_wrong_type("_get_widget", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<Fl_Widget*>(p);
}

void
spark_fltk::_generic_callback(Fl_Widget* w, void* p)
{
  Widget* widget = reinterpret_cast<Widget*>(p);
  if (widget)
    {
      if (widget->callback)
	{
	  if (widget->callback == scheme_null)
	    return;
	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  if (widget->callback_widget)
	    args[0] = widget->callback_widget;
	  else
	    {
	      Scheme_Object* tag = 0;
	      MZ_GC_DECL_REG(1);
	      MZ_GC_VAR_IN_REG(0, tag);
	      MZ_GC_REG();	  
	      Fltk_tag t = FL_WIDGET_TAG;
	      tag = scheme_make_integer(t);
	      args[0] = scheme_make_cptr(w, tag);
	      MZ_GC_UNREG();	  
	    }
	  args[1] = widget->argument;
	  scheme_apply(widget->callback, arg_count, args);
	}
    }
}

Scheme_Object* 
spark_fltk::align_to_intlist(int a)
{
  const int count = 9;
  Scheme_Object** elems = new Scheme_Object*[count];
  MZ_GC_DECL_REG(count);
  for (int i=0; i<count; ++i)
    {
      elems[i] = NULL;
      MZ_GC_VAR_IN_REG(i, elems[i]);
      elems[i] = scheme_null;
    }
  MZ_GC_REG();
  const int aligns[count] = {FL_ALIGN_BOTTOM, FL_ALIGN_CENTER,
			     FL_ALIGN_CLIP, FL_ALIGN_INSIDE,
			     FL_ALIGN_LEFT, FL_ALIGN_RIGHT,
			     FL_ALIGN_TEXT_OVER_IMAGE,
			     FL_ALIGN_TOP, FL_ALIGN_WRAP};
  for (int i=0; i<count; ++i)
    {
      if ((a & aligns[i]) == aligns[i])
	elems[i] = scheme_make_integer(aligns[i]);
    }
  MZ_GC_UNREG();
  Scheme_Object* ret = scheme_build_list(count, elems);
  delete[] elems;
  return ret;
}

int 
spark_fltk::intlist_to_flag(Scheme_Object* lst)
{
  Scheme_Object* sym = SCHEME_CAR(lst);
  int i = 0;
  int tmp = 0;
  std::string s = "";
  while (sym)
    {
      if (sym == scheme_null)
	break;
      tmp = 0;
      if (spark::Utils::int_from_scheme_long(sym, tmp))
	i |= tmp;
      lst = SCHEME_CDR(lst);
      if (!lst)
	break;
      if (lst == scheme_null)
	break;
      sym = SCHEME_CAR(lst);
    }
  return i;
}

