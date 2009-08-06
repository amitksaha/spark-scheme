// MzScheme inetrface to the FLTK Menu.
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

#include <FL/Fl_Menu_.H>
#include <FL/Fl_Menu_Item.H>
#include <FL/Fl_Choice.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Menu_Button.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_menu
{
  static Scheme_Object* fl_menu(int, Scheme_Object**);
  static Scheme_Object* fl_menu_item(int, Scheme_Object**);
  static Scheme_Object* fl_menu_bar(int, Scheme_Object**);
  // Fl_Menu_
  static Scheme_Object* add(int, Scheme_Object**);
  static Scheme_Object* find_item(int, Scheme_Object**);
  static Scheme_Object* global(int, Scheme_Object**);
  static Scheme_Object* item_pathname(int, Scheme_Object**);
  static Scheme_Object* clear(int, Scheme_Object**);
  static Scheme_Object* mode(int, Scheme_Object**);
  static Scheme_Object* remove(int, Scheme_Object**);
  static Scheme_Object* replace(int, Scheme_Object**);
  static Scheme_Object* size(int, Scheme_Object**);
  static Scheme_Object* test_shortcut(int, Scheme_Object**);
  static Scheme_Object* text(int, Scheme_Object**);
  static Scheme_Object* textcolor(int, Scheme_Object**);
  static Scheme_Object* textfont(int, Scheme_Object**);
  static Scheme_Object* textsize(int, Scheme_Object**);
  static Scheme_Object* mvalue(int, Scheme_Object**);
  static Scheme_Object* down_box(int, Scheme_Object**);
  static Scheme_Object* copy(int, Scheme_Object**);
  static Scheme_Object* copy_from(int, Scheme_Object**);
  // Fl_Menu_Item
  static Scheme_Object* label(int, Scheme_Object**);
  static Scheme_Object* labeltype(int, Scheme_Object**);
  static Scheme_Object* labelsize(int, Scheme_Object**);
  static Scheme_Object* labelfont(int, Scheme_Object**);
  static Scheme_Object* labelcolor(int, Scheme_Object**);
  static Scheme_Object* callback(int, Scheme_Object**);
  static Scheme_Object* set_callback_widget(int, Scheme_Object**);
  static Scheme_Object* user_data(int, Scheme_Object**);
  static Scheme_Object* do_callback(int, Scheme_Object**);
  static Scheme_Object* menu_item_shortcut(int, Scheme_Object**);
  static Scheme_Object* submenu(int, Scheme_Object**);
  static Scheme_Object* checkbox(int, Scheme_Object**);
  static Scheme_Object* radio(int, Scheme_Object**);
  static Scheme_Object* menu_item_value(int, Scheme_Object**);
  static Scheme_Object* check(int, Scheme_Object**);
  static Scheme_Object* check_only(int, Scheme_Object**);
  static Scheme_Object* uncheck(int, Scheme_Object**);
  static Scheme_Object* is_visible(int, Scheme_Object**);
  static Scheme_Object* set_visible(int, Scheme_Object**);
  static Scheme_Object* is_active(int, Scheme_Object**);
  static Scheme_Object* activate(int, Scheme_Object**);
  static Scheme_Object* deactivate(int, Scheme_Object**);
  static Scheme_Object* popup(int, Scheme_Object**);
  static Scheme_Object* pulldown(int, Scheme_Object**);
  static Scheme_Object* menu_item_size(int, Scheme_Object**);
  static Scheme_Object* next(int, Scheme_Object**);
  static Scheme_Object* menu_item_test_shortcut(int, Scheme_Object**);
  // Fl_Choice
  static Scheme_Object* clear_changed(int, Scheme_Object**);
  static Scheme_Object* changed(int, Scheme_Object**);
  static Scheme_Object* choice_down_box(int, Scheme_Object**);
  static Scheme_Object* set_changed(int, Scheme_Object**);
  static Scheme_Object* choice_value(int, Scheme_Object**);
  // Fl_Menu_Button
  static Scheme_Object* menu_button_popup(int, Scheme_Object**);
  static Scheme_Object* menu_button_type(int, Scheme_Object**);
}

spark::Status_code
spark_fltk::_add_menu_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_menu::fl_menu, "fl-menu", 6),
    new Procedure(spark_fltk_menu::fl_menu_bar, "fl-menu-bar", 5),
    new Procedure(spark_fltk_menu::fl_menu_item, "fl-menu-item", 
		  5, 9),
    // Fl_Menu_
    new Procedure(spark_fltk_menu::add, "add-menu-item", 6),
    new Procedure(spark_fltk_menu::find_item, "find-menu-item", 2),
    new Procedure(spark_fltk_menu::global, "global", 1),
    new Procedure(spark_fltk_menu::item_pathname, "menu-item-path", 1, 2),
    new Procedure(spark_fltk_menu::clear, "clear-menu", 1),
    new Procedure(spark_fltk_menu::mode, "menu-item-mode", 3),
    new Procedure(spark_fltk_menu::remove, "remove-menu-item", 2),
    new Procedure(spark_fltk_menu::replace, "replace-menu-item", 3),
    new Procedure(spark_fltk_menu::size, "count-menu-items", 1),
    new Procedure(spark_fltk_menu::test_shortcut, "test-shortcut", 1),
    new Procedure(spark_fltk_menu::text, "menu-item-text", 1, 2),
    new Procedure(spark_fltk_menu::textcolor, "menu-item-text-color", 1, 2),
    new Procedure(spark_fltk_menu::textfont, "menu-item-text-font", 1, 2),
    new Procedure(spark_fltk_menu::textsize, "menu-item-text-size", 1, 2),
    new Procedure(spark_fltk_menu::mvalue, "mvalue", 1),
    new Procedure(spark_fltk_menu::down_box, "file-input-down-box", 1, 2),
    new Procedure(spark_fltk_menu::copy, "copy-menu-items", 2),
    new Procedure(spark_fltk_menu::copy_from, "copy-menu-items-from", 2),
    // Fl_Menu_Item
    new Procedure(spark_fltk_menu::label, "menu-item-label", 1, 2),
    new Procedure(spark_fltk_menu::labeltype, "menu-item-label-type", 1, 2),
    new Procedure(spark_fltk_menu::labelsize, "menu-item-label-size", 1, 2),
    new Procedure(spark_fltk_menu::labelfont, "menu-item-label-font", 1, 2),
    new Procedure(spark_fltk_menu::labelcolor, "menu-item-label-color", 1, 2),
    new Procedure(spark_fltk_menu::callback, "menu-item-callback", 3),
    new Procedure(spark_fltk_menu::set_callback_widget, 
		  "set-menu-item-callback-widget!", 2),
    new Procedure(spark_fltk_menu::user_data, "menu-item-user-data", 1, 2),
    new Procedure(spark_fltk_menu::do_callback, "do-callback", 2, 3),
    new Procedure(spark_fltk_menu::menu_item_shortcut, 
		  "menu-item-shortcut", 1, 2),
    new Procedure(spark_fltk_menu::submenu, "submenu", 1),
    new Procedure(spark_fltk_menu::checkbox, "checkbox", 1),
    new Procedure(spark_fltk_menu::radio, "radio", 1),
    new Procedure(spark_fltk_menu::menu_item_value, "menu-item-value", 1),
    new Procedure(spark_fltk_menu::check, "menu-item-check", 1),
    new Procedure(spark_fltk_menu::check_only, "menu-item-check-only", 1),
    new Procedure(spark_fltk_menu::uncheck, "menu-item-uncheck", 1),
    new Procedure(spark_fltk_menu::is_visible, "menu-item-is-visible", 1),
    new Procedure(spark_fltk_menu::set_visible, "menu-item-set-visible", 1),
    new Procedure(spark_fltk_menu::is_active, "menu-item-is-active", 1),
    new Procedure(spark_fltk_menu::activate, "menu-item-activate", 1),
    new Procedure(spark_fltk_menu::deactivate, "menu-item-deactivate", 1),
    new Procedure(spark_fltk_menu::popup, "popup", 3, 6),
    new Procedure(spark_fltk_menu::pulldown, "pulldown", 5, 9),
    new Procedure(spark_fltk_menu::menu_item_size, "menu-item-size", 1),
    new Procedure(spark_fltk_menu::next, "menu-item-next", 1),
    new Procedure(spark_fltk_menu::menu_item_test_shortcut, 
		  "menu-item-test-shortcut", 1),
    // Fl_Choice
    new Procedure(spark_fltk_menu::clear_changed, "choice-clear-changed", 1),
    new Procedure(spark_fltk_menu::changed, "choice-changed", 1),
    new Procedure(spark_fltk_menu::set_changed, "choice-set-changed", 1),
    new Procedure(spark_fltk_menu::choice_value, "choice-value", 1, 2),
    new Procedure(spark_fltk_menu::choice_down_box, "choice-down-box", 1, 2),
    // Fl_Menu_Button
    new Procedure(spark_fltk_menu::menu_button_popup, 
		  "menu-button-popup", 5, 6),
    new Procedure(spark_fltk_menu::menu_button_type, 
		  "menu-button-type", 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_menu_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    Constant("FL-MENU-INACTIVE", FL_MENU_INACTIVE),
    Constant("FL-MENU-TOGGLE", FL_MENU_TOGGLE),
    Constant("FL-MENU-VALUE", FL_MENU_VALUE),
    Constant("FL-MENU-RADIO", FL_MENU_RADIO),
    Constant("FL-MENU-INVISIBLE", FL_MENU_INVISIBLE),
    Constant("FL-SUBMENU-POINTER", FL_SUBMENU_POINTER),
    Constant("FL-SUBMENU", FL_SUBMENU),
    Constant("FL-MENU-DIVIDER", FL_MENU_DIVIDER),
    Constant("FL-MENU-HORIZONTAL", FL_MENU_HORIZONTAL),
    Constant("FL-MENU-POPUP1", Fl_Menu_Button::POPUP1),
    Constant("FL-MENU-POPUP2", Fl_Menu_Button::POPUP2),
    Constant("FL-MENU-POPUP3", Fl_Menu_Button::POPUP3),
    Constant("FL-MENU-POPUP12", Fl_Menu_Button::POPUP12),
    Constant("FL-MENU-POPUP13", Fl_Menu_Button::POPUP13),
    Constant("FL-MENU-POPUP23", Fl_Menu_Button::POPUP23),
    Constant("FL-MENU-POPUP123", Fl_Menu_Button::POPUP123),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

Scheme_Object* 
spark_fltk_menu::fl_menu(int argc, Scheme_Object** argv)
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
  Fl_Menu_* menu = 0;
  if (!SCHEME_SYMBOLP(argv[5]))
    scheme_wrong_type("fl-menu", "symbol", 5, argc, argv);
  std::string s = SCHEME_SYM_VAL(argv[5]);
  if (s == "choice")
    menu = new Fl_Choice(x, y, w, h);
  else if (s == "bar")
    menu = new Fl_Menu_Bar(x, y, w, h);
  else if (s == "button")
    menu = new Fl_Menu_Button(x, y, w, h);
  else
    {
      DEFAULT_RET_FINISH;
    }
  if (title.length() > 0)
    menu->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  menu->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(menu, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::fl_menu_bar(int argc, Scheme_Object** argv)
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
  Fl_Menu_Bar* menu_bar = new Fl_Menu_Bar(x, y, w, h);
  if (title.length() > 0)
    menu_bar->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  menu_bar->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(widget, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::fl_menu_item(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  char* title = 0;
  if (SCHEME_CHAR_STRINGP(argv[0]))
    {
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      std::string s = SCHEME_BYTE_STR_VAL(str);
      const char* tmp = spark::Global_strings::contains(s.c_str());
      if (tmp)
	title = const_cast<char*>(tmp);
      else
	{
	  size_t len = s.length();
	  title = new char[len + 1];
	  strcpy(title, s.c_str());
	  spark::Global_strings::add(title);
	}
    }
  long sc = 0;
  spark::Utils::long_from_scheme_long(argv[1], sc);
  Scheme_Object* cb = argv[2];
  Scheme_Object* ud = argv[3];
  Fltk_tag t = FL_WIDGET_TAG;
  Fl_Menu_Item* menu_item = 0;
  Widget* widget= 0;
  menu_item = new Fl_Menu_Item;
  widget = new spark_fltk::Widget;
  menu_item->text = title;
  menu_item->shortcut_ = static_cast<ulong>(sc);
  menu_item->callback_ = _generic_callback;
  menu_item->user_data_ = reinterpret_cast<void*>(widget);
  widget->callback = cb;
  widget->argument = ud;
  menu_item->flags = 0;
  Scheme_Object* lst = argv[4];
  while (lst)
    {
      Scheme_Object* obj = SCHEME_CAR(lst);
      if (obj != scheme_null)
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(obj, i))
	    menu_item->flags |= i;
	}
      lst = SCHEME_CDR(lst);
      if (lst == scheme_null)
	break;
    }
  if (argc == 9)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[5], i);
      menu_item->labeltype_ = static_cast<uchar>(i);
      i = 0;
      spark::Utils::int_from_scheme_long(argv[6], i);
      menu_item->labelfont_ = static_cast<uchar>(i);
      i = 0;
      spark::Utils::int_from_scheme_long(argv[7], i);
      menu_item->labelsize_ = static_cast<uchar>(i);
      i = 0;
      spark::Utils::int_from_scheme_long(argv[8], i);
      menu_item->labelcolor_ = static_cast<uchar>(i);
    }
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(menu_item, tag);
  }

  DEFAULT_RET_FINISH;
}

static Fl_Menu_* _get_fl_menu(int, Scheme_Object**, int);
static Fl_Menu* _get_fl_menu_item(int, Scheme_Object**, int);
static Fl_Choice* _get_fl_choice(int, Scheme_Object**, int);
static Fl_Menu_Button* _get_fl_menu_button(int, Scheme_Object**, int);

typedef std::map<int, Widget*> Int_widget_map;

Scheme_Object* 
spark_fltk_menu::add(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (!menu)
    {
      DEFAULT_RET_FINISH;
    }
  if (menu)
    {
      std::string label;
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  label = SCHEME_BYTE_STR_VAL(str);
	}
      else
	{
	  DEFAULT_RET_FINISH;
	}
      int sc = 0;
      spark::Utils::int_from_scheme_long(argv[2], sc);
      Widget* widget = new spark_fltk::Widget;
      widget->callback = argv[3];
      widget->argument = argv[4];
      int flags = spark::Utils::flag_from_list(argv[5]);
      int index = menu->add(label.c_str(),
			    sc,
			    0, 0,
			    flags);
      
      const Fl_Menu_Item* menu_items = menu->menu();
      if (menu_items)
	{
	  Fl_Menu_Item* menu_item = 
	    const_cast<Fl_Menu_Item*>(&menu_items[index]);
	  menu_item->callback_ = _generic_callback;
	  menu_item->user_data_ = reinterpret_cast<void*>(widget);
	  _ret_ = scheme_true;
	}
      else
	delete widget;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      menu->clear();
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::global(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      menu->global();
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::find_item(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string item_name = SCHEME_BYTE_STR_VAL(str);
	  const Fl_Menu_Item* mi = menu->find_item(item_name.c_str());
	  if (mi)
	    {
	      Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(mi);
	      Widget* widget = reinterpret_cast<Widget*>(item->user_data_);
	      if (widget)
		{
		  Fltk_tag t = FL_WIDGET_TAG;
		  {
		    Scheme_Object* tag = 0;
		    MZ_GC_DECL_REG(1);
		    MZ_GC_VAR_IN_REG(0, tag);
		    MZ_GC_REG();
		    tag = scheme_make_integer(t);
		    MZ_GC_UNREG();
		    _ret_ = scheme_make_cptr(item, tag);
		  }
		}
	    }
	}
      
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::test_shortcut(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menu->test_shortcut());
      if (!item)
	{
	  DEFAULT_RET_FINISH;
	}
      Fltk_tag t = FL_WIDGET_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	MZ_GC_UNREG();
	_ret_ = scheme_make_cptr(item, tag);
      }	  
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::item_pathname(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  const int path_sz = 255;
	  char name[path_sz + 1];
	  if (menu->item_pathname(name, path_sz) == 0)
	    _ret_ = scheme_make_utf8_string(name);
	}
      else
	{
	  Fl_Menu_Item* item = _get_fl_menu_item(argc, argv, 1);
	  if (item)
	    {
	      const int path_sz = 255;
	      char name[path_sz + 1];
	      if (menu->item_pathname(name, path_sz, item) == 0)
		_ret_ = scheme_make_utf8_string(name);
	    }
	}

    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  const char* t = menu->text();
	  if (t)
	    _ret_ = scheme_make_utf8_string(t);
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  const char* t = menu->text(i);
	  if (t)
	    _ret_ = scheme_make_utf8_string(t);
	}

    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::mode(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[2], x);
      menu->mode(i, x);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::remove(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (SCHEME_CHAR_STRINGP(argv[1]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  std::string item_name = SCHEME_BYTE_STR_VAL(str);
	  const Fl_Menu_Item* mi = menu->find_item(item_name.c_str());
	  if (mi)
	    {
	      const Fl_Menu_Item* items = menu->menu();
	      if (items)
		{
		  int index = -1;
		  int size = menu->size();
		  for (int i=0; i<size; ++i)
		    {
		      if (mi == &items[i])
			{
			  index = i;
			  break;
			}
		    }
		  if (index >= 0)
		    {
		      menu->remove(index);
		      _ret_ = scheme_true;
		    }
		}
	    }
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      menu->remove(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::replace(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      if (SCHEME_CHAR_STRINGP(argv[2]))
	{
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	  std::string item_name = SCHEME_BYTE_STR_VAL(str);
	  menu->replace(i, item_name.c_str());
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::down_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = menu->down_box();
	  int i = static_cast<int>(bt);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
	      menu->down_box(bt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::copy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (SCHEME_LISTP(argv[1]))
	{
	  int len = scheme_list_length(argv[1]);
	  Fl_Menu_Item* menu_items = new Fl_Menu_Item[len + 1];
	  Scheme_Object* lst = argv[1];
	  for (int i=0; i<len; ++i)
	    {
	      Scheme_Object* obj = SCHEME_CAR(lst);
	      if (obj == scheme_null)
		menu_items[i].text = 0;
	      else
		{
		  void* p = SCHEME_CPTR_VAL(obj);
		  if (p)
		    {
		      Fl_Menu_Item* mi =
			reinterpret_cast<Fl_Menu_Item*>(p);
		      menu_items[i] = *mi;		      
		    }
		}	      
	      lst = SCHEME_CDR(lst);
	      if (lst == scheme_null)
		break;
	    }
	  menu_items[len].text = 0;
	  menu->copy(menu_items);
	  delete[] menu_items;
	  {
	    const Fl_Menu_Item* menu_items = menu->menu();
	    if (menu_items)
	      {
		int sz = menu->size();
		for (int i=0; i<sz; ++i)
		  {
		    if (menu_items[i].text == 0)
		      continue;
		    void* ud = menu_items[i].user_data_;
		    Widget* widget = 0;
		    if (ud)
		      {
			widget = reinterpret_cast<Widget*>(ud);			
		      }
		    else
		      {
			widget = new spark_fltk::Widget;
			Fl_Menu_Item* mi = const_cast<Fl_Menu_Item*>(&menu_items[i]);
			mi->user_data_ = reinterpret_cast<void*>(widget);
		      }
		  }
	      }
	  }
	  _ret_ = scheme_true;	  
	}
    }
 
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::copy_from(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
 
  Fl_Menu_* menu_dest = _get_fl_menu(argc, argv, 0);
  Fl_Menu_* menu_src = _get_fl_menu(argc, argv, 1);
  if (menu_dest && menu_src)
    {
      const Fl_Menu_Item* items = menu_src->menu();
      if (items)
	{
	  menu_dest->menu(items);
	  _ret_ = scheme_true;	  
	}
    }
 
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::textcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  Fl_Color v = menu->textcolor();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Color v = static_cast<Fl_Color>(i);
	      menu->textcolor(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::textfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  Fl_Font v = menu->textfont();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Font v = static_cast<Fl_Font>(i);
	      menu->textfont(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::textsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      if (argc == 1)
	{
	  uchar v = menu->textsize();
	  int i = static_cast<int>(v);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      uchar v = static_cast<uchar>(i);
	      menu->textsize(v);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    _ret_ = scheme_make_integer(menu->size());
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::mvalue(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_* menu = _get_fl_menu(argc, argv, 0);
  if (menu)
    {
      Fl_Menu_Item* mi = const_cast<Fl_Menu_Item*>(menu->mvalue());
      if (mi)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_WIDGET_TAG);
	  MZ_GC_UNREG();
	  _ret_ = scheme_make_cptr(mi, tag);
	}
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::label(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	{
	  const char* s = menu_item->label();
	  if (s)
	    _ret_ = scheme_make_utf8_string(s);
	}
      else
	{
	  if (SCHEME_CHAR_STRINGP(argv[1]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	      std::string s = SCHEME_BYTE_STR_VAL(str);
	      const char* tmp = spark::Global_strings::contains(s.c_str());
	      char* label = 0;
	      if (tmp)
		label = const_cast<char*>(tmp);
	      else
		{
		  size_t len = s.length();
		  label = new char[len + 1];
		  strcpy(label, s.c_str());
		  spark::Global_strings::add(label);
		}
	      menu_item->label(label);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::labelcolor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	{
	  Fl_Color i = menu_item->labelcolor();
	  _ret_ = scheme_make_integer(static_cast<int>(i));
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Color f = static_cast<Fl_Color>(i);
	  menu_item->labelcolor(f);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::labeltype(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	{
	  Fl_Labeltype i = menu_item->labeltype();
	  _ret_ = scheme_make_integer(static_cast<int>(i));
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Labeltype ft = static_cast<Fl_Labeltype>(i);
	  menu_item->labeltype(ft);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}


Scheme_Object*
spark_fltk_menu::labelsize(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(menu_item->labelsize());
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  menu_item->labelsize(i);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::labelfont(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	{
	  Fl_Font i = menu_item->labelfont();
	  _ret_ = scheme_make_integer(static_cast<int>(i));
	}
      else
	{
	  int i = 0;
	  spark::Utils::int_from_scheme_long(argv[1], i);
	  Fl_Font f = static_cast<Fl_Font>(i);
	  menu_item->labelfont(f);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Widget* widget = reinterpret_cast<Widget*>(menu_item->user_data_);
      if (widget)
	{
	  menu_item->callback_ = _generic_callback;
	  widget->callback = argv[1];
	  widget->argument = argv[2];
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::user_data(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Widget* widget = reinterpret_cast<Widget*>(menu_item->user_data_);
      if (widget)
	{
	  if (argc == 1)
	    {
	      _ret_ = widget->argument;
	    }
	  else
	    {
	      widget->argument = argv[1];
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::do_callback(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 2)
	{
	  menu_item->do_callback(0);
	  _ret_ = scheme_true;
	}
      else
	{
	  Widget* w = reinterpret_cast<Widget*>(menu_item->user_data_);
	  if (w)
	    {
	      w->argument = argv[2];
	      menu_item->do_callback(0);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::set_callback_widget(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Widget* w = reinterpret_cast<Widget*>(menu_item->user_data_);
      if (w)
	{
	  w->callback_widget = argv[1];
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::menu_item_shortcut(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argc == 1)
	_ret_ = scheme_make_integer(static_cast<int>(menu_item->shortcut()));
      else
	{
	  long i = 0;
	  spark::Utils::long_from_scheme_long(argv[1], i);
	  menu_item->shortcut(static_cast<ulong>(i));
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::submenu(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      _ret_ = menu_item->submenu() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::checkbox(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      _ret_ = menu_item->checkbox() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::radio(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      _ret_ = menu_item->radio() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::menu_item_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      int i = menu_item->value();
      _ret_ = scheme_make_integer(i) ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::check(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      menu_item->set();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::check_only(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      menu_item->setonly();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::uncheck(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      menu_item->clear();
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::is_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      _ret_ = menu_item->visible() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::set_visible(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      if (argv[1] == scheme_true)
	menu_item->show();
      else
	menu_item->hide();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::is_active(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      _ret_ = menu_item->active() ? scheme_true : scheme_false;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::activate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      menu_item->activate();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::deactivate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      menu_item->deactivate();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::popup(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Fl_Menu_Item* ret = 0;
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      if (x < 0)
	x = Fl::event_x();
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      if (y < 0)
	y = Fl::event_y();
      if (argc == 3)
	ret = const_cast<Fl_Menu_Item*>(menu_item->popup(x, y));
      else
	{
	  std::string title;
	  if (SCHEME_CHAR_STRINGP(argv[3]))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[3]);
	      title = SCHEME_BYTE_STR_VAL(str);
	    }	  
	  Fl_Menu_Item* picked = 0;
	  if (argv[4] != scheme_null)
	    picked = _get_fl_menu_item(argc, argv, 4);
	  Fl_Menu_* button = 0;
	  if (argv[5] != scheme_null)
	    button = _get_fl_menu(argc, argv, 5);
	  ret = 
	    const_cast<Fl_Menu_Item*>(menu_item->popup(x, y, title.c_str(), 
						       picked, button));
	  {
	    Widget* widget = reinterpret_cast<Widget*>(ret->user_data_);
	    if (widget)
	      {
		Scheme_Object* tag = 0;
		MZ_GC_DECL_REG(1);
		MZ_GC_VAR_IN_REG(0, tag);
		MZ_GC_REG();
		tag = scheme_make_integer(FL_WIDGET_TAG);
		MZ_GC_UNREG();
		_ret_ = scheme_make_cptr(ret, tag);
	      }
	  }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::pulldown(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Fl_Menu_Item* ret = 0;
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[1], x);
      if (x < 0)
	x = Fl::event_x();
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[2], y);
      if (y < 0)
	y = Fl::event_y();
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[3], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[4], h);
      if (argc == 5)
	ret = const_cast<Fl_Menu_Item*>(menu_item->pulldown(x, y, w, h));
      else
	{
	  Fl_Menu_Item* title = 0;
	  if (argv[7] != scheme_null)
	    title = _get_fl_menu_item(argc, argv, 7);
	  Fl_Menu_Item* picked = 0;
	  if (argv[5] != scheme_null)
	    picked = _get_fl_menu_item(argc, argv, 5);
	  Fl_Menu_* button = 0;
	  if (argv[6] != scheme_null)
	    button = _get_fl_menu(argc, argv, 6);
	  int menubar = 0;
	  spark::Utils::int_from_scheme_long(argv[8], menubar);
	  ret = 
	    const_cast<Fl_Menu_Item*>(menu_item->pulldown(x, y, w, h, picked, 
							  button, title, menubar));
	  {
	    Widget* widget = reinterpret_cast<Widget*>(ret->user_data_);
	    if (widget)
	      {
		Scheme_Object* tag = 0;
		MZ_GC_DECL_REG(1);
		MZ_GC_VAR_IN_REG(0, tag);
		MZ_GC_REG();
		tag = scheme_make_integer(FL_WIDGET_TAG);
		MZ_GC_UNREG();
		_ret_ = scheme_make_cptr(ret, tag);
	      }
	  }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::menu_item_size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    _ret_ = scheme_make_integer(menu_item->size());
    
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::next(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      Fl_Menu_Item* n = const_cast<Fl_Menu_Item*>(menu_item->next(i));
      if (n)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_WIDGET_TAG);
	  MZ_GC_UNREG();
	  _ret_ = scheme_make_cptr(n, tag);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_menu::menu_item_test_shortcut(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Item* menu_item = _get_fl_menu_item(argc, argv, 0);
  if (menu_item)
    {
      Fl_Menu_Item* n = const_cast<Fl_Menu_Item*>(menu_item->test_shortcut());
      if (n)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_WIDGET_TAG);
	  MZ_GC_UNREG();
	  _ret_ = scheme_make_cptr(n, tag);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::clear_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Choice* choice = _get_fl_choice(argc, argv, 0);
  if (choice)
    {
      choice->clear_changed();
      _ret_ = scheme_true;
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Choice* choice = _get_fl_choice(argc, argv, 0);
  if (choice)
    _ret_ = choice->changed() ? scheme_true : scheme_false;
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::choice_down_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Choice* choice = _get_fl_choice(argc, argv, 0);
  if (choice)
    {
      if (argc == 1)
	{
	  Fl_Boxtype bt = choice->down_box();
	  int i = static_cast<int>(bt);
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      Fl_Boxtype bt = static_cast<Fl_Boxtype>(i);
	      choice->down_box(bt);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::set_changed(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Choice* choice = _get_fl_choice(argc, argv, 0);
  if (choice)
    {
      choice->set_changed();
      _ret_ = scheme_true;
    }
    
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::choice_value(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Choice* choice = _get_fl_choice(argc, argv, 0);
  if (choice)
    {
      if (argc == 1)
	{
	  int i = choice->value();
	  _ret_ = scheme_make_integer(i);
	}
      else
	{
	  int i = 0;
	  if (spark::Utils::int_from_scheme_long(argv[1], i))
	    {
	      choice->value(i);
	      _ret_ = scheme_true;
	    }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::menu_button_popup(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Button* menu_button = _get_fl_menu_button(argc, argv, 0);
  if (menu_button)
    {
      Fl_Menu* menu = const_cast<Fl_Menu*>(menu_button->popup());
      if (menu)
	{
	  Scheme_Object* tag = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, tag);
	  MZ_GC_REG();
	  tag = scheme_make_integer(FL_WIDGET_TAG);
	  MZ_GC_UNREG();
	  _ret_ = scheme_make_cptr(menu, tag);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_menu::menu_button_type(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Menu_Button* menu_button = _get_fl_menu_button(argc, argv, 0);
  if (menu_button)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      menu_button->type(static_cast<uchar>(i));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

static void* 
_get_cptr(int argc, Scheme_Object** argv, int index)
{
  void* p = SCHEME_CPTR_VAL(argv[index]);
  if (!p)
    {
      scheme_wrong_type("_get_cptr", "cptr-val", 
			index, argc, argv);
      return 0;
    }
  return p;
}

Fl_Menu_*
_get_fl_menu(int argc, Scheme_Object** argv, int index)
{
  return reinterpret_cast<Fl_Menu_*>(_get_cptr(argc, argv, index));
}

Fl_Menu_Item*
_get_fl_menu_item(int argc, Scheme_Object** argv, int index)
{
  return reinterpret_cast<Fl_Menu_Item*>(_get_cptr(argc, argv, index));
}

Fl_Choice*
_get_fl_choice(int argc, Scheme_Object** argv, int index)
{
  return reinterpret_cast<Fl_Choice*>(_get_cptr(argc, argv, index));
}

Fl_Menu_Button*
_get_fl_menu_button(int argc, Scheme_Object** argv, int index)
{
  return reinterpret_cast<Fl_Menu_Button*>(_get_cptr(argc, argv, index));
}
