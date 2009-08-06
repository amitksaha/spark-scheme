// MzScheme inetrface to the FLTK Tile widget.
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

#include <FL/Fl_Tile.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_tile
{
  static Scheme_Object* fl_tile(int, Scheme_Object**);
  static Scheme_Object* position(int, Scheme_Object**);
  static Scheme_Object* resizable(int, Scheme_Object**);
} // namespace spark_fltk_tile

spark::Status_code
spark_fltk::_add_tile_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_tile::fl_tile,
		  "fl-tile", 5),
    new Procedure(spark_fltk_tile::position, "tile-position", 5),
    new Procedure(spark_fltk_tile::resizable, "border-widget", 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

static Fl_Tile* _get_fl_tile(int argc, 
			     Scheme_Object** argv, 
			     int index);

Scheme_Object* 
spark_fltk_tile::fl_tile(int argc, Scheme_Object** argv)
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
  Fl_Tile* tile = new Fl_Tile(x, y, w, h);
  if (title.length() > 0)
    tile->copy_label(title.c_str());
  Fltk_tag t = FL_WIDGET_TAG;
  spark_fltk::Widget* widget = new spark_fltk::Widget;
  tile->argument(reinterpret_cast<long>(widget));
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(tile, tag);
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_tile::position(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Tile* tile = _get_fl_tile(argc, argv, 0);
  if (tile)
    {
      int from_x = 0;
      int from_y = 0;
      int to_x = 0;
      int to_y = 0;
      spark::Utils::int_from_scheme_long(argv[1], from_x);
      spark::Utils::int_from_scheme_long(argv[2], from_y);
      spark::Utils::int_from_scheme_long(argv[3], to_x);
      spark::Utils::int_from_scheme_long(argv[4], to_y);
      tile->position(from_x, from_y, to_x, to_y);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_tile::resizable(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Tile* tile = _get_fl_tile(argc, argv, 0);
  if (tile)
    {
      Fl_Widget* w = _get_widget(argc, argv, 1);
      if (w)
	{
	  tile->resizable(w);
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Tile*
_get_fl_tile(int argc, Scheme_Object** argv, int index)
{
  Fl_Widget* widget;
  if ((widget = _get_widget(argc, argv, index)) == 0)
    return 0;
  return dynamic_cast<Fl_Tile*>(widget);
}
