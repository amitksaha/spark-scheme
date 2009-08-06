// MzScheme inetrface to the FLTK drawing functions.
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

#include <cstdio> 
#include <FL/x.H>
#include <FL/fl_draw.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_draw
{
  static Scheme_Object* fl_draw_box(int, Scheme_Object**);
  static Scheme_Object* fl_frame(int, Scheme_Object**);
  static Scheme_Object* fl_frame2(int, Scheme_Object**);
  static Scheme_Object* fl_push_clip(int, Scheme_Object**);
  static Scheme_Object* _fl_push_no_clip(int, Scheme_Object**);
  static Scheme_Object* _fl_pop_clip(int, Scheme_Object**);
  static Scheme_Object* fl_not_clipped(int, Scheme_Object**);
  static Scheme_Object* fl_clip_box(int, Scheme_Object**);
  static Scheme_Object* fl_color(int, Scheme_Object**);
  static Scheme_Object* fl_line_style(int, Scheme_Object**);
  static Scheme_Object* fl_point(int, Scheme_Object**);
  static Scheme_Object* fl_rectf(int, Scheme_Object**);
  static Scheme_Object* fl_rect(int, Scheme_Object**);
  static Scheme_Object* fl_line(int, Scheme_Object**);
  static Scheme_Object* fl_loop(int, Scheme_Object**);
  static Scheme_Object* fl_polygon(int, Scheme_Object**);
  static Scheme_Object* fl_xyline(int, Scheme_Object**);
  static Scheme_Object* fl_yxline(int, Scheme_Object**);
  static Scheme_Object* fl_arc(int, Scheme_Object**);
  static Scheme_Object* fl_pie(int, Scheme_Object**);
  // 2-d
  static Scheme_Object* fl_push_matrix(int, Scheme_Object**);
  static Scheme_Object* fl_pop_matrix(int, Scheme_Object**);
  static Scheme_Object* fl_scale(int, Scheme_Object**);
  static Scheme_Object* fl_translate(int, Scheme_Object**);
  static Scheme_Object* fl_rotate(int, Scheme_Object**);
  static Scheme_Object* fl_mult_matrix(int, Scheme_Object**);
  static Scheme_Object* fl_transform_x(int, Scheme_Object**);
  static Scheme_Object* fl_transform_y(int, Scheme_Object**);
  static Scheme_Object* fl_transform_dx(int, Scheme_Object**);
  static Scheme_Object* fl_transform_dy(int, Scheme_Object**);
  static Scheme_Object* fl_transformed_vertex(int, Scheme_Object**);
  static Scheme_Object* fl_begin_line(int, Scheme_Object**);
  static Scheme_Object* fl_end_line(int, Scheme_Object**);
  static Scheme_Object* fl_begin_points(int, Scheme_Object**);
  static Scheme_Object* fl_end_points(int, Scheme_Object**);
  static Scheme_Object* fl_begin_loop(int, Scheme_Object**);
  static Scheme_Object* fl_end_loop(int, Scheme_Object**);
  static Scheme_Object* fl_begin_polygon(int, Scheme_Object**);
  static Scheme_Object* fl_end_polygon(int, Scheme_Object**);
  static Scheme_Object* fl_begin_complex_polygon(int, Scheme_Object**);
  static Scheme_Object* fl_end_complex_polygon(int, Scheme_Object**);
  static Scheme_Object* fl_gap(int, Scheme_Object**);
  static Scheme_Object* fl_vertex(int, Scheme_Object**);
  static Scheme_Object* fl_curve(int, Scheme_Object**);
  static Scheme_Object* fl_2d_arc(int, Scheme_Object**);
  static Scheme_Object* fl_circle(int, Scheme_Object**);
  // :~
  // text
  static Scheme_Object* fl_draw_text(int, Scheme_Object**);
  static Scheme_Object* fl_height(int, Scheme_Object**);
  static Scheme_Object* fl_descent(int, Scheme_Object**);
  static Scheme_Object* fl_width(int, Scheme_Object**);
  static Scheme_Object* fl_measure(int, Scheme_Object**);
  // :~
  // fonts
  static Scheme_Object* fl_font(int, Scheme_Object**);
  static Scheme_Object* fl_size(int, Scheme_Object**);
  // :~
  static Scheme_Object* fl_overlay_rect(int, Scheme_Object**);
  static Scheme_Object* fl_overlay_clear(int, Scheme_Object**);
  // images
  //   static Scheme_Object* fl_draw_image(int, Scheme_Object**);
  //   static Scheme_Object* fl_draw_image_mono(int, Scheme_Object**);
  //   static Scheme_Object* fl_draw_pixmap(int, Scheme_Object**);
  //   static Scheme_Object* fl_measure_pixmap(int, Scheme_Object**);
  static Scheme_Object* fl_read_image(int, Scheme_Object**);
  // offscreen drawing
  static Scheme_Object* _fl_create_offscreen(int, Scheme_Object**);
  static Scheme_Object* _fl_create_offscreen_with_alpha(int, Scheme_Object**);
  static Scheme_Object* _fl_delete_offscreen(int, Scheme_Object**);
  // static Scheme_Object* _fl_begin_offscreen(int, Scheme_Object**);
  // static Scheme_Object* _fl_end_offscreen(int, Scheme_Object**);
  static Scheme_Object* _fl_copy_offscreen(int, Scheme_Object**);
  static Scheme_Object* fl_cursor(int, Scheme_Object**);
} // namespace spark_fltk_draw

spark::Status_code
spark_fltk::_add_draw_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_draw::fl_draw_box, 
		  "fl-draw-box", 6),
    new Procedure(spark_fltk_draw::fl_frame, 
		  "fl-frame", 5),
    new Procedure(spark_fltk_draw::fl_frame2, 
		  "fl-frame2", 5),
    new Procedure(spark_fltk_draw::fl_push_clip, 
		  "fl-push-clip", 4),
    new Procedure(spark_fltk_draw::_fl_push_no_clip, 
		  "fl-push-no-clip", 0),
    new Procedure(spark_fltk_draw::_fl_pop_clip, 
		  "fl-pop-clip", 0),
    new Procedure(spark_fltk_draw::fl_not_clipped, 
		  "fl-not-clipped", 4),
    new Procedure(spark_fltk_draw::fl_clip_box, 
		  "fl-clip-box", 4),
    new Procedure(spark_fltk_draw::fl_color, 
		  "fl-color", 0, 3),
    new Procedure(spark_fltk_draw::fl_line_style, 
		  "fl-line-style", 1, 3),
    new Procedure(spark_fltk_draw::fl_point, 
		  "fl-point", 2),
    new Procedure(spark_fltk_draw::fl_rectf, 
		  "fl-rectf", 4, 7),
    new Procedure(spark_fltk_draw::fl_rect, 
		  "fl-rect", 4, 5),
    new Procedure(spark_fltk_draw::fl_line, 
		  "fl-line", 4, 6),
    new Procedure(spark_fltk_draw::fl_loop, 
		  "fl-loop", 6, 8),
    new Procedure(spark_fltk_draw::fl_polygon, 
		  "fl-polygon", 6, 8),
    new Procedure(spark_fltk_draw::fl_xyline, 
		  "fl-xyline", 5),
    new Procedure(spark_fltk_draw::fl_yxline, 
		  "fl-yxline", 5),
    new Procedure(spark_fltk_draw::fl_arc,
		  "fl-arc", 6),
    new Procedure(spark_fltk_draw::fl_pie,
		  "fl-pie", 6),
    // 2-d
    new Procedure(spark_fltk_draw::fl_push_matrix, 
		  "fl-push-matrix", 0),
    new Procedure(spark_fltk_draw::fl_pop_matrix, 
		  "fl-pop-matrix", 0),
    new Procedure(spark_fltk_draw::fl_scale, 
		  "fl-scale", 1, 2),
    new Procedure(spark_fltk_draw::fl_translate, 
		  "fl-translate", 2),
    new Procedure(spark_fltk_draw::fl_rotate, 
		  "fl-rotate", 1),
    new Procedure(spark_fltk_draw::fl_mult_matrix, 
		  "fl-mult-matrix", 6),
    new Procedure(spark_fltk_draw::fl_transform_x, 
		  "fl-transform-x", 2),
    new Procedure(spark_fltk_draw::fl_transform_y, 
		  "fl-transform-y", 2),
    new Procedure(spark_fltk_draw::fl_transform_dx, 
		  "fl-transform-dx", 2),
    new Procedure(spark_fltk_draw::fl_transform_dy, 
		  "fl-transform-dy", 2),
    new Procedure(spark_fltk_draw::fl_transformed_vertex, 
		  "fl-transformed-vertex", 2),
    new Procedure(spark_fltk_draw::fl_begin_line, 
		  "fl-begin-line", 0),
    new Procedure(spark_fltk_draw::fl_end_line, 
		  "fl-end-line", 0),
    new Procedure(spark_fltk_draw::fl_begin_points, 
		  "fl-begin-points", 0),
    new Procedure(spark_fltk_draw::fl_end_points, 
		  "fl-end-points", 0),
    new Procedure(spark_fltk_draw::fl_begin_loop, 
		  "fl-begin-loop", 0),
    new Procedure(spark_fltk_draw::fl_end_loop, 
		  "fl-end-loop", 0),
    new Procedure(spark_fltk_draw::fl_begin_polygon, 
		  "fl-begin-polygon", 0),
    new Procedure(spark_fltk_draw::fl_end_polygon, 
		  "fl-end-polygon", 0),
    new Procedure(spark_fltk_draw::fl_begin_complex_polygon, 
		  "fl-begin-complex-polygon", 0),
    new Procedure(spark_fltk_draw::fl_end_complex_polygon, 
		  "fl-end-complex-polygon", 0),
    new Procedure(spark_fltk_draw::fl_gap, 
		  "fl-gap", 0),
    new Procedure(spark_fltk_draw::fl_vertex, 
		  "fl-vertex", 2),
    new Procedure(spark_fltk_draw::fl_curve, 
		  "fl-curve", 8),
    new Procedure(spark_fltk_draw::fl_2d_arc, 
		  "fl-2d-arc", 5),
    new Procedure(spark_fltk_draw::fl_circle, 
		  "fl-circle", 3),
    // text
    new Procedure(spark_fltk_draw::fl_draw_text, 
		  "fl-draw-text", 3, 8),
    new Procedure(spark_fltk_draw::fl_measure, 
		  "fl-measure", 2),
    new Procedure(spark_fltk_draw::fl_height, 
		  "fl-height", 0),
    new Procedure(spark_fltk_draw::fl_descent, 
		  "fl-descent", 0),
    new Procedure(spark_fltk_draw::fl_width, 
		  "fl-width", 1),
    // fonts
    new Procedure(spark_fltk_draw::fl_font, 
		  "fl-font", 0, 2),
    new Procedure(spark_fltk_draw::fl_size, 
		  "fl-size", 0),
    // overlay
    new Procedure(spark_fltk_draw::fl_overlay_rect, 
		  "fl-overlay-rect", 4),
    new Procedure(spark_fltk_draw::fl_overlay_clear, 
		  "fl-overlay-clear", 0),
    // image
    // new Procedure(spark_fltk_draw::fl_draw_image, 
    // 		  "fl-draw-image", 5, 7),
    //     new Procedure(spark_fltk_draw::fl_draw_image_mono, 
    // 		  "fl-draw-image-mono", 5, 7),
    //     new Procedure(spark_fltk_draw::fl_draw_pixmap, 
    // 		  "fl-draw-pixmap", 3, 4),
    //     new Procedure(spark_fltk_draw::fl_measure_pixmap, 
    // 		  "fl-measure-pixmap", 1),
    new Procedure(spark_fltk_draw::fl_read_image, 
		  "fl-read-image", 5, 6),
    // offscreen
    new Procedure(spark_fltk_draw::_fl_create_offscreen, 
		  "fl-create-offscreen", 2),
    new Procedure(spark_fltk_draw::_fl_create_offscreen_with_alpha, 
		  "fl-create-offscreen-with-alpha", 2),
    new Procedure(spark_fltk_draw::_fl_delete_offscreen, 
		  "fl-delete-offscreen", 1),
    // new Procedure(spark_fltk_draw::_fl_begin_offscreen, 
    //		  "fl-begin-offscreen", 1),
    // new Procedure(spark_fltk_draw::_fl_end_offscreen, 
    //		  "fl-end-offscreen", 0),
    new Procedure(spark_fltk_draw::_fl_copy_offscreen, 
		  "fl-copy-offscreen", 7),
    new Procedure(spark_fltk_draw::fl_cursor, 
		  "fl-cursor", 3),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

spark::Status_code
spark_fltk::_add_draw_constants(Scheme_Env* env)
{
 using spark::Constant;
  Constant constants[] = { 
    // line styles
    Constant("FL-SOLID", FL_SOLID),
    Constant("FL-DASH", FL_DASH),
    Constant("FL-DOT", FL_DOT),
    Constant("FL-DASHDOT", FL_DASHDOT),
    Constant("FL-DASHDOTDOT", FL_DASHDOTDOT),
    Constant("FL-CAP-FLAT", FL_CAP_FLAT),
    Constant("FL-CAP-ROUND", FL_CAP_ROUND),
    Constant("FL-CAP-SQUARE", FL_CAP_SQUARE),
    Constant("FL-JOIN-MITER", FL_JOIN_MITER),
    Constant("FL-JOIN-ROUND", FL_JOIN_ROUND),
    Constant("FL-JOIN-BEVEL", FL_JOIN_BEVEL),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-fltk");
}

Scheme_Object* 
spark_fltk_draw::fl_draw_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int bt = 0;
  spark::Utils::int_from_scheme_long(argv[i++], bt);
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  int c = 0;
  spark::Utils::int_from_scheme_long(argv[i++], c);
  ::fl_draw_box(static_cast<Fl_Boxtype>(bt), x, y, w, h,
		static_cast<Fl_Color>(c));
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_frame(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-frame", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  int i = 1;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  ::fl_frame(s.c_str(), x, y, w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_frame2(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-frame2", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  int i = 1;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  ::fl_frame2(s.c_str(), x, y, w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_push_clip(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  ::fl_push_clip(x, y, w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_not_clipped(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  _ret_ = ::fl_not_clipped(x, y, w, h) ? scheme_true : scheme_false;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_clip_box(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  int X = 0;
  int Y = 0;
  int W = 0;
  int H = 0;
  int r = ::fl_clip_box(x, y, w, h, X, Y, W, H);
  const int count = 5;
  Scheme_Object** elems = new Scheme_Object*[count];
  {
    MZ_GC_DECL_REG(count);
    for (int i=0; i<count; ++i)
      {
	elems[i] = NULL;
	MZ_GC_VAR_IN_REG(i, elems[i]);
      }
    MZ_GC_REG();
    elems[0] = r ? scheme_true : scheme_false;
    elems[1] = scheme_make_integer(X);
    elems[2] = scheme_make_integer(Y);
    elems[3] = scheme_make_integer(W);
    elems[4] = scheme_make_integer(H);
    MZ_GC_UNREG();
    _ret_ = scheme_build_list(count, elems);
    delete[] elems;
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::_fl_push_no_clip(int /*argc*/, 
				  Scheme_Object** /*argv*/)
{
  DEFAULT_RET_INIT;

  ::fl_push_no_clip();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::_fl_pop_clip(int /*argc*/, Scheme_Object** /*argv*/)
{
  DEFAULT_RET_INIT;

  fl_pop_clip();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_color(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    {
      int c = static_cast<int>(::fl_color());
      _ret_ = scheme_make_integer(c);
    }
  else if (argc == 1)
    {
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[0], c);
      ::fl_color(static_cast<Fl_Color>(c));
      _ret_ = scheme_true;
    }
  else if (argc == 3)
    {
      int r = 0;
      spark::Utils::int_from_scheme_long(argv[0], r);
      int g = 0;
      spark::Utils::int_from_scheme_long(argv[1], g);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[2], b);
      ::fl_color(static_cast<uchar>(r),
		 static_cast<uchar>(g),
		 static_cast<uchar>(b));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_line_style(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int style = 0;
  int width = 0;
  std::string s;
  spark::Utils::int_from_scheme_long(argv[0], style);
  if (argc >= 2)
    spark::Utils::int_from_scheme_long(argv[1], width);
  if (argc == 3)
    {
        if (!SCHEME_CHAR_STRINGP(argv[2]))
	  scheme_wrong_type("fl-line-style", "string", 2, argc, argv);
	Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	s = SCHEME_BYTE_STR_VAL(str);
    }
  ::fl_line_style(style, width, const_cast<char*>(s.c_str()));
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_point(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  ::fl_point(x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_rectf(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  if (argc == 4)
    {
      ::fl_rectf(x, y, w, h);
      _ret_ = scheme_true;
    }
  else
    {
      int r = 0;
      spark::Utils::int_from_scheme_long(argv[i++], r);
      int g = 0;
      spark::Utils::int_from_scheme_long(argv[i++], g);
      int b = 0;
      spark::Utils::int_from_scheme_long(argv[i++], b);
      ::fl_rectf(x, y, w, h,
		 static_cast<uchar>(r),
		 static_cast<uchar>(g),
		 static_cast<uchar>(b));
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_rect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  if (argc == 4)
    ::fl_rect(x, y, w, h);
  else
    {
      int c = 0;
      spark::Utils::int_from_scheme_long(argv[i], c);
      Fl_Color color = static_cast<Fl_Color>(c);
      ::fl_rect(x, y, w, h, color);
    }
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int x2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x2);
  int y2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y2);
  if (argc == 4)
    {
      ::fl_line(x, y, x2, y2);
      _ret_ = scheme_true;
    }
  else
    {
      int x3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x3);
      int y3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y3);
      ::fl_line(x, y, x2, y2, x3, y3);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_loop(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int x1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x1);
  int y1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y1);
  int x2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x2);
  int y2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y2);
  if (argc == 6)
    {
      ::fl_loop(x, y, x1, y1, x2, y2);
      _ret_ = scheme_true;
    }
  else
    {
      int x3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x3);
      int y3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y3);
      ::fl_loop(x, y, x1, y1, x2, y2, x3, y3);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_polygon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int x1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x1);
  int y1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y1);
  int x2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x2);
  int y2 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y2);
  if (argc == 6)
    {
      ::fl_polygon(x, y, x1, y1, x2, y2);
      _ret_ = scheme_true;
    }
  else
    {
      int x3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x3);
      int y3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y3);
      ::fl_polygon(x, y, x1, y1, x2, y2, x3, y3);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_xyline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int x1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x1);
  if (argc == 3)
    {
      ::fl_xyline(x, y, x1);
      _ret_ = scheme_true;
    }
  else if (argc == 4)
    {
      int y2 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y2);
      ::fl_xyline(x, y, x1, y2);
      _ret_ = scheme_true;
    }
  else if (argc == 5)
    {
      int y2 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y2);
      int x3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x3);
      ::fl_xyline(x, y, x1, y2, x3);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_yxline(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int y1 = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y1);
  if (argc == 3)
    {
      ::fl_yxline(x, y, y1);
      _ret_ = scheme_true;
    }
  else if (argc == 4)
    {
      int x2 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x2);
      ::fl_yxline(x, y, y1, x2);
      _ret_ = scheme_true;
    }
  else if (argc == 5)
    {
      int x2 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x2);
      int y3 = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y3);
      ::fl_yxline(x, y, y1, x2, y3);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_arc(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  double a1 = 0.0f;
  spark::Utils::double_from_scheme_double(argv[i++], a1);
  double a2 = 0.0f;
  spark::Utils::double_from_scheme_double(argv[i++], a2);
  ::fl_arc(x, y, w, h, a1, a2);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_pie(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  double a1 = 0.0f;
  spark::Utils::double_from_scheme_double(argv[i++], a1);
  double a2 = 0.0f;
  spark::Utils::double_from_scheme_double(argv[i++], a2);
  ::fl_pie(x, y, w, h, a1, a2);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_push_matrix(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_push_matrix();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_pop_matrix(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_pop_matrix();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_begin_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_begin_line();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_end_line(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_end_line();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_begin_points(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_begin_points();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_end_points(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_end_points();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_begin_loop(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_begin_loop();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_end_loop(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_end_loop();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_begin_polygon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_begin_polygon();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_end_polygon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_end_polygon();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_begin_complex_polygon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_begin_complex_polygon();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_end_complex_polygon(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_end_complex_polygon();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_gap(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_gap();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_scale(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[0], x);
  if (argc == 1)
    {
      ::fl_scale(x);
      _ret_ = scheme_true;
    }
  else
    {
      float y = 0.0f;
      spark::Utils::float_from_scheme_double(argv[1], y);
      ::fl_scale(x, y);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_translate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[0], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[1], y);
  ::fl_translate(x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_rotate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[0], x);
  ::fl_rotate(x);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_mult_matrix(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  float a = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], a);
  float b = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], b);
  float c = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], c);
  float d = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], d);
  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y);
  ::fl_mult_matrix(a, b, c, d, x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_transform_x(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double x = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], x);
  double y = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], y);
  _ret_ = scheme_make_double(::fl_transform_x(x, y));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_transform_y(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double x = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], x);
  double y = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], y);
  _ret_ = scheme_make_double(::fl_transform_y(x, y));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_transform_dx(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double x = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], x);
  double y = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], y);
  _ret_ = scheme_make_double(::fl_transform_dx(x, y));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_transform_dy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double x = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], x);
  double y = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], y);
  _ret_ = scheme_make_double(::fl_transform_dy(x, y));

  DEFAULT_RET_FINISH;
}


Scheme_Object* 
spark_fltk_draw::fl_transformed_vertex(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  double x = 0.0f;
  spark::Utils::double_from_scheme_double(argv[0], x);
  double y = 0.0f;
  spark::Utils::double_from_scheme_double(argv[1], y);
  ::fl_transformed_vertex(x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}


Scheme_Object* 
spark_fltk_draw::fl_vertex(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y);
  ::fl_vertex(x, y);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_curve(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y);
  float x1 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x1);
  float y1 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y1);
  float x2 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x2);
  float y2 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y2);
  float x3 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x3);
  float y3 = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y3);
  ::fl_curve(x, y, x1, y1, x2, y2, x3, y3);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_2d_arc(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y);
  float r = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], r);
  float start = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], start);
  float end = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], end);
  ::fl_arc(x, y, r, start, end);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_circle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  float x = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], x);
  float y = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], y);
  float r = 0.0f;
  spark::Utils::float_from_scheme_double(argv[i++], r);
  ::fl_circle(x, y, r);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_draw_text(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-draw-text", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  int i = 1;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  if (argc == 3)
    {
      ::fl_draw(s.c_str(), x, y);
      _ret_ = scheme_true;
    }
  else if (argc >= 8)
    {
      int w = 0;
      spark::Utils::int_from_scheme_long(argv[i++], w);
      int h = 0;
      spark::Utils::int_from_scheme_long(argv[i++], h);
      int a = 0;
      spark::Utils::int_from_scheme_long(argv[i++], a);
      Fl_Image* image = 0;
      if (argc >= 7)
	image = _scheme_object_to_image(argc, argv, i++);
      int draw_symbols = 1;
      if (argc >= 8)
	draw_symbols = argv[i] == scheme_true ? 1 : 0;
      ::fl_draw(s.c_str(), x, y, w, h,
		static_cast<Fl_Align>(a),
		image, draw_symbols);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_measure(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-measure", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  int draw_symbols = 1;
  if (argc == 2)
    draw_symbols = argv[1] == scheme_true ? 1 : 0;
  int w = 0;
  int h = 0;
  ::fl_measure(s.c_str(), w, h, draw_symbols);  
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
    elems[0] = scheme_make_integer(w);
    elems[1] = scheme_make_integer(h);
    MZ_GC_UNREG();
    _ret_ = scheme_build_list(count, elems);
    delete[] elems;
  }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_height(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(::fl_height());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_descent(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(::fl_height());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-width", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string s = SCHEME_BYTE_STR_VAL(str);
  _ret_ = scheme_make_double(::fl_width(s.c_str()));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_font(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (argc == 0)
    _ret_ = scheme_make_integer(::fl_font());
  else
    {
      int f = 0;
      spark::Utils::int_from_scheme_long(argv[0], f);
      int s = 0;
      spark::Utils::int_from_scheme_long(argv[1], s);
      ::fl_font(f, s);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_size(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(::fl_size());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_overlay_rect(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  ::fl_overlay_rect(x, y, w, h);
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_overlay_clear(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  ::fl_overlay_clear();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_cursor(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  int fg = 0;
  spark::Utils::int_from_scheme_long(argv[1], fg);
  int bg = 0;
  spark::Utils::int_from_scheme_long(argv[2], bg);
  ::fl_cursor(static_cast<Fl_Cursor>(i), 
	      static_cast<Fl_Color>(fg),
	      static_cast<Fl_Color>(bg));
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::fl_read_image(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("fl-read-image", "string", 0, argc, argv);
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string file_name = SCHEME_BYTE_STR_VAL(str);
  FILE* f = fopen(file_name.c_str(), "wb");
  if (!f)
    {
      scheme_signal_error("Failed to open file to write image data.");
      DEFAULT_RET_FINISH;
    }  
  int i = 1;
  int x = 0;
  spark::Utils::int_from_scheme_long(argv[i++], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[i++], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[i++], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[i++], h);
  int a = 0;
  if (argc == 6)
    spark::Utils::int_from_scheme_long(argv[i++], a);
  uchar* p = 0;
  size_t len = 0;
  if (a)
    len = w*h*4;
  else
    len = w*h*3;
  p = new uchar[len];
  p = ::fl_read_image(p, x, y, w, h, a);
  if (p)
    {
      if (fwrite(reinterpret_cast<void*>(p), 
		 sizeof(uchar), len, f) == len)
	_ret_ = scheme_true;
    }
  fclose(f);

  DEFAULT_RET_FINISH;
}

static const int MAX_OFFSCREEN = 255;

struct Offscreen_holder
{
public:
  static Offscreen_holder& instance()
  {
    return INSTANCE;
  }
  int put(Fl_Offscreen fo)
  {
    spark::Lock lock;
    for (int i=0; i<MAX_OFFSCREEN; ++i)
      {
	if (_offscreens[i] == 0)
	  {
	    _offscreens[i] = fo;
	    return i;
	  }
      }
    return -1;
  }
  Fl_Offscreen get(int i)
  {
    if (i < 0 || i >= MAX_OFFSCREEN)
      return 0;
    return _offscreens[i];
  }
  void remove(int i)
  {
    if (i < 0 || i >= MAX_OFFSCREEN)
      return;
    spark::Lock lock;
    _offscreens[i] = 0;
  }
private:
  Offscreen_holder()
  {
    for (int i=0; i<MAX_OFFSCREEN; ++i)
      _offscreens[i] = 0;
  }
private:
  Fl_Offscreen _offscreens[MAX_OFFSCREEN];
  static Offscreen_holder INSTANCE;
}; // struct Offscreen_holder

Offscreen_holder Offscreen_holder::INSTANCE;

Scheme_Object* 
spark_fltk_draw::_fl_create_offscreen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int w = 0;
  spark::Utils::int_from_scheme_long(argv[0], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[1], h);
  Fl_Offscreen fo = fl_create_offscreen(w, h);
  if (fo)
    {
      Offscreen_holder oh = Offscreen_holder::instance();
      int i = oh.put(fo);
      if (i >= 0)
	_ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::_fl_create_offscreen_with_alpha(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int w = 0;
  spark::Utils::int_from_scheme_long(argv[0], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[1], h);
  Fl_Offscreen fo = fl_create_offscreen(w, h);
  if (fo)
    {
      Offscreen_holder oh = Offscreen_holder::instance();
      int i = oh.put(fo);
      if (i >= 0)
	_ret_ = scheme_make_integer(i);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::_fl_delete_offscreen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  Offscreen_holder oh = Offscreen_holder::instance();
  Fl_Offscreen fo = oh.get(i);
  if (fo)
    {
      fl_delete_offscreen(fo);
      oh.remove(i);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

/*
Scheme_Object* 
spark_fltk_draw::_fl_begin_offscreen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  Offscreen_holder oh = Offscreen_holder::instance();
  Fl_Offscreen fo = oh.get(i);
  if (fo)
    {
      fl_begin_offscreen(fo);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_fltk_draw::_fl_end_offscreen(int, Scheme_Object**)
{
  DEFAULT_RET_INIT;

  fl_end_offscreen();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}
*/

Scheme_Object* 
spark_fltk_draw::_fl_copy_offscreen(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int x = 0;
  spark::Utils::int_from_scheme_long(argv[0], x);
  int y = 0;
  spark::Utils::int_from_scheme_long(argv[1], y);
  int w = 0;
  spark::Utils::int_from_scheme_long(argv[2], w);
  int h = 0;
  spark::Utils::int_from_scheme_long(argv[3], h);
  int i = 0;
  spark::Utils::int_from_scheme_long(argv[4], i);
  int srcx = 0;
  spark::Utils::int_from_scheme_long(argv[5], srcx);
  int srcy = 0;
  spark::Utils::int_from_scheme_long(argv[6], srcy);
  Offscreen_holder oh = Offscreen_holder::instance();
  Fl_Offscreen fo = oh.get(i);
  if (fo)
    {
      fl_copy_offscreen(x, y, w, h, fo, srcx, srcy);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}
