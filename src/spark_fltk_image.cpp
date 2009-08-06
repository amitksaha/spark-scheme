// MzScheme inetrface to the FLTK Image classes.
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

#include <FL/Fl_Image.H>
#include <FL/Fl_XBM_Image.H>
#include <FL/Fl_GIF_Image.H>
#include <FL/Fl_XPM_Image.H>
#include <FL/Fl_JPEG_Image.H>
#include <FL/Fl_PNG_Image.H>
#include <FL/Fl_PNM_Image.H>
#include <FL/Fl_BMP_Image.H>
#include <FL/Fl_Shared_Image.H>
#include <FL/Fl_Tiled_Image.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_image
{
  static Scheme_Object* fl_image(int, Scheme_Object**);
  static Scheme_Object* color_average(int, Scheme_Object**);
  static Scheme_Object* copy(int, Scheme_Object**);
  static Scheme_Object* count(int, Scheme_Object**);
  static Scheme_Object* depth(int, Scheme_Object**);
  //static Scheme_Object* data(int, Scheme_Object**);
  static Scheme_Object* desaturate(int, Scheme_Object**);
  static Scheme_Object* draw(int, Scheme_Object**);
  static Scheme_Object* height(int, Scheme_Object**);
  static Scheme_Object* inactive(int, Scheme_Object**);
  static Scheme_Object* ld(int, Scheme_Object**);
  static Scheme_Object* uncache(int, Scheme_Object**);
  static Scheme_Object* width(int, Scheme_Object**);
  static Scheme_Object* tiled_image(int, Scheme_Object**);
  static Scheme_Object* dispose(int, Scheme_Object**);
  // shared image
  static Scheme_Object* register_images(int, Scheme_Object**);
  static Scheme_Object* shared_image_get(int, Scheme_Object**);
  static Scheme_Object* shared_image_find(int, Scheme_Object**);
  static Scheme_Object* shared_image_release(int, Scheme_Object**);
  static Scheme_Object* make_shared_image(int, Scheme_Object**);
  static Scheme_Object* shared_image_name(int, Scheme_Object**);
  static Scheme_Object* shared_image_count(int, Scheme_Object**);
  static Scheme_Object* shared_image_refcount(int, Scheme_Object**);
} // namespace spark_fltk_image


spark::Status_code
spark_fltk::_add_image_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_fltk_image::fl_image, "fl-image", 1, 2),
    new Procedure(spark_fltk_image::color_average, 
		  "color-average", 3),
    new Procedure(spark_fltk_image::copy, "image-copy", 1, 3),
    new Procedure(spark_fltk_image::count, "image-count", 1),
    new Procedure(spark_fltk_image::depth, "image-depth", 1),
    //new Procedure(spark_fltk_image::data, "image-data", 1, 2),
    new Procedure(spark_fltk_image::desaturate, "grayscale", 1),
    new Procedure(spark_fltk_image::draw, "image-draw", 3, 7),
    new Procedure(spark_fltk_image::height, "image-height", 1),
    new Procedure(spark_fltk_image::inactive, "image-inactive", 1),
    new Procedure(spark_fltk_image::uncache, "uncache", 1),
    new Procedure(spark_fltk_image::width, "image-width", 1),
    new Procedure(spark_fltk_image::ld, "image-line-data-size", 1),
    new Procedure(spark_fltk_image::tiled_image, "tiled-image", 1, 3),
    new Procedure(spark_fltk_image::dispose, "image-dispose", 1),
    // shared image
    new Procedure(spark_fltk_image::register_images, 
		  "register-images", 0),
    new Procedure(spark_fltk_image::shared_image_get, 
		  "shared-image-get", 1, 3),
    new Procedure(spark_fltk_image::shared_image_find, 
		  "shared-image-find", 1, 3),
    new Procedure(spark_fltk_image::shared_image_release, 
		  "shared-image-release", 1),
    new Procedure(spark_fltk_image::make_shared_image, 
		  "make-shared-image", 1),
    new Procedure(spark_fltk_image::shared_image_name, 
		  "shared-image-name", 1),
    new Procedure(spark_fltk_image::shared_image_count, 
		  "shared-image-count", 0),
    new Procedure(spark_fltk_image::shared_image_refcount, 
		  "shared-image-refcount", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-fltk");
}

enum Image_type
  {
    NONE, XBM, GIF, XPM, JPEG, PNG, PNM, BMP, SHARED
  };

static Fl_Image* _get_fl_image(int, Scheme_Object**, 
			       int, Image_type t = NONE);

Scheme_Object* 
spark_fltk_image::fl_image(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = 0;
  if (argc <= 2)
    {
      if (!SCHEME_CHAR_STRINGP(argv[0]))
	scheme_wrong_type("fl-image", "string", 0, argc, argv);	
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      std::string file_name = SCHEME_BYTE_STR_VAL(str);
      std::string type;
      if (argc == 2)
	{
	  if (!SCHEME_SYMBOLP(argv[1]))
	    scheme_wrong_type("fl-image", "symbol", 1, argc, argv);
	  type = SCHEME_SYM_VAL(argv[1]);
	}
      spark::Utils::get_file_extn(file_name.c_str(), type);
      if (type == "xbm")
	image = new Fl_XBM_Image(file_name.c_str());
      else if (type == "gif")
	image = new Fl_GIF_Image(file_name.c_str());
      else if (type == "xpm")
	image = new Fl_XPM_Image(file_name.c_str());
      else if (type == "jpg" || type == "jpeg")
	image = new Fl_JPEG_Image(file_name.c_str());
      else if (type == "png")
	image = new Fl_PNG_Image(file_name.c_str());
      else if (type == "pnm")
	image = new Fl_PNM_Image(file_name.c_str());
      else if (type == "bmp")
	image = new Fl_BMP_Image(file_name.c_str());
    }
  if (!image)
    {
      DEFAULT_RET_FINISH;
    }
  Fltk_tag t = FL_IMAGE_TAG;
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(t);
    MZ_GC_UNREG();
    _ret_ = scheme_make_cptr(image, tag);
  }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::color_average(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      Fl_Color c = static_cast<Fl_Color>(i);
      float f = 0.0f;
      spark::Utils::float_from_scheme_double(argv[2], f);
      image->color_average(c, f);
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::copy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      Fl_Image* c_image = 0;
      if (argc == 1)
	c_image = image->copy();
      else
	{
	  int w = 0;
	  spark::Utils::int_from_scheme_long(argv[1], w);
	  int h = 0;
	  spark::Utils::int_from_scheme_long(argv[2], h);
	  c_image = image->copy(w, h);
	}
      if (c_image)
	{
	  Fltk_tag t = FL_IMAGE_TAG;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(t);
	    _ret_ = scheme_make_cptr(c_image, tag);
	    MZ_GC_UNREG();
	  }
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::count(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    _ret_ = scheme_make_integer(image->count());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::depth(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    _ret_ = scheme_make_integer(image->d());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::width(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    _ret_ = scheme_make_integer(image->w());
  
  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::height(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    _ret_ = scheme_make_integer(image->h());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::ld(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    _ret_ = scheme_make_integer(image->ld());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::uncache(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      image->uncache();
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::desaturate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      image->desaturate();
      _ret_ = scheme_true;
    }	  

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::inactive(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      image->inactive();
      _ret_ = scheme_true;
    }	  

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::draw(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      int i = 1;
      int x = 0;
      spark::Utils::int_from_scheme_long(argv[i++], x);
      int y = 0;
      spark::Utils::int_from_scheme_long(argv[i++], y);
      if (argc == 3)
	image->draw(x, y);
      else if (argc == 7)
	{
	  int w = 0;
	  spark::Utils::int_from_scheme_long(argv[i++], w);
	  int h = 0;
	  spark::Utils::int_from_scheme_long(argv[i++], h);
	  int cx = 0;
	  spark::Utils::int_from_scheme_long(argv[i++], cx);
	  int cy = 0;
	  spark::Utils::int_from_scheme_long(argv[i], cy);
	  image->draw(x, y, w, h, cx, cy);
	}	  
      _ret_ = ((argc == 3 || argc == 7)) ? scheme_true : scheme_null;
    }	  

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::tiled_image(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      int w = 0;
      int h = 0;
      if (argc == 2)
	spark::Utils::int_from_scheme_long(argv[1], w);
      if (argc == 3)
	spark::Utils::int_from_scheme_long(argv[2], h);
      Fl_Tiled_Image* t_image = new Fl_Tiled_Image(image, w, h);
      Fltk_tag t = FL_IMAGE_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(t_image, tag);
	MZ_GC_UNREG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::dispose(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* image = _get_fl_image(argc, argv, 0);
  if (image)
    {
      delete image;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// shared image

typedef std::map<Fl_Shared_Image*, Scheme_Object*> Shared_image_ptr_map;

struct Shared_image_holder
{
public:
  static void 
  put(Fl_Shared_Image* i, Scheme_Object* cptr)
  {
    spark::Lock lock;
    _shared_images[i] = cptr;
  }
  static Scheme_Object*
  get(Fl_Shared_Image* i)
  {
    spark::Lock lock;
    Shared_image_ptr_map::const_iterator it_curr = _shared_images.begin();
    Shared_image_ptr_map::const_iterator it_end = _shared_images.end();
    while (it_curr != it_end)
      {
	if (it_curr->first == i)
	  return it_curr->second;
	++it_curr;
      }
    return 0;
  }
  static void
  remove(Fl_Shared_Image* i)
  {
    spark::Lock lock;
    Shared_image_ptr_map::iterator it_curr = _shared_images.begin();
    Shared_image_ptr_map::iterator it_end = _shared_images.end();
    while (it_curr != it_end)
      {
	if (it_curr->first == i)
	  {
	    _shared_images.erase(it_curr);
	    return;
	  }
	++it_curr;
      }
  }
private:
  static Shared_image_ptr_map _shared_images;
};

Shared_image_ptr_map Shared_image_holder::_shared_images;

Scheme_Object*
spark_fltk_image::register_images(int, Scheme_Object**)
{
  DEFAULT_RET_INIT;
  
  ::fl_register_images();
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_get(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("shared-image-get", "string", 0, argc, argv);	
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string file_name = SCHEME_BYTE_STR_VAL(str);
  int w = 0;
  int h = 0;
  if (argc >= 2)
    spark::Utils::int_from_scheme_long(argv[1], w);
  if (argc >= 3)
    spark::Utils::int_from_scheme_long(argv[2], h);
  Fl_Shared_Image* image = Fl_Shared_Image::get(file_name.c_str(), w, h);
  if (image)
    {
      Fltk_tag t = FL_IMAGE_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(image, tag);
	Shared_image_holder::put(image, _ret_);
	MZ_GC_UNREG();
      }
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_find(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("shared-image-find", "string", 0, argc, argv);	
  Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
  std::string file_name = SCHEME_BYTE_STR_VAL(str);
  int w = 0;
  int h = 0;
  if (argc >= 2)
    spark::Utils::int_from_scheme_long(argv[1], w);
  if (argc >= 3)
    spark::Utils::int_from_scheme_long(argv[2], h);
  Fl_Shared_Image* image = Fl_Shared_Image::find(file_name.c_str(), w, h);
  if (image)
    {
      Scheme_Object* cptr = Shared_image_holder::get(image);
      if (cptr)
	_ret_ = cptr;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_release(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* i = _get_fl_image(argc, argv, 0, SHARED);
  if (i)
    {
      Fl_Shared_Image* image = dynamic_cast<Fl_Shared_Image*>(i);
      if (image)
	{
	  if (image->refcount() == 1)
	    Shared_image_holder::remove(image);
	  image->release();
	  _ret_ = scheme_true;
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_refcount(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* i = _get_fl_image(argc, argv, 0, SHARED);
  if (i)
    {
      Fl_Shared_Image* image = dynamic_cast<Fl_Shared_Image*>(i);
      if (image)
	_ret_ = scheme_make_integer(image->refcount());
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_count(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(Fl_Shared_Image::num_images());

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::shared_image_name(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* i = _get_fl_image(argc, argv, 0, SHARED);
  if (i)
    {
      Fl_Shared_Image* image = dynamic_cast<Fl_Shared_Image*>(i);
      if (image)
	{
	  const char* n = image->name();
	  if (n)
	    _ret_ = scheme_make_utf8_string(n);
	}
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_fltk_image::make_shared_image(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Fl_Image* i = _get_fl_image(argc, argv, 0, SHARED);
  if (i)
    {
      Fl_Shared_Image* image = dynamic_cast<Fl_Shared_Image*>(i);
      if (image)
	{
	  Fltk_tag t = FL_IMAGE_TAG;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(t);
	    _ret_ = scheme_make_cptr(image, tag);
	    MZ_GC_UNREG();
	  }
	}
    }

  DEFAULT_RET_FINISH;
}

Fl_Image*
_get_fl_image(int argc, Scheme_Object** argv, 
	      int index, Image_type t)
{
  Fl_Image* image = _scheme_object_to_image(argc, argv, index);
  if (!image)
    return 0;
  switch (t)
    {
    case XBM:
      return dynamic_cast<Fl_XBM_Image*>(image);
    case XPM:
      return dynamic_cast<Fl_XPM_Image*>(image);
    case GIF:
      return dynamic_cast<Fl_GIF_Image*>(image);
    case JPEG:
      return dynamic_cast<Fl_JPEG_Image*>(image);
    case BMP:
      return dynamic_cast<Fl_BMP_Image*>(image);
    case PNG:
      return dynamic_cast<Fl_PNG_Image*>(image);
    case PNM:
      return dynamic_cast<Fl_PNM_Image*>(image);
    case SHARED:
      return dynamic_cast<Fl_Shared_Image*>(image);
    case NONE:
    default:
      return image;
    }
}
