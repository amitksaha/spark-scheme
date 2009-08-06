// Utilities for OpenGL.
// Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

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

#ifndef _SPARK_OPENGL_UTIL_H_
#define _SPARK_OPENGL_UTIL_H_

namespace spark_opengl_util
{
  struct Texture_image
  {
    enum Type { BMP, RAW };

    Texture_image(Type t=BMP, int w=64, int h=64)
      : type(t), width(w), height(h), data(0)
    { } 

    ~Texture_image()
    {
      if (data)
	delete[] data;
    }

    Type type;
    int width;
    int height;
    unsigned char* data;
  };

  // The following load_xxx functions loads an image file
  // and makes the RGB values ready for OpenGL texturing.
  // out should be preinitialized. The return value is an
  // int: 0 on success, 1 on file IO error and 2 on other
  // error.
  int load_bmp(const char* file_name, Texture_image* out);
  int load_raw(const char* file_name, int w, int h, Texture_image* out);
  // :~
} // namespace spark_opengl_util

#endif // #ifndef _SPARK_OPENGL_UTIL_H_

