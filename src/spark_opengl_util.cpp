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

#include <fstream>
#include <FL/Fl_BMP_Image.H>
#include "spark_opengl_util.h"

int
spark_opengl_util::load_bmp(const char* filename, Texture_image* out)
{
  Fl_BMP_Image *img = new Fl_BMP_Image(filename);
  if (img->d() == 0) 
    return 1;
  char r,g,b;
  int w = img->w();
  int h = img->h();
  int d = img->d();
  const char* buf = img->data()[0];
  int count = img->count();
  unsigned char* buffer = new unsigned char[w * h * 3];
  int ret_i = 0;
  for (int y=0; y<h; ++y) 
    {   
      for (int x=0; x<w; ++x) 
	{
	  long index = (y * w * d) + (x * d); 
	  switch (count)
	    {
	    case 1: 
	      { 
		switch (d)
		  {
		  case 1: // 8bit
		    r = g = b = *(buf+index);
		    break;		    
		  case 3:  // 24bit
		    r = *(buf+index+0);
		    g = *(buf+index+1);
		    b = *(buf+index+2);
		    break;
		  default: // ??
		    delete[] buffer;
		    return 2;
		    break;
		  }
	      }
	    default: // ?? pixmap, bit vals
	      delete[] buffer;
	      return 3;
	    }
	  buffer[ret_i++] = (unsigned char)r;
	  buffer[ret_i++] = (unsigned char)g;
	  buffer[ret_i++] = (unsigned char)b;
	}
    }
  
  out->type = Texture_image::BMP;
  out->width = w;
  out->height = h;
  out->data = buffer;
  
  return 0;
}

int
spark_opengl_util::load_raw(const char* filename, 
			    int w, int h, 
			    Texture_image* out)
{
  std::ifstream in(filename, std::ios::binary);
  if (!in)
    return 1;

  char* data = new char[w * h * 3];
  
  in.read(data, w * h * 3);

  out->type = Texture_image::RAW;
  out->width = w;
  out->height = h;
  out->data = reinterpret_cast<unsigned char*>(data);

  return 0;
}
