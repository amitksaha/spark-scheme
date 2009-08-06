// MzScheme inetrface to the FLTK Text_Buffer class.
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

#ifndef _SPARK_FLTK_TEXT_BUFFER_H_
#define _SPARK_FLTK_TEXT_BUFFER_H_

#include <FL/Fl_Text_Buffer.H>
#include "spark_fltk_common.h"
using namespace spark_fltk;

namespace spark_fltk_text_buffer
{
  struct Text_buffer
  {
    Fl_Text_Buffer* fl_text_buffer;
    Scheme_object_list modify_cbs;
    Scheme_object_list cb_params;
    
    Text_buffer()
      : fl_text_buffer(0)
    { } 
    Text_buffer(Fl_Text_Buffer* tb)
      : fl_text_buffer(tb)
    { } 

    bool remove_callback(Scheme_Object* cb)
    {
      Scheme_object_list::iterator it_curr = modify_cbs.begin();
      Scheme_object_list::iterator it_end = modify_cbs.end();
      while (it_curr != it_end)
	{
	  if (*it_curr == cb)
	    {
	      modify_cbs.erase(it_curr);
	      return true;
	    }
	  it_curr++;
	}
      return false;
    }
  };

  typedef std::map<Fl_Text_Buffer*, Text_buffer*> Text_buffer_map;

  struct Text_buffer_holder
  {
    static void put(Fl_Text_Buffer* tb, 
		    Text_buffer* text_buffer) 
    {
      _active_buffers[tb] = text_buffer;
    }
    static Text_buffer* get(Fl_Text_Buffer* tb)
    {
      return _active_buffers[tb];
    }
  private:
    static Text_buffer_map _active_buffers;
  }; // struct Widget_holder
  
  Text_buffer* _get_text_buffer(int argc, Scheme_Object** argv, 
				int index);
} // namespace spark_fltk_text_buffer

#endif // #ifndef _SPARK_FLTK_TEXT_BUFFER_H_
