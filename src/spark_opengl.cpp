// MzScheme inetrface to OpenGL.
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

#include <FL/gl.h>
#include <GL/glu.h>
#include "spark_opengl.h"
#include "spark_opengl_util.h"
using namespace spark_opengl;
using namespace spark_opengl_util;

static const char* MODULE_NAME = "#%spark-opengl";

typedef std::vector<GLdouble> GLdouble_list;
typedef std::vector<GLfloat> GLfloat_list;
typedef std::vector<GLclampf> GLclampf_list;
typedef std::vector<GLint> GLint_list;
typedef std::vector<GLshort> GLshort_list;
typedef std::vector<GLbyte> GLbyte_list;

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);

static bool _get_args_list(int argc, Scheme_Object** argv,
                          int offset, int count,
                          GLdouble_list& ret);
static bool _get_args_list(int argc, Scheme_Object** argv,
                          int offset, int count,
                          GLfloat_list& ret);
static bool _get_args_list(int argc, Scheme_Object** argv,
                          int offset, int count,
                          GLint_list& ret);
static bool _get_args_list(int argc, Scheme_Object** argv,
                          int offset, int count,
                          GLshort_list& ret);
static bool _get_args_list(int argc, Scheme_Object** argv,
                          int offset, int count,
                          GLbyte_list& ret);
static bool _glint_from_scheme_long(Scheme_Object* obj, GLint& ret);
static bool _glsizei_from_scheme_long(Scheme_Object* obj, GLsizei& ret);

spark::Status_code
spark_opengl::initialize(Scheme_Env* env)
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

// exported function signatures
namespace spark_opengl
{
  static Scheme_Object* gl_begin(int, Scheme_Object**);
  static Scheme_Object* gl_end(int, Scheme_Object**);
  static Scheme_Object* gl_enable(int, Scheme_Object**);
  static Scheme_Object* gl_disable(int, Scheme_Object**);
  static Scheme_Object* gl_clear_color(int, Scheme_Object**);
  static Scheme_Object* gl_clear_depth(int, Scheme_Object**);
  static Scheme_Object* gl_clear(int, Scheme_Object**);
  static Scheme_Object* gl_depth_func(int, Scheme_Object**);
  static Scheme_Object* gl_flush(int, Scheme_Object**);
  static Scheme_Object* gl_ortho(int, Scheme_Object**);
  static Scheme_Object* gl_viewport(int, Scheme_Object**);
  static Scheme_Object* gl_rectd(int, Scheme_Object**);
  static Scheme_Object* gl_rectf(int, Scheme_Object**);
  static Scheme_Object* gl_recti(int, Scheme_Object**);
  static Scheme_Object* gl_rects(int, Scheme_Object**);
  static Scheme_Object* gl_geterror(int, Scheme_Object**);
  static Scheme_Object* gl_getstring(int, Scheme_Object**);
  static Scheme_Object* gl_hint(int, Scheme_Object**);
  static Scheme_Object* gl_cull_face(int, Scheme_Object**);
  static Scheme_Object* gl_edge_flag(int, Scheme_Object**);
  static Scheme_Object* gl_front_face(int, Scheme_Object**);
  static Scheme_Object* gl_line_stipple(int, Scheme_Object**);
  static Scheme_Object* gl_line_width(int, Scheme_Object**);
  static Scheme_Object* gl_point_size(int, Scheme_Object**);
  static Scheme_Object* gl_polygon_mode(int, Scheme_Object**);
  static Scheme_Object* gl_polygon_stipple(int, Scheme_Object**);
  static Scheme_Object* gl_vertex_d(int, Scheme_Object**);
  static Scheme_Object* gl_vertex_f(int, Scheme_Object**);
  static Scheme_Object* gl_vertex_i(int, Scheme_Object**);
  static Scheme_Object* gl_vertex_s(int, Scheme_Object**);
  static Scheme_Object* gl_frustum(int, Scheme_Object**);
  static Scheme_Object* gl_load_identity(int, Scheme_Object**);
  static Scheme_Object* gl_load_matrix_d(int, Scheme_Object**);
  static Scheme_Object* gl_load_matrix_f(int, Scheme_Object**);
  static Scheme_Object* gl_mult_matrix_d(int, Scheme_Object**);
  static Scheme_Object* gl_mult_matrix_f(int, Scheme_Object**);
  static Scheme_Object* gl_matrix_mode(int, Scheme_Object**);
  static Scheme_Object* gl_push_matrix(int, Scheme_Object**);
  static Scheme_Object* gl_pop_matrix(int, Scheme_Object**);
  static Scheme_Object* gl_matrix_transform_d(int, Scheme_Object**);
  static Scheme_Object* gl_matrix_transform_f(int, Scheme_Object**);
  static Scheme_Object* gl_clear_index(int, Scheme_Object**);
  static Scheme_Object* gl_color_b(int, Scheme_Object**);
  static Scheme_Object* gl_color_d(int, Scheme_Object**);
  static Scheme_Object* gl_color_f(int, Scheme_Object**);
  static Scheme_Object* gl_color_i(int, Scheme_Object**);
  static Scheme_Object* gl_color_s(int, Scheme_Object**);
  static Scheme_Object* gl_color_ub(int, Scheme_Object**);
  static Scheme_Object* gl_color_ui(int, Scheme_Object**);
  static Scheme_Object* gl_color_us(int, Scheme_Object**);
  static Scheme_Object* gl_color_mask(int, Scheme_Object**);
  static Scheme_Object* gl_index_d(int, Scheme_Object**);
  static Scheme_Object* gl_index_f(int, Scheme_Object**);
  static Scheme_Object* gl_index_i(int, Scheme_Object**);
  static Scheme_Object* gl_index_s(int, Scheme_Object**);
  static Scheme_Object* gl_index_mask(int, Scheme_Object**);
  static Scheme_Object* gl_logic_op(int, Scheme_Object**);
  static Scheme_Object* gl_shade_model(int, Scheme_Object**);
  static Scheme_Object* gl_color_material(int, Scheme_Object**);
  static Scheme_Object* gl_get_material_f(int, Scheme_Object**);
  static Scheme_Object* gl_get_material_i(int, Scheme_Object**);
  static Scheme_Object* gl_get_light_f(int, Scheme_Object**);
  static Scheme_Object* gl_get_light_i(int, Scheme_Object**);
  static Scheme_Object* gl_light_f(int, Scheme_Object**);
  static Scheme_Object* gl_light_i(int, Scheme_Object**);
  static Scheme_Object* gl_light_model_f(int, Scheme_Object**);
  static Scheme_Object* gl_light_model_i(int, Scheme_Object**);
  static Scheme_Object* gl_material_f(int, Scheme_Object**);
  static Scheme_Object* gl_material_i(int, Scheme_Object**);
  static Scheme_Object* gl_normal_3b(int, Scheme_Object**);
  static Scheme_Object* gl_normal_3d(int, Scheme_Object**);
  static Scheme_Object* gl_normal_3f(int, Scheme_Object**);
  static Scheme_Object* gl_normal_3i(int, Scheme_Object**);
  static Scheme_Object* gl_normal_3s(int, Scheme_Object**);
  static Scheme_Object* gl_call_list(int, Scheme_Object**);
  static Scheme_Object* gl_call_lists(int, Scheme_Object**);
  static Scheme_Object* gl_delete_lists(int, Scheme_Object**);
  static Scheme_Object* gl_end_list(int, Scheme_Object**);
  static Scheme_Object* gl_gen_lists(int, Scheme_Object**);
  static Scheme_Object* gl_is_list(int, Scheme_Object**);
  static Scheme_Object* gl_list_base(int, Scheme_Object**);
  static Scheme_Object* gl_new_list(int, Scheme_Object**);
  static Scheme_Object* gl_copy_pixels(int, Scheme_Object**);
  static Scheme_Object* gl_draw_pixels(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_map_f(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_map_ui(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_map_us(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_store_f(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_store_i(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_zoom(int, Scheme_Object**);
  static Scheme_Object* gl_read_pixels(int, Scheme_Object**);
  static Scheme_Object* gl_gen_textures(int, Scheme_Object**);
  static Scheme_Object* gl_bind_texture(int, Scheme_Object**);
  static Scheme_Object* gl_delete_textures(int, Scheme_Object**);
  static Scheme_Object* gl_tex_coord_f(int, Scheme_Object**);
  static Scheme_Object* gl_tex_coord_d(int, Scheme_Object**);
  static Scheme_Object* gl_tex_coord_i(int, Scheme_Object**);
  static Scheme_Object* gl_tex_coord_s(int, Scheme_Object**);
  static Scheme_Object* gl_tex_env_f(int, Scheme_Object**);
  static Scheme_Object* gl_tex_env_i(int, Scheme_Object**);
  static Scheme_Object* gl_tex_gen_d(int, Scheme_Object**);
  static Scheme_Object* gl_tex_gen_f(int, Scheme_Object**);
  static Scheme_Object* gl_tex_gen_i(int, Scheme_Object**);
  static Scheme_Object* gl_tex_image(int, Scheme_Object**);
  static Scheme_Object* gl_tex_parameter_f(int, Scheme_Object**);
  static Scheme_Object* gl_tex_parameter_i(int, Scheme_Object**);
  static Scheme_Object* gl_blend_func(int, Scheme_Object**);
  static Scheme_Object* gl_fog_f(int, Scheme_Object**);
  static Scheme_Object* gl_fog_i(int, Scheme_Object**);
  static Scheme_Object* gl_eval_coord_f(int, Scheme_Object**);
  static Scheme_Object* gl_eval_coord_d(int, Scheme_Object**);
  static Scheme_Object* gl_eval_mesh(int, Scheme_Object**);
  static Scheme_Object* gl_eval_point(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_transfer_f(int, Scheme_Object**);
  static Scheme_Object* gl_pixel_transfer_i(int, Scheme_Object**);
  static Scheme_Object* gl_get_map(int, Scheme_Object**);
  static Scheme_Object* gl_map_d(int, Scheme_Object**);
  static Scheme_Object* gl_map_f(int, Scheme_Object**);
  static Scheme_Object* gl_map_grid_d(int, Scheme_Object**);
  static Scheme_Object* gl_map_grid_f(int, Scheme_Object**);
  static Scheme_Object* gl_feedback_buffer(int, Scheme_Object**);  
  static Scheme_Object* gl_get_feedback_buffer(int, Scheme_Object**);  
  static Scheme_Object* gl_init_names(int, Scheme_Object**);  
  static Scheme_Object* gl_push_name(int, Scheme_Object**);  
  static Scheme_Object* gl_pop_name(int, Scheme_Object**);  
  static Scheme_Object* gl_load_name(int, Scheme_Object**);  
  static Scheme_Object* gl_pass_through(int, Scheme_Object**);  
  static Scheme_Object* gl_select_buffer(int, Scheme_Object**);  
  static Scheme_Object* gl_get_select_buffer(int, Scheme_Object**);  
  static Scheme_Object* gl_render_mode(int, Scheme_Object**);  
  // GLU
  static Scheme_Object* glu_lookat(int, Scheme_Object**);  
  static Scheme_Object* glu_newquadric(int, Scheme_Object**);  
  static Scheme_Object* glu_deletequadric(int, Scheme_Object**);  
  static Scheme_Object* glu_quadric_drawstyle(int, Scheme_Object**);  
  static Scheme_Object* glu_quadric_normals(int, Scheme_Object**);  
  static Scheme_Object* glu_quadric_orientation(int, Scheme_Object**);  
  static Scheme_Object* glu_quadric_texture(int, Scheme_Object**);  
  static Scheme_Object* glu_cylinder(int, Scheme_Object**);  
  static Scheme_Object* glu_disk(int, Scheme_Object**);  
  static Scheme_Object* glu_partial_disk(int, Scheme_Object**);  
  static Scheme_Object* glu_sphere(int, Scheme_Object**);  
} // namespace spark_opengl

spark::Status_code
_add_constants(Scheme_Env* env)
{
 using spark::Constant;
 Constant constants[] = {
   Constant("GL-POINTS", GL_POINTS),
   Constant("GL-LINES", GL_LINES),
   Constant("GL-LINE-STRIP", GL_LINE_STRIP),
   Constant("GL-LINE-LOOP", GL_LINE_LOOP),
   Constant("GL-TRIANGLES", GL_TRIANGLES),
   Constant("GL-TRIANGLE-STRIP", GL_TRIANGLE_STRIP),
   Constant("GL-TRIANGLE-FAN", GL_TRIANGLE_FAN),
   Constant("GL-QUADS", GL_QUADS),
   Constant("GL-QUAD-STRIP", GL_QUAD_STRIP),
   Constant("GL-POLYGON", GL_POLYGON),
   // buffers
   Constant("GL-COLOR-BUFFER-BIT", GL_COLOR_BUFFER_BIT),
   Constant("GL-DEPTH-BUFFER-BIT", GL_DEPTH_BUFFER_BIT),
   Constant("GL-STENCIL-BUFFER-BIT", GL_STENCIL_BUFFER_BIT),
   // errors
   Constant("GL-NO-ERROR", GL_NO_ERROR),
   Constant("GL-INVALID-ENUM", GL_INVALID_ENUM),
   Constant("GL-INVALID-VALUE", GL_INVALID_VALUE),
   Constant("GL-INVALID-OPERATION", GL_INVALID_OPERATION),
   Constant("GL-STACK-OVERFLOW", GL_STACK_OVERFLOW),
   Constant("GL-STACK-UNDERFLOW", GL_STACK_UNDERFLOW),
   Constant("GL-OUT-OF-MEMORY", GL_OUT_OF_MEMORY),
   // opengl aspects
   Constant("GL-VENDOR", GL_VENDOR),
   Constant("GL-RENDERER", GL_RENDERER),
   Constant("GL-VERSION", GL_VERSION),
   Constant("GL-EXTENSIONS", GL_EXTENSIONS),
   // hint targets
   Constant("GL-FOG-HINT", GL_FOG_HINT),
   Constant("GL-LINE-SMOOTH-HINT", GL_LINE_SMOOTH_HINT),
   Constant("GL-PERSPECTIVE-CORRECTION-HINT",
            GL_PERSPECTIVE_CORRECTION_HINT),
   Constant("GL-POINT-SMOOTH-HINT", GL_POINT_SMOOTH_HINT),
   Constant("GL-POLYGON-SMOOTH-HINT", GL_POLYGON_SMOOTH_HINT),
   // hint modes
   Constant("GL-FASTEST", GL_FASTEST),
   Constant("GL-NICEST", GL_NICEST),
   Constant("GL-DONT-CARE", GL_DONT_CARE),
   // front face modes
   Constant("GL-CW", GL_CW),
   Constant("GL-CCW", GL_CCW),
   // face
   Constant("GL-FRONT", GL_FRONT),
   Constant("GL-BACK", GL_BACK),
   Constant("GL-FRONT-AND-BACK", GL_FRONT_AND_BACK),
   // matrix mode
   Constant("GL-MODELVIEW", GL_MODELVIEW),
   Constant("GL-PROJECTION", GL_PROJECTION),
   Constant("GL-TEXTURE", GL_TEXTURE),
   // logical pixel operations
   Constant("GL-CLEAR", GL_CLEAR),
   Constant("GL-SET", GL_SET),
   Constant("GL-COPY", GL_COPY),
   Constant("GL-COPY-INVERTED", GL_COPY_INVERTED),
   Constant("GL-NOOP", GL_NOOP),
   Constant("GL-INVERT", GL_INVERT),
   Constant("GL-AND", GL_AND),
   Constant("GL-OR", GL_OR),
   Constant("GL-NAND", GL_NAND),
   Constant("GL-NOR", GL_NOR),
   Constant("GL-XOR", GL_XOR),
   Constant("GL-EQUIV", GL_EQUIV),
   Constant("GL-AND-REVERSE", GL_AND_REVERSE),
   Constant("GL-AND-INVERTED", GL_AND_INVERTED),
   Constant("GL-OR-REVERSE", GL_OR_REVERSE),
   Constant("GL-OR-INVERTED", GL_OR_INVERTED),
   // shade models
   Constant("GL-FLAT", GL_FLAT),
   Constant("GL-SMOOTH", GL_SMOOTH),
   // enable-disable capabilities
   Constant("GL-ALPHA-TEST", GL_ALPHA_TEST),
   Constant("GL-AUTO-NORMAL", GL_AUTO_NORMAL),
   // Constant("GL-BLEND," GL_BLEND),
   Constant("GL-COLOR-LOGIC-OP", GL_COLOR_LOGIC_OP),
   Constant("GL-COLOR-MATERIAL", GL_COLOR_MATERIAL),
   Constant("GL-COLOR-TABLE", GL_COLOR_TABLE),
   Constant("GL-CONVOLUTION-1D", GL_CONVOLUTION_1D),
   Constant("GL-CONVOLUTION-2D", GL_CONVOLUTION_2D),
   Constant("GL-CULL-FACE", GL_CULL_FACE),
   Constant("GL-DEPTH-TEST", GL_DEPTH_TEST),
   Constant("GL-DITHER", GL_DITHER),
   Constant("GL-HISTOGRAM", GL_HISTOGRAM),
   Constant("GL-MINMAX", GL_MINMAX),
   Constant("GL-FOG", GL_FOG),
   Constant("GL-INDEX-LOGIC-OP", GL_INDEX_LOGIC_OP),
   Constant("GL-MAX-LIGHTS", GL_MAX_LIGHTS),
   Constant("GL-LIGHTING", GL_LIGHTING),
   Constant("GL-LIGHT-0", GL_LIGHT0),
   Constant("GL-LIGHT-1", GL_LIGHT1),
   Constant("GL-LIGHT-2", GL_LIGHT2),
   Constant("GL-LIGHT-3", GL_LIGHT3),
   Constant("GL-LIGHT-4", GL_LIGHT4),
   Constant("GL-LIGHT-5", GL_LIGHT5),
   Constant("GL-LIGHT-6", GL_LIGHT6),
   Constant("GL-LIGHT-7", GL_LIGHT7),
   Constant("GL-LINE-SMOOTH", GL_LINE_SMOOTH),
   Constant("GL-LINE-STIPPLE", GL_LINE_STIPPLE),
   Constant("GL-MAP1-COLOR-4", GL_MAP1_COLOR_4),
   Constant("GL-MAP1-INDEX", GL_MAP1_INDEX),
   Constant("GL-MAP1-NORMAL", GL_MAP1_NORMAL),
   Constant("GL-MAP1-TEXTURE-COORD-1", GL_MAP1_TEXTURE_COORD_1),
   Constant("GL-MAP1-TEXTURE-COORD-2", GL_MAP1_TEXTURE_COORD_2),
   Constant("GL-MAP1-TEXTURE-COORD-3", GL_MAP1_TEXTURE_COORD_3),
   Constant("GL-MAP1-TEXTURE-COORD-4", GL_MAP1_TEXTURE_COORD_4),
   Constant("GL-MAP1-VERTEX-3", GL_MAP1_VERTEX_3),
   Constant("GL-MAP1-VERTEX-4", GL_MAP1_VERTEX_4),
   Constant("GL-MAP2-COLOR-4", GL_MAP2_COLOR_4),
   Constant("GL-MAP2-INDEX", GL_MAP2_INDEX),
   Constant("GL-MAP2-NORMAL", GL_MAP2_NORMAL),
   Constant("GL-MAP2-TEXTURE-COORD-1", GL_MAP2_TEXTURE_COORD_1),
   Constant("GL-MAP2-TEXTURE-COORD-2", GL_MAP2_TEXTURE_COORD_2),
   Constant("GL-MAP2-TEXTURE-COORD-3", GL_MAP2_TEXTURE_COORD_3),
   Constant("GL-MAP2-TEXTURE-COORD-4", GL_MAP2_TEXTURE_COORD_4),
   Constant("GL-MAP2-VERTEX-3", GL_MAP2_VERTEX_3),
   Constant("GL-MAP2-VERTEX-4", GL_MAP2_VERTEX_4),
   Constant("GL-NORMALIZE", GL_NORMALIZE),
   Constant("GL-POINT-SMOOTH", GL_POINT_SMOOTH),
   Constant("GL-POLYGON-OFFSET-FILL", GL_POLYGON_OFFSET_FILL),
   Constant("GL-POLYGON-OFFSET-LINE", GL_POLYGON_OFFSET_LINE),
   Constant("GL-POLYGON-OFFSET-POINT", GL_POLYGON_OFFSET_POINT),
   Constant("GL-POLYGON-SMOOTH", GL_POLYGON_SMOOTH),
   Constant("GL-POLYGON-STIPPLE", GL_POLYGON_STIPPLE),
   Constant("GL-SCISSOR-TEST", GL_SCISSOR_TEST),
   Constant("GL-STENCIL-TEST", GL_STENCIL_TEST),
   Constant("GL-TEXTURE-1D", GL_TEXTURE_1D),
   Constant("GL-TEXTURE-2D", GL_TEXTURE_2D),
   Constant("GL-TEXTURE-3D", GL_TEXTURE_3D),
   Constant("GL-TEXTURE-GEN-Q", GL_TEXTURE_GEN_Q),
   Constant("GL-TEXTURE-GEN-R", GL_TEXTURE_GEN_R),
   Constant("GL-TEXTURE-GEN-S", GL_TEXTURE_GEN_S),
   Constant("GL-TEXTURE-GEN-T", GL_TEXTURE_GEN_T),
   Constant("GL-POST-COLOR-MATRIX-COLOR-TABLE", GL_POST_COLOR_MATRIX_COLOR_TABLE),
   Constant("GL-POST-CONVOLUTION-COLOR-TABLE", GL_POST_CONVOLUTION_COLOR_TABLE),
   Constant("GL-RESCALE-NORMAL", GL_RESCALE_NORMAL),
   Constant("GL-SEPARABLE-2D", GL_SEPARABLE_2D),
   // color material modes
   Constant("GL-EMISSION", GL_EMISSION),
   Constant("GL-AMBIENT", GL_AMBIENT),
   Constant("GL-DIFFUSE", GL_DIFFUSE),
   Constant("GL-SPECULAR", GL_SPECULAR),
   Constant("GL-AMBIENT-AND-DIFFUSE", GL_AMBIENT_AND_DIFFUSE),
   Constant("GL-SHININESS", GL_SHININESS),
   Constant("GL-COLOR-INDEXES", GL_COLOR_INDEXES),
   // get_light constants
   Constant("GL-POSITION", GL_POSITION),
   Constant("GL-SPOT-DIRECTION", GL_SPOT_DIRECTION),
   Constant("GL-SPOT-EXPONENT", GL_SPOT_EXPONENT),
   Constant("GL-SPOT-CUTOFF", GL_SPOT_CUTOFF),
   Constant("GL-CONSTANT-ATTENUATION", GL_CONSTANT_ATTENUATION),
   Constant("GL-LINEAR-ATTENUATION", GL_LINEAR_ATTENUATION),
   Constant("GL-QUADRATIC-ATTENUATION", GL_QUADRATIC_ATTENUATION),
   // light models
   Constant("GL-LIGHT-MODEL-AMBIENT", GL_LIGHT_MODEL_AMBIENT),
   Constant("GL-LIGHT-MODEL-LOCAL-VIEWER", GL_LIGHT_MODEL_LOCAL_VIEWER),
   Constant("GL-LIGHT-MODEL-TWO-SIDE", GL_LIGHT_MODEL_TWO_SIDE),
   // new list modes
   Constant("GL-COMPILE", GL_COMPILE),
   Constant("GL-COMPILE-AND-EXECUTE", GL_COMPILE_AND_EXECUTE),
   // pixel copy types
   Constant("GL-COLOR", GL_COLOR),
   Constant("GL-STENCIL", GL_STENCIL),
   Constant("GL-DEPTH", GL_DEPTH),
   // draw-pixel formats
   Constant("GL-COLOR-INDEX", GL_COLOR_INDEX),
   Constant("GL-LUMINANCE", GL_LUMINANCE),
   Constant("GL-LUMINANCE-ALPHA", GL_LUMINANCE_ALPHA),
   Constant("GL-RGB", GL_RGB),
   Constant("GL-RGBA", GL_RGBA),
   Constant("GL-RED", GL_RED),
   Constant("GL-GREEN", GL_GREEN),
   Constant("GL-BLUE", GL_BLUE),
   Constant("GL-ALPHA", GL_ALPHA),
   Constant("GL-STENCIL-INDEX", GL_STENCIL_INDEX),
   Constant("GL-DEPTH-COMPONENT", GL_DEPTH_COMPONENT),
   // pixel map types
   Constant("GL-PIXEL-MAP-I-TO-I", GL_PIXEL_MAP_I_TO_I),
   Constant("GL-PIXEL-MAP-S-TO-S", GL_PIXEL_MAP_S_TO_S),
   Constant("GL-PIXEL-MAP-I-TO-R", GL_PIXEL_MAP_I_TO_R),
   Constant("GL-PIXEL-MAP-I-TO-G", GL_PIXEL_MAP_I_TO_G),
   Constant("GL-PIXEL-MAP-I-TO-B", GL_PIXEL_MAP_I_TO_B),
   Constant("GL-PIXEL-MAP-I-TO-A", GL_PIXEL_MAP_I_TO_A),
   Constant("GL-PIXEL-MAP-R-TO-R", GL_PIXEL_MAP_R_TO_R),
   Constant("GL-PIXEL-MAP-G-TO-G", GL_PIXEL_MAP_G_TO_G),
   Constant("GL-PIXEL-MAP-B-TO-B", GL_PIXEL_MAP_B_TO_B),
   Constant("GL-PIXEL-MAP-A-TO-A", GL_PIXEL_MAP_A_TO_A),
   // pixel store modes
   Constant("GL-PACK-SWAP-BYTES", GL_PACK_SWAP_BYTES),
   Constant("GL-PACK-LSB-FIRST", GL_PACK_LSB_FIRST),
   Constant("GL-PACK-ROW-LENGTH", GL_PACK_ROW_LENGTH),
   Constant("GL-PACK-SKIP-PIXELS", GL_PACK_SKIP_PIXELS),
   Constant("GL-PACK-SKIP-ROWS", GL_PACK_SKIP_ROWS),
   Constant("GL-PACK-ALIGNMENT", GL_PACK_ALIGNMENT),
   Constant("GL-UNPACK-SWAP-BYTES", GL_UNPACK_SWAP_BYTES),
   Constant("GL-UNPACK-LSB-FIRST", GL_UNPACK_LSB_FIRST),
   Constant("GL-UNPACK-ROW-LENGTH", GL_UNPACK_ROW_LENGTH),
   Constant("GL-UNPACK-SKIP-PIXELS", GL_UNPACK_SKIP_PIXELS),
   Constant("GL-UNPACK-SKIP-ROWS", GL_UNPACK_SKIP_ROWS),
   Constant("GL-UNPACK-ALIGNMENT", GL_UNPACK_ALIGNMENT),
   Constant("GL-UNPACK-ALIGNMENT", GL_UNPACK_ALIGNMENT),
   // pixel transfer modes
   Constant("GL-MAP-COLOR", GL_MAP_COLOR),
   Constant("GL-MAP-STENCIL", GL_MAP_STENCIL),
   Constant("GL-INDEX-SHIFT", GL_INDEX_SHIFT),
   Constant("GL-INDEX-OFFSET", GL_INDEX_OFFSET),
   Constant("GL-RED-SCALE", GL_RED_SCALE),
   Constant("GL-RED-BIAS", GL_RED_BIAS),
   Constant("GL-GREEN-SCALE", GL_GREEN_SCALE),
   Constant("GL-GREEN-BIAS", GL_GREEN_BIAS),
   Constant("GL-BLUE-SCALE", GL_BLUE_SCALE),
   Constant("GL-BLUE-BIAS", GL_BLUE_BIAS),
   Constant("GL-ALPHA-SCALE", GL_ALPHA_SCALE),
   Constant("GL-ALPHA-BIAS", GL_ALPHA_BIAS),
   Constant("GL-DEPTH-SCALE", GL_DEPTH_SCALE),
   Constant("GL-DEPTH-BIAS", GL_DEPTH_BIAS),
   // texture environments
   Constant("GL-TEXTURE-ENV", GL_TEXTURE_ENV),
   Constant("GL-TEXTURE-ENV-MODE", GL_TEXTURE_ENV_MODE),
   Constant("GL-TEXTURE-ENV-COLOR", GL_TEXTURE_ENV_COLOR),
   Constant("GL-DECAL", GL_DECAL),
   Constant("GL-BLEND", GL_BLEND),
   Constant("GL-MODULATE", GL_MODULATE),
   // texture gen
   Constant("GL-S", GL_S),
   Constant("GL-T", GL_T),
   Constant("GL-R", GL_R),
   Constant("GL-Q", GL_Q),
   Constant("GL-TEXTURE-GEN-MODE", GL_TEXTURE_GEN_MODE),
   Constant("GL-OBJECT-PLANE", GL_OBJECT_PLANE),
   Constant("GL-EYE-PLANE", GL_EYE_PLANE),
   Constant("GL-OBJECT-LINEAR", GL_OBJECT_LINEAR),
   Constant("GL-EYE-LINEAR", GL_EYE_LINEAR),
   Constant("GL-SPHERE-MAP", GL_SPHERE_MAP),
   // texture parameters
   Constant("GL-TEXTURE-MIN-FILTER", GL_TEXTURE_MIN_FILTER),
   Constant("GL-TEXTURE-MAG-FILTER", GL_TEXTURE_MAG_FILTER),
   Constant("GL-TEXTURE-WRAP-S", GL_TEXTURE_WRAP_S),
   Constant("GL-TEXTURE-WRAP-T", GL_TEXTURE_WRAP_T),
   Constant("GL-TEXTURE-PRIORITY", GL_TEXTURE_PRIORITY),
   Constant("GL-CLAMP", GL_CLAMP),
   Constant("GL-REPEAT", GL_REPEAT),
   // Constant("GL-BORDER-COLOR", GL_BORDER_COLOR),
   Constant("GL-NEAREST", GL_NEAREST),
   Constant("GL-LINEAR", GL_LINEAR),
   Constant("GL-NEAREST-MIPMAP-NEAREST", GL_NEAREST_MIPMAP_NEAREST),
   Constant("GL-LINEAR-MIPMAP-NEAREST", GL_LINEAR_MIPMAP_NEAREST),
   Constant("GL-NEAREST-MIPMAP-LINEAR", GL_NEAREST_MIPMAP_LINEAR),
   Constant("GL-LINEAR-MIPMAP-LINEAR", GL_LINEAR_MIPMAP_LINEAR),
   // blend factor
   Constant("GL-ZERO", GL_ZERO),
   Constant("GL-ONE", GL_ONE),
   Constant("GL-SRC-COLOR", GL_SRC_COLOR),
   Constant("GL-ONE-MINUS-SRC-COLOR", GL_ONE_MINUS_SRC_COLOR),
   Constant("GL-DST-COLOR", GL_DST_COLOR),
   Constant("GL-ONE-MINUS-DST-COLOR", GL_ONE_MINUS_DST_COLOR),
   Constant("GL-SRC-ALPHA", GL_SRC_ALPHA),
   Constant("GL-ONE-MINUS-SRC-ALPHA", GL_ONE_MINUS_SRC_ALPHA),
   Constant("GL-DST-ALPHA", GL_DST_ALPHA),
   Constant("GL-ONE-MINUS-DST-ALPHA", GL_ONE_MINUS_DST_ALPHA),
   Constant("GL-SRC-ALPHA-SATURATE", GL_SRC_ALPHA_SATURATE),
   Constant("GL-CONSTANT-COLOR", GL_CONSTANT_COLOR),
   Constant("GL-ONE-MINUS-CONSTANT-COLOR", GL_ONE_MINUS_CONSTANT_COLOR),
   Constant("GL-CONSTANT-ALPHA", GL_CONSTANT_ALPHA),
   Constant("GL-ONE-MINUS-CONSTANT-ALPHA", GL_ONE_MINUS_CONSTANT_ALPHA),
   // fog
   Constant("GL-FOG", GL_FOG),
   Constant("GL-FOG-COLOR", GL_FOG_COLOR),
   Constant("GL-FOG-DENSITY", GL_FOG_DENSITY),
   Constant("GL-FOG-END", GL_FOG_END),
   Constant("GL-FOG-MODE", GL_FOG_MODE),
   Constant("GL-FOG-START", GL_FOG_START),
   Constant("GL-FOG-INDEX", GL_FOG_INDEX),
   Constant("GL-FOG-COORD-SRC", GL_FOG_COORD_SRC),
   Constant("GL-FOG-COORD", GL_FOG_COORD),
   Constant("GL-FRAGMENT-DEPTH", GL_FRAGMENT_DEPTH),
   // eval mesh modes
   Constant("GL-POINT", GL_POINT),
   Constant("GL-LINE", GL_LINE),
   Constant("GL-FILL", GL_FILL),
   // mapget targets
   Constant("GL-MAP1-COLOR-4", GL_MAP1_COLOR_4),
   Constant("GL-MAP1-INDEX", GL_MAP1_INDEX),
   Constant("GL-MAP1-N0RMAL", GL_MAP1_NORMAL),
   Constant("GL-MAP1-TEXTURE-COORD-1", GL_MAP1_TEXTURE_COORD_1),
   Constant("GL-MAP1-TEXTURE-COORD-2", GL_MAP1_TEXTURE_COORD_2),
   Constant("GL-MAP1-TEXTURE-COORD-3", GL_MAP1_TEXTURE_COORD_3),
   Constant("GL-MAP1-TEXTURE-COORD-4", GL_MAP1_TEXTURE_COORD_4),
   Constant("GL-MAP1-VERTEX-3", GL_MAP1_VERTEX_3),
   Constant("GL-MAP1-VERTEX-4", GL_MAP1_VERTEX_4),
   Constant("GL-MAP2-COLOR-4", GL_MAP2_COLOR_4),
   Constant("GL-MAP2-INDEX", GL_MAP2_INDEX),
   Constant("GL-MAP2-N0RMAL", GL_MAP2_NORMAL),
   Constant("GL-MAP2-TEXTURE-COORD-1", GL_MAP2_TEXTURE_COORD_1),
   Constant("GL-MAP2-TEXTURE-COORD-2", GL_MAP2_TEXTURE_COORD_2),
   Constant("GL-MAP2-TEXTURE-COORD-3", GL_MAP2_TEXTURE_COORD_3),
   Constant("GL-MAP2-TEXTURE-COORD-4", GL_MAP2_TEXTURE_COORD_4),
   Constant("GL-MAP2-VERTEX-3", GL_MAP2_VERTEX_3),
   Constant("GL-MAP2-VERTEX-4", GL_MAP2_VERTEX_4),
   // mapget queries
   Constant("GL-COEFF", GL_COEFF),
   Constant("GL-ORDER", GL_ORDER),
   Constant("GL-DOMAIN", GL_DOMAIN),
   // feedback buffers
   Constant("GL-2D", GL_2D),
   Constant("GL-3D", GL_3D),
   Constant("GL-3D-COLOR", GL_3D_COLOR),
   Constant("GL-3D-COLOR-TEXTURE", GL_3D_COLOR_TEXTURE),
   Constant("GL-4D-COLOR-TEXTURE", GL_4D_COLOR_TEXTURE),
   // depth functions
   Constant("GL-NEVER", GL_NEVER),
   Constant("GL-LESS", GL_LESS),
   Constant("GL-EQUAL", GL_EQUAL),
   Constant("GL-LEQUAL", GL_LEQUAL),
   Constant("GL-NOTEQUAL", GL_NOTEQUAL),
   Constant("GL-GREATER", GL_GREATER),
   Constant("GL-GEQUAL", GL_GEQUAL),
   Constant("GL-ALWAYS", GL_ALWAYS),
   // glu quadric draw styles
   Constant("GLU-FILL", GLU_FILL),
   Constant("GLU-LINE", GLU_LINE),
   Constant("GLU-SILHOUETTE", GLU_SILHOUETTE),
   Constant("GLU-POINT", GLU_POINT),
   // glu quadric normals
   Constant("GLU-NONE", GLU_NONE),
   Constant("GLU-FLAT", GLU_FLAT),
   Constant("GLU-SMOOTH", GLU_SMOOTH),
   // glu quadric orientation
   Constant("GLU-INSIDE", GLU_INSIDE),
   Constant("GLU-OUTSIDE", GLU_OUTSIDE),
   // glu quadric texturing
   Constant("GLU-TRUE", GLU_TRUE),
   Constant("GLU-FALSE", GLU_FALSE),
   // pixel data types
   Constant("GL-BYTE", GL_BYTE),
   Constant("GL-UNSIGNED-BYTE", GL_UNSIGNED_BYTE),
   Constant("GL-SHORT", GL_SHORT),
   Constant("GL-UNSIGNED-SHORT", GL_UNSIGNED_SHORT),
   Constant("GL-INT", GL_INT),
   Constant("GL-UNSIGNED-INT", GL_UNSIGNED_INT),
   Constant("GL-BITMAP", GL_BITMAP),
   Constant("GL-FLOAT", GL_FLOAT),
   Constant("", 0)
 };
 return add_constants(env, constants, "spark-opengl");
}


spark::Status_code
_add_procedures(Scheme_Env* env)
{
 using spark::Procedure;
 Procedure* procedures[] = {
   new Procedure(spark_opengl::gl_begin, "gl-begin", 1),
   new Procedure(spark_opengl::gl_end, "gl-end", 0),
   new Procedure(spark_opengl::gl_enable, "gl-enable", 1),
   new Procedure(spark_opengl::gl_disable, "gl-disable", 1),
   new Procedure(spark_opengl::gl_clear, "gl-clear", 1),
   new Procedure(spark_opengl::gl_depth_func, "gl-depth-func", 1),
   new Procedure(spark_opengl::gl_clear_color, "gl-clear-color", 4),
   new Procedure(spark_opengl::gl_clear_depth, "gl-clear-depth", 1),
   new Procedure(spark_opengl::gl_flush, "gl-flush", 0),
   new Procedure(spark_opengl::gl_ortho, "gl-ortho", 6),
   new Procedure(spark_opengl::gl_viewport, "gl-viewport", 4),
   new Procedure(spark_opengl::gl_rectd, "gl-rectd", 4),
   new Procedure(spark_opengl::gl_rectf, "gl-rectf", 4),
   new Procedure(spark_opengl::gl_recti, "gl-recti", 4),
   new Procedure(spark_opengl::gl_rects, "gl-rects", 4),
   new Procedure(spark_opengl::gl_geterror, "gl-geterror", 0),
   new Procedure(spark_opengl::gl_getstring, "gl-getstring", 1),
   new Procedure(spark_opengl::gl_hint, "gl-hint", 2),
   new Procedure(spark_opengl::gl_cull_face, "gl-cull-face", 1),
   new Procedure(spark_opengl::gl_edge_flag, "gl-edge-flag", 1),
   new Procedure(spark_opengl::gl_front_face, "gl-front-face", 1),
   new Procedure(spark_opengl::gl_line_stipple, "gl-line-stipple", 2),
   new Procedure(spark_opengl::gl_line_width, "gl-line-width", 1),
   new Procedure(spark_opengl::gl_point_size, "gl-point-size", 1),
   new Procedure(spark_opengl::gl_polygon_mode, "gl-polygon-mode", 2),
   new Procedure(spark_opengl::gl_polygon_stipple, "gl-polygon-stipple", 2),
   new Procedure(spark_opengl::gl_vertex_d, "gl-vertex-d", 2, 4),
   new Procedure(spark_opengl::gl_vertex_f, "gl-vertex-f", 2, 4),
   new Procedure(spark_opengl::gl_vertex_i, "gl-vertex-i", 2, 4),
   new Procedure(spark_opengl::gl_vertex_s, "gl-vertex-s", 2, 4),
   new Procedure(spark_opengl::gl_frustum, "gl-frustum", 6),
   new Procedure(spark_opengl::gl_load_identity, "gl-load-identity", 0),
   new Procedure(spark_opengl::gl_load_matrix_d, "gl-load-matrix-d", 16),
   new Procedure(spark_opengl::gl_load_matrix_f, "gl-load-matrix-f", 16),
   new Procedure(spark_opengl::gl_mult_matrix_d, "gl-mult-matrix-d", 16),
   new Procedure(spark_opengl::gl_mult_matrix_f, "gl-mult-matrix-f", 16),
   new Procedure(spark_opengl::gl_matrix_mode, "gl-matrix-mode", 1),
   new Procedure(spark_opengl::gl_push_matrix, "gl-push-matrix", 0),
   new Procedure(spark_opengl::gl_pop_matrix, "gl-pop-matrix", 0),
   new Procedure(spark_opengl::gl_matrix_transform_d, "gl-matrix-transform-d", 4, 5),
   new Procedure(spark_opengl::gl_matrix_transform_f, "gl-matrix-transform-f", 4, 5),
   new Procedure(spark_opengl::gl_clear_index, "gl-clear-index", 1),
   new Procedure(spark_opengl::gl_color_b, "gl-color-b", 3, 4),
   new Procedure(spark_opengl::gl_color_d, "gl-color-d", 3, 4),
   new Procedure(spark_opengl::gl_color_f, "gl-color-f", 3, 4),
   new Procedure(spark_opengl::gl_color_i, "gl-color-i", 3, 4),
   new Procedure(spark_opengl::gl_color_s, "gl-color-s", 3, 4),
   new Procedure(spark_opengl::gl_color_ub, "gl-color-ub", 3, 4),
   new Procedure(spark_opengl::gl_color_ui, "gl-color-ui", 3, 4),
   new Procedure(spark_opengl::gl_color_us, "gl-color-us", 3, 4),
   new Procedure(spark_opengl::gl_color_mask, "gl-color-mask", 4),
   new Procedure(spark_opengl::gl_index_d, "gl-index-d", 1),
   new Procedure(spark_opengl::gl_index_f, "gl-index-f", 1),
   new Procedure(spark_opengl::gl_index_i, "gl-index-i", 1),
   new Procedure(spark_opengl::gl_index_s, "gl-index-s", 1),
   new Procedure(spark_opengl::gl_index_mask, "gl-index-mask", 1),
   new Procedure(spark_opengl::gl_logic_op, "gl-logic-op", 1),
   new Procedure(spark_opengl::gl_shade_model, "gl-shade-model", 1),
   new Procedure(spark_opengl::gl_color_material, "gl-color-material", 2),
   new Procedure(spark_opengl::gl_get_material_f, "gl-get-material-f", 2),
   new Procedure(spark_opengl::gl_get_material_i, "gl-get-material-i", 2),
   new Procedure(spark_opengl::gl_get_light_f, "gl-get-light-f", 2),
   new Procedure(spark_opengl::gl_get_light_i, "gl-get-light-i", 2),
   new Procedure(spark_opengl::gl_light_f, "gl-light-f", 3),
   new Procedure(spark_opengl::gl_light_i, "gl-light-i", 3),
   new Procedure(spark_opengl::gl_light_f, "gl-light-model-f", 2),
   new Procedure(spark_opengl::gl_light_i, "gl-light-model-i", 2),
   new Procedure(spark_opengl::gl_material_f, "gl-material-f", 3),
   new Procedure(spark_opengl::gl_material_i, "gl-material-i", 3),
   new Procedure(spark_opengl::gl_normal_3b, "gl-normal-3b", 3),
   new Procedure(spark_opengl::gl_normal_3d, "gl-normal-3d", 3),
   new Procedure(spark_opengl::gl_normal_3f, "gl-normal-3f", 3),
   new Procedure(spark_opengl::gl_normal_3i, "gl-normal-3i", 3),
   new Procedure(spark_opengl::gl_normal_3s, "gl-normal-3s", 3),
   new Procedure(spark_opengl::gl_call_list, "gl-call-list", 1),
   new Procedure(spark_opengl::gl_call_lists, "gl-call-lists", 1),
   new Procedure(spark_opengl::gl_delete_lists, "gl-delete-lists", 2),
   new Procedure(spark_opengl::gl_end_list, "gl-end-list", 0),
   new Procedure(spark_opengl::gl_gen_lists, "gl-gen-lists", 1),
   new Procedure(spark_opengl::gl_is_list, "gl-is-list", 1),
   new Procedure(spark_opengl::gl_list_base, "gl-list-base", 1),
   new Procedure(spark_opengl::gl_new_list, "gl-new-list", 2),
   new Procedure(spark_opengl::gl_copy_pixels, "gl-copy-pixels", 5),
   new Procedure(spark_opengl::gl_draw_pixels, "gl-draw-pixels", 4),
   new Procedure(spark_opengl::gl_pixel_map_f, "gl-pixel-map-f", 2),
   new Procedure(spark_opengl::gl_pixel_map_ui, "gl-pixel-map-ui", 2),
   new Procedure(spark_opengl::gl_pixel_map_us, "gl-pixel-map-us", 2),
   new Procedure(spark_opengl::gl_pixel_store_i, "gl-pixel-store-i", 2),
   new Procedure(spark_opengl::gl_pixel_store_f, "gl-pixel-store-f", 2),
   new Procedure(spark_opengl::gl_pixel_transfer_i, "gl-pixel-transfer-i", 2),
   new Procedure(spark_opengl::gl_pixel_transfer_f, "gl-pixel-transfer-f", 2),
   new Procedure(spark_opengl::gl_pixel_zoom, "gl-pixel-zoom", 2),
   new Procedure(spark_opengl::gl_read_pixels, "gl-read-pixels", 6),
   new Procedure(spark_opengl::gl_gen_textures, "gl-gen-textures", 1),
   new Procedure(spark_opengl::gl_bind_texture, "gl-bind-texture", 2),
   new Procedure(spark_opengl::gl_delete_textures, "gl-delete-textures", 1),
   new Procedure(spark_opengl::gl_tex_coord_f, "gl-tex-coord-f", 1, 4),
   new Procedure(spark_opengl::gl_tex_coord_d, "gl-tex-coord-d", 1, 4),
   new Procedure(spark_opengl::gl_tex_coord_i, "gl-tex-coord-i", 1, 4),
   new Procedure(spark_opengl::gl_tex_coord_s, "gl-tex-coord-s", 1, 4),
   new Procedure(spark_opengl::gl_tex_env_f, "gl-tex-env-f", 3),
   new Procedure(spark_opengl::gl_tex_env_i, "gl-tex-env-i", 3),
   new Procedure(spark_opengl::gl_tex_gen_d, "gl-tex-gen-d", 3),
   new Procedure(spark_opengl::gl_tex_gen_i, "gl-tex-gen-i", 3),
   new Procedure(spark_opengl::gl_tex_gen_f, "gl-tex-gen-f", 3),
   new Procedure(spark_opengl::gl_tex_image, "gl-tex-image", 7, 8),
   new Procedure(spark_opengl::gl_tex_parameter_f, "gl-tex-parameter-f", 3),
   new Procedure(spark_opengl::gl_tex_parameter_i, "gl-tex-parameter-i", 3),
   new Procedure(spark_opengl::gl_blend_func, "gl-blend-func", 2),
   new Procedure(spark_opengl::gl_fog_f, "gl-fog-f", 2),
   new Procedure(spark_opengl::gl_fog_i, "gl-fog-i", 2),
   new Procedure(spark_opengl::gl_eval_coord_d, "gl-eval-coord-d", 1, 2),
   new Procedure(spark_opengl::gl_eval_coord_f, "gl-eval-coord-f", 1, 2),
   new Procedure(spark_opengl::gl_eval_mesh, "gl-eval-mesh", 3, 5),
   new Procedure(spark_opengl::gl_eval_point, "gl-eval-point", 1, 2),
   new Procedure(spark_opengl::gl_pixel_transfer_f, "gl-pixel-transfer-f", 2),
   new Procedure(spark_opengl::gl_pixel_transfer_i, "gl-pixel-transfer-i", 2),
   new Procedure(spark_opengl::gl_get_map, "gl-get-map", 3),
   new Procedure(spark_opengl::gl_map_d, "gl-map-d", 6, 10),
   new Procedure(spark_opengl::gl_map_f, "gl-map-f", 6, 10),
   new Procedure(spark_opengl::gl_map_grid_d, "gl-map-grid-d", 3, 6),
   new Procedure(spark_opengl::gl_map_grid_f, "gl-map-grid-f", 3, 6),
   new Procedure(spark_opengl::gl_feedback_buffer, "gl-feedback-buffer", 2),
   new Procedure(spark_opengl::gl_get_feedback_buffer, "gl-get-feedback-buffer", 1, 2),
   new Procedure(spark_opengl::gl_init_names, "gl-init-names", 0),
   new Procedure(spark_opengl::gl_push_name, "gl-push-name", 1),
   new Procedure(spark_opengl::gl_pop_name, "gl-pop-name", 0),
   new Procedure(spark_opengl::gl_load_name, "gl-load-name", 1),
   new Procedure(spark_opengl::gl_pass_through, "gl-pass-through", 1),
   new Procedure(spark_opengl::gl_select_buffer, "gl-select-buffer", 1),
   new Procedure(spark_opengl::gl_get_select_buffer, "gl-get-select-buffer", 1, 2),
   new Procedure(spark_opengl::gl_render_mode, "gl-render-mode", 1),
   // GLU
   new Procedure(spark_opengl::glu_lookat, "glu-lookat", 9),
   new Procedure(spark_opengl::glu_newquadric, "glu-new-quadric", 0),
   new Procedure(spark_opengl::glu_deletequadric, "glu-delete-quadric", 1),
   new Procedure(spark_opengl::glu_quadric_drawstyle, "glu-quadric-drawstyle", 2),
   new Procedure(spark_opengl::glu_quadric_normals, "glu-quadric-normals", 2),
   new Procedure(spark_opengl::glu_quadric_orientation, "glu-quadric-orientation", 2),
   new Procedure(spark_opengl::glu_quadric_texture, "glu-quadric-texture", 2),
   new Procedure(spark_opengl::glu_cylinder, "glu-cylinder", 6),
   new Procedure(spark_opengl::glu_disk, "glu-disk", 5),
   new Procedure(spark_opengl::glu_partial_disk, "glu-partial-disk", 7),
   new Procedure(spark_opengl::glu_sphere, "glu-sphere", 4),
   0
 };
 return spark::add_procedures(env, procedures, "spark-opengl");
}

// Exported OpenGL functions

Scheme_Object*
spark_opengl::gl_begin(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int v = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], v))
   {
     scheme_wrong_type("gl-begin", "int",
                       0, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }
 
 glBegin(v);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_end(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glEnd();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_clear_color(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;


 GLclampf_list colors;
 if (!_get_args_list(argc, argv, 0, 4, colors))
   {
     DEFAULT_RET_FINISH;
   }
 
 glClearColor(colors[0], colors[1], colors[2], colors[3]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_clear_depth(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 double v = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[0], v))
   {
     scheme_wrong_type("gl-clear-depth", "double",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 
 glClearDepth(static_cast<GLclampd>(v));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_depth_func(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int f = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], f))
   {
     scheme_wrong_type("gl-depth-func", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 
 glDepthFunc(static_cast<GLenum>(f));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_clear(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int opt = 0;
 if (!SCHEME_LISTP(argv[0]))
   {
     if (!spark::Utils::int_from_scheme_long(argv[0], opt))
       {
         scheme_wrong_type("gl-clear", "int",
                           0, argc,
                           argv);
         DEFAULT_RET_FINISH;
       }
   }
 else
   {
     Scheme_Object* lst = argv[0];
     while (lst)
       {
         if (lst == scheme_null)
           break;
         Scheme_Object* v = SCHEME_CAR(lst);
         int t = 0;
         if (!spark::Utils::int_from_scheme_long(argv[0], t))
           {
             scheme_wrong_type("gl-clear", "int-list",
                               0, argc,
                               argv);
             DEFAULT_RET_FINISH;
           }
         opt |= t;
         lst = SCHEME_CDR(lst);
       }
   }

 glClear(opt);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_flush(int, Scheme_Object**)
{
 DEFAULT_RET_INIT;

 glFlush();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

// Sets or modifies the clipping volume extents.
Scheme_Object*
spark_opengl::gl_ortho(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, 6, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glOrtho(vals[0], vals[1], vals[2], vals[3],
         vals[4], vals[5]);
 _ret_ = scheme_true;    

 DEFAULT_RET_FINISH;
}

// Sets the portion of a window that can be drawn in by OpenGL.
Scheme_Object*
spark_opengl::gl_viewport(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint x = 0;
 if (!_glint_from_scheme_long(argv[0], x))
   {
     scheme_wrong_type("gl-viewport", "int",
                       0, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }  
 GLint y = 0;
 if (!_glint_from_scheme_long(argv[1], y))
   {
     scheme_wrong_type("gl-viewport", "int",
                       1, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }  
 GLsizei width = 0;
 if (!_glsizei_from_scheme_long(argv[2], width))
   {
     scheme_wrong_type("gl-viewport", "int",
                       2, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }  
 GLsizei height = 0;
 if (!_glsizei_from_scheme_long(argv[3], height))
   {
     scheme_wrong_type("gl-viewport", "int",
                       3, argc,
                       argv);
     DEFAULT_RET_FINISH;
   }  


 glViewport(x, y, width, height);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_rectd(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, 4, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glRectd(vals[0], vals[1], vals[2], vals[3]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_rectf(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, 4, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glRectf(vals[0], vals[1], vals[2], vals[3]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_recti(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, 4, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glRecti(vals[0], vals[1], vals[2], vals[3]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_rects(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLshort_list vals;
 if (!_get_args_list(argc, argv, 0, 4, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glRects(vals[0], vals[1], vals[2], vals[3]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_geterror(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = static_cast<int>(glGetError());
 _ret_ = scheme_make_integer(r);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_getstring(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-getstring", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum e = static_cast<GLenum>(r);
 const GLubyte* s = glGetString(e);
 if (s)
   {
     _ret_ = scheme_make_utf8_string(reinterpret_cast<const char*>(s));
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_hint(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-hint", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(r);

 r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], r))
   {
     scheme_wrong_type("gl-hint", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(r);
 
 glHint(target, mode);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_cull_face(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-cull-face", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum e = static_cast<GLenum>(r);
 
 glCullFace(e);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_edge_flag(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLboolean flag = (argv[0] == scheme_true) ? GL_TRUE : GL_FALSE;
 glEdgeFlag(flag);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_front_face(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-front-face", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum e = static_cast<GLenum>(r);
 
 glFrontFace(e);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_line_stipple(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-line-stipple", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }


 GLint factor = static_cast<GLint>(r);

 short s = 0;
 if (!spark::Utils::short_from_scheme_long(argv[1], s))
   {
     scheme_wrong_type("gl-line-stipple", "short",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLshort pattern = static_cast<GLshort>(s);
 
 glLineStipple(factor, pattern);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_line_width(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float r = 0;
 if (!spark::Utils::float_from_scheme_double(argv[0], r))
   {
     scheme_wrong_type("gl-line-width", "float",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLfloat w = static_cast<GLfloat>(r);
 
 glLineWidth(w);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_point_size(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float r = 0;
 if (!spark::Utils::float_from_scheme_double(argv[0], r))
   {
     scheme_wrong_type("gl-point-size", "float",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLfloat w = static_cast<GLfloat>(r);
 
 glPointSize(w);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_polygon_mode(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-polygon-mode", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum face = static_cast<GLenum>(r);

 r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], r))
   {
     scheme_wrong_type("gl-polygon-mode", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(r);
 
 glPolygonMode(face, mode);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_polygon_stipple(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_LISTP(argv[0]))
   {
     scheme_wrong_type("gl-polygon-stipple", "list",
		       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 
 Scheme_Object* lst = argv[0];
 GLubyte* pattern = 0;
 int len = 0;
 if (lst)
   {
     len = scheme_list_length(lst);
     pattern = new GLubyte[len];
   }
 if (len == 0)
   {
     DEFAULT_RET_FINISH;
   }
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     unsigned char r = 0;
     if (!spark::Utils::uchar_from_scheme_long(obj, r))
       {
	 pattern[i++] = (static_cast<GLubyte>(r));
       }
     if (i >= len)
       break;
   }
 glPolygonStipple(pattern);
 delete[] pattern;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_vertex_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 switch (argc)
   {
   case 2:
     glVertex2d(vals[0], vals[1]);
     break;
   case 3:
     glVertex3d(vals[0], vals[1], vals[2]);
     break;
   case 4:
     glVertex4d(vals[0], vals[1], vals[2], vals[3]);
     break;
   }
 
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_vertex_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 switch (argc)
   {
   case 2:
     glVertex2f(vals[0], vals[1]);
     break;
   case 3:
     glVertex3f(vals[0], vals[1], vals[2]);
     break;
   case 4:
     glVertex4f(vals[0], vals[1], vals[2], vals[3]);
     break;
   }
 
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_vertex_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 switch (argc)
   {
   case 2:
     glVertex2i(vals[0], vals[1]);
     break;
   case 3:
     glVertex3i(vals[0], vals[1], vals[2]);
     break;
   case 4:
     glVertex4i(vals[0], vals[1], vals[2], vals[3]);
     break;
   }
 
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_vertex_s(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLshort_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 switch (argc)
   {
   case 2:
     glVertex2s(vals[0], vals[1]);
     break;
   case 3:
     glVertex3s(vals[0], vals[1], vals[2]);
     break;
   case 4:
     glVertex4s(vals[0], vals[1], vals[2], vals[3]);
     break;
   }
 
 _ret_ = scheme_true;


 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_frustum(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glFrustum(vals[0], vals[1], vals[2],
           vals[3], vals[4], vals[5]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_load_identity(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glLoadIdentity();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_push_matrix(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glPushMatrix();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pop_matrix(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glPopMatrix();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_load_matrix_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 size_t sz = vals.size();
 GLdouble* array = new GLdouble[sz];
 for (size_t i=0; i<sz; ++i)
   array[i] = vals[i];

 glLoadMatrixd(array);

 delete[] array;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_load_matrix_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 size_t sz = vals.size();
 GLfloat* array = new GLfloat[sz];
 for (size_t i=0; i<sz; ++i)
   array[i] = vals[i];

 glLoadMatrixf(array);

 delete[] array;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_mult_matrix_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 size_t sz = vals.size();
 GLdouble* array = new GLdouble[sz];
 for (size_t i=0; i<sz; ++i)
   array[i] = vals[i];

 glMultMatrixd(array);

 delete[] array;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_mult_matrix_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 size_t sz = vals.size();
 GLfloat* array = new GLfloat[sz];
 for (size_t i=0; i<sz; ++i)
   array[i] = vals[i];

 glMultMatrixf(array);

 delete[] array;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_matrix_mode(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int r = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], r))
   {
     scheme_wrong_type("gl-matrix-mode", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(r);

 glMatrixMode(mode);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_matrix_transform_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_SYMBOLP(argv[0]))
   {
     scheme_wrong_type("gl-matrix-transform-d", "symbol",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 
 std::string trans = SCHEME_SYM_VAL(argv[0]);
 if (trans == "rotate")
   {
     GLdouble_list vals;
     if (!_get_args_list(argc, argv, 1, 4, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glRotated(vals[0], vals[1], vals[2], vals[3]);
     _ret_ = scheme_true;
   }
 else if (trans == "scale")
   {
     GLdouble_list vals;
     if (!_get_args_list(argc, argv, 1, 3, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glScaled(vals[0], vals[1], vals[2]);
     _ret_ = scheme_true;
   }
 else if (trans == "translate")
   {
     GLdouble_list vals;
     if (!_get_args_list(argc, argv, 1, 3, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glTranslated(vals[0], vals[1], vals[2]);
     _ret_ = scheme_true;
   }


 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_matrix_transform_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_SYMBOLP(argv[0]))
   {
     scheme_wrong_type("gl-matrix-transform-f", "symbol",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 
 std::string trans = SCHEME_SYM_VAL(argv[0]);
 if (trans == "rotate")
   {
     GLfloat_list vals;
     if (!_get_args_list(argc, argv, 1, 4, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glRotatef(vals[0], vals[1], vals[2], vals[3]);
     _ret_ = scheme_true;
   }
 else if (trans == "scale")
   {
     GLfloat_list vals;
     if (!_get_args_list(argc, argv, 1, 3, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glScalef(vals[0], vals[1], vals[2]);
     _ret_ = scheme_true;
   }
 else if (trans == "translate")
   {
     GLfloat_list vals;
     if (!_get_args_list(argc, argv, 1, 3, vals))
       {
         DEFAULT_RET_FINISH;
       }
     glTranslatef(vals[0], vals[1], vals[2]);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_clear_index(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float f = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[0], f))
   {
     scheme_wrong_type("gl-clear-index", "float",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLfloat color = static_cast<GLfloat>(f);

 glClearIndex(color);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_b(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3b(static_cast<GLbyte>(vals[0]),
             static_cast<GLbyte>(vals[1]),
             static_cast<GLbyte>(vals[2]));
 else
   glColor4b(static_cast<GLbyte>(vals[0]),
             static_cast<GLbyte>(vals[1]),
             static_cast<GLbyte>(vals[2]),
             static_cast<GLbyte>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_ub(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3ub(static_cast<GLubyte>(vals[0]),
             static_cast<GLubyte>(vals[1]),
             static_cast<GLubyte>(vals[2]));
 else
   glColor4ub(static_cast<GLubyte>(vals[0]),
             static_cast<GLubyte>(vals[1]),
             static_cast<GLubyte>(vals[2]),
             static_cast<GLubyte>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3d(static_cast<GLdouble>(vals[0]),
             static_cast<GLdouble>(vals[1]),
             static_cast<GLdouble>(vals[2]));
 else
   glColor4d(static_cast<GLdouble>(vals[0]),
             static_cast<GLdouble>(vals[1]),
             static_cast<GLdouble>(vals[2]),
             static_cast<GLdouble>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3f(static_cast<GLfloat>(vals[0]),
             static_cast<GLfloat>(vals[1]),
             static_cast<GLfloat>(vals[2]));
 else
   glColor4f(static_cast<GLfloat>(vals[0]),
             static_cast<GLfloat>(vals[1]),
             static_cast<GLfloat>(vals[2]),
             static_cast<GLfloat>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3i(static_cast<GLint>(vals[0]),
             static_cast<GLint>(vals[1]),
             static_cast<GLint>(vals[2]));
 else
   glColor4i(static_cast<GLint>(vals[0]),
             static_cast<GLint>(vals[1]),
             static_cast<GLint>(vals[2]),
             static_cast<GLint>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}


Scheme_Object*
spark_opengl::gl_color_ui(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3ui(static_cast<GLuint>(vals[0]),
             static_cast<GLuint>(vals[1]),
             static_cast<GLuint>(vals[2]));
 else
   glColor4ui(static_cast<GLuint>(vals[0]),
             static_cast<GLuint>(vals[1]),
             static_cast<GLuint>(vals[2]),
             static_cast<GLuint>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_s(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLshort_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3s(static_cast<GLshort>(vals[0]),
             static_cast<GLshort>(vals[1]),
             static_cast<GLshort>(vals[2]));
 else
   glColor4s(static_cast<GLshort>(vals[0]),
             static_cast<GLshort>(vals[1]),
             static_cast<GLshort>(vals[2]),
             static_cast<GLshort>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_us(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, argc, vals))
   {
     DEFAULT_RET_FINISH;
   }

 if (vals.size() == 3)
   glColor3us(static_cast<GLushort>(vals[0]),
             static_cast<GLushort>(vals[1]),
             static_cast<GLushort>(vals[2]));
 else
   glColor4us(static_cast<GLushort>(vals[0]),
             static_cast<GLushort>(vals[1]),
             static_cast<GLushort>(vals[2]),
             static_cast<GLushort>(vals[3]));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_mask(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int i = 0;
 GLboolean red = (argv[i++] == scheme_true) ? GL_TRUE : GL_FALSE;
 GLboolean green = (argv[i++] == scheme_true) ? GL_TRUE : GL_FALSE;
 GLboolean blue = (argv[i++] == scheme_true) ? GL_TRUE : GL_FALSE;
 GLboolean alpha = (argv[i++] == scheme_true) ? GL_TRUE : GL_FALSE;

 glColorMask(red, green, blue, alpha);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_index_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 double d = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[0], d))
   {
     scheme_wrong_type("gl-index-d", "double",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLdouble c = static_cast<GLdouble>(d);
 glIndexd(c);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_index_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float d = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[0], d))
   {
     scheme_wrong_type("gl-index-f", "float",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLfloat c = static_cast<GLfloat>(d);
 glIndexf(c);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_index_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-index-i", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLint c = static_cast<GLint>(d);
 glIndexi(c);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_index_s(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 short d = 0;
 if (!spark::Utils::short_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-index-s", "short",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLshort c = static_cast<GLshort>(d);
 glIndexs(c);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_index_mask(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-index-mask", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLuint c = static_cast<GLuint>(d);
 glIndexMask(c);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_logic_op(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;


 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-logic-op", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum e = static_cast<GLenum>(d);
 glLogicOp(e);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_shade_model(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-shade-model", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum e = static_cast<GLenum>(d);
 glShadeModel(e);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_color_material(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-color-material", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-color-material", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 glColorMaterial(face, mode); 
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_material_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-get-material", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-get-material", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 float* ret = 0;
 int sz = 0;
 if (mode == GL_SHININESS)
   sz = 1;
 else if (mode == GL_COLOR_INDEXES)
   sz = 3;
 else
   sz = 4;
 ret = new float[sz];

 glGetMaterialfv(face, mode, ret);

 Scheme_Object** values = new Scheme_Object*[sz];
 for (int i=0; i<sz; ++i)
   {
     Scheme_Object* obj = scheme_make_float(ret[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(sz, values);
 delete[] values;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_material_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-get-material", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-get-material", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 int* ret = 0;
 int sz = 0;
 if (mode == GL_SHININESS)
   sz = 1;
 else if (mode == GL_COLOR_INDEXES)
   sz = 3;
 else
   sz = 4;
 ret = new int[sz];

 glGetMaterialiv(face, mode, ret);

 Scheme_Object** values = new Scheme_Object*[sz];
 for (int i=0; i<sz; ++i)
   {
     Scheme_Object* obj = scheme_make_integer(ret[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(sz, values);
 delete[] values;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_material_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-material-f", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-material-f", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);

 if (!SCHEME_LISTP(argv[2]))
   {
     float r = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[2], r))
       {
	 scheme_wrong_type("gl-material-f", "list or float",
			   2, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glMaterialf(face, mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* lst = argv[2];

 int sz = scheme_list_length(lst);

 float* params = new float[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     if (spark::Utils::float_from_scheme_double(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected float.");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
   }

 glMaterialfv(face, mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_material_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-material-i", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-material-i", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);

 if (!SCHEME_LISTP(argv[2]))
   {
     int r = 0;
     if (!spark::Utils::int_from_scheme_long(argv[2], r))
       {
	 scheme_wrong_type("gl-material-i", "list or int",
			   2, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glMateriali(face, mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* lst = argv[2];
 int sz = scheme_list_length(lst);
 int* params = new int[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     if (spark::Utils::float_from_scheme_double(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected float.");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
   }

 glMaterialiv(face, mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_light_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-get-light", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-get-light", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 int sz = 0;
 switch (mode)
   {
   case GL_AMBIENT:
   case GL_DIFFUSE:
   case GL_SPECULAR:
   case GL_POSITION:
     sz = 4;
     break;
   case GL_SPOT_DIRECTION:
     sz = 3;
     break;
   default:
     sz = 1;
   }

 float* ret = new float[sz];
 glGetLightfv(face, mode, ret);
 Scheme_Object** values = new Scheme_Object*[sz];
 for (int i=0; i<sz; ++i)
   {
     Scheme_Object* obj = scheme_make_float(ret[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(sz, values);
 delete[] values;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_light_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-get-light", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-get-light", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 int sz = 0;
 switch (mode)
   {
   case GL_AMBIENT:
   case GL_DIFFUSE:
   case GL_SPECULAR:
   case GL_POSITION:
     sz = 4;
     break;
   case GL_SPOT_DIRECTION:
     sz = 3;
     break;
   default:
     sz = 1;
   }

 int* ret = new int[sz];
 glGetLightiv(face, mode, ret);
 Scheme_Object** values = new Scheme_Object*[sz];
 for (int i=0; i<sz; ++i)
   {
     Scheme_Object* obj = scheme_make_integer(ret[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(sz, values);
 delete[] values;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_light_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-light-f", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-light-f", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 if (!SCHEME_LISTP(argv[2]))
   {
     float r = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[2], r))
       {
	 scheme_wrong_type("gl-light-f", "list or float",
			   2, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glLightf(face, mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* lst = argv[2];
 
 int sz = 0;
 switch (mode)
   {
   case GL_AMBIENT:
   case GL_DIFFUSE:
   case GL_SPECULAR:
   case GL_POSITION:
     sz = 4;
     break;
   case GL_SPOT_DIRECTION:
     sz = 3;
     break;
   default:
     sz = 1;
   }

 if (scheme_list_length(lst) != sz)
   {
     scheme_signal_error("Invalid list size %d. Expected %d.",
			 scheme_list_length(lst), sz);
     DEFAULT_RET_FINISH;
   }

 float* params = new float[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     if (spark::Utils::float_from_scheme_double(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected float.");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
     if (lst == scheme_null)
       break;
     if (i >= sz)
       break;
   }

 glLightfv(face, mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_light_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-light-i", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum face = static_cast<GLenum>(d);
 d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], d))
   {
     scheme_wrong_type("gl-light-i", "int",
                       1, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 if (!SCHEME_LISTP(argv[2]))
   {
     int r = 0;
     if (!spark::Utils::int_from_scheme_long(argv[2], r))
       {
	 scheme_wrong_type("gl-light-i", "list or int",
			   2, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glLighti(face, mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
     
 Scheme_Object* lst = argv[2];
 
 int sz = 0;
 switch (mode)
   {
   case GL_AMBIENT:
   case GL_DIFFUSE:
   case GL_SPECULAR:
   case GL_POSITION:
     sz = 4;
     break;
   case GL_SPOT_DIRECTION:
     sz = 3;
     break;
   default:
     sz = 1;
   }

 if (scheme_list_length(lst) != sz)
   {
     scheme_signal_error("Invalid list size %d. Expected %d.",
			 scheme_list_length(lst), sz);
     DEFAULT_RET_FINISH;
   }

 int* params = new int[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     int r = 0;
     if (spark::Utils::int_from_scheme_long(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected integer.");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
     if (lst == scheme_null)
       break;
     if (i >= sz)
       break;
   }

 glLightiv(face, mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_light_model_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-light-model-f", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 if (!SCHEME_LISTP(argv[1]))
   {
     float r = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[1], r))
       {
	 scheme_wrong_type("gl-light-model-f", "list or float",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glLightModelf(mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* lst = argv[1];
 
 int sz = 0;
 switch (mode)
   {
   case GL_LIGHT_MODEL_AMBIENT:
     sz = 4;
     break;
   default:
     sz = 1;
   }

 if (scheme_list_length(lst) != sz)
   {
     scheme_signal_error("Invalid list size %d. Expected %d.",
			 scheme_list_length(lst), sz);
     DEFAULT_RET_FINISH;
   }

 GLfloat* params = new GLfloat[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     if (spark::Utils::float_from_scheme_double(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected float.");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
   }

 glLightModelfv(mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_light_model_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-light-model-i", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum mode = static_cast<GLenum>(d);
 
 if (!SCHEME_LISTP(argv[1]))
   {
     int r = 0;
     if (!spark::Utils::int_from_scheme_long(argv[1], r))
       {
	 scheme_wrong_type("gl-light-model-i", "list or float",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     glLightModeli(mode, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 Scheme_Object* lst = argv[1];
 
 int sz = 0;
 switch (mode)
   {
   case GL_LIGHT_MODEL_AMBIENT:
     sz = 4;
     break;
   default:
     sz = 1;
   }

 if (scheme_list_length(lst) != sz)
   {
     scheme_signal_error("Invalid list size %d. Expected %d.",
			 scheme_list_length(lst), sz);
     DEFAULT_RET_FINISH;
   }

 GLint* params = new GLint[sz];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     if (spark::Utils::float_from_scheme_double(obj, r))
       {
	 params[i++] = r;
       }
     else
       {
	 scheme_signal_error("Expected int");
	 delete[] params;
	 DEFAULT_RET_FINISH;
       }
   }

 glLightModeliv(mode, params);
 _ret_ = scheme_true;
 delete[] params;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_enable(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-enable", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum e = static_cast<GLenum>(d);
 glEnable(e);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_disable(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int d = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], d))
   {
     scheme_wrong_type("gl-disable", "int",
                       0, argc, argv);
     DEFAULT_RET_FINISH;
   }
 GLenum e = static_cast<GLenum>(d);
 glDisable(e);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_normal_3b(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLbyte_list vals;
 if (!_get_args_list(argc, argv, 0, 3, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glNormal3b(vals[0], vals[1], vals[2]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_normal_3d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list vals;
 if (!_get_args_list(argc, argv, 0, 3, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glNormal3d(vals[0], vals[1], vals[2]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_normal_3f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list vals;
 if (!_get_args_list(argc, argv, 0, 3, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glNormal3f(vals[0], vals[1], vals[2]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_normal_3i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list vals;
 if (!_get_args_list(argc, argv, 0, 3, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glNormal3i(vals[0], vals[1], vals[2]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_normal_3s(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLshort_list vals;
 if (!_get_args_list(argc, argv, 0, 3, vals))
   {
     DEFAULT_RET_FINISH;
   }

 glNormal3s(vals[0], vals[1], vals[2]);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_call_list(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int i = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], i))
   {
     scheme_wrong_type("gl-call-list", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glCallList(i);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_call_lists(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_LISTP(argv[0]))
   {
     scheme_wrong_type("gl-call-lists", "list",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 Scheme_Object* lst = argv[0];
 int len = scheme_list_length(lst);
 if (len <= 0)
   {
     DEFAULT_RET_FINISH;
   }
 int* params = new int[len];
 int i = 0;
 while (lst)
   {
     if (lst = scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     int r = 0;
     spark::Utils::int_from_scheme_long(obj, r);
     params[i++] = r;
   }
 glCallLists(static_cast<GLsizei>(len), 
	     GL_INT, 
	     reinterpret_cast<void*>(params));
 delete[] params;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_delete_lists(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int s = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], s))
   {
     scheme_wrong_type("gl-delete-lists", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 long e = 0;
 if (!spark::Utils::long_from_scheme_long(argv[1], e))
   {
     scheme_wrong_type("gl-delete-lists", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glDeleteLists(static_cast<GLuint>(s), 
	       static_cast<GLsizei>(e));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_end_list(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;
 
 glEndList();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_gen_lists(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 long i = 0;
 if (!spark::Utils::long_from_scheme_long(argv[0], i))
   {
     scheme_wrong_type("gl-gen-lists", "long",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 _ret_ = scheme_make_integer(glGenLists(i));

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_is_list(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 long i = 0;
 if (!spark::Utils::long_from_scheme_long(argv[0], i))
   {
     scheme_wrong_type("gl-is-list", "long",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLboolean b = glIsList(static_cast<GLuint>(i));
 _ret_ = (b == GL_TRUE) ? scheme_true : scheme_false;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_list_base(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 long i = 0;
 if (!spark::Utils::long_from_scheme_long(argv[0], i))
   {
     scheme_wrong_type("gl-list-base", "long",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glListBase(static_cast<GLuint>(i));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_new_list(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 long i = 0;
 if (!spark::Utils::long_from_scheme_long(argv[0], i))
   {
     scheme_wrong_type("gl-new-list", "long",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-new-list", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(m);

 glNewList(static_cast<GLuint>(i), mode);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_copy_pixels(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int x = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], x))
   {
     scheme_wrong_type("gl-copy-pixels", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int y = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], y))
   {
     scheme_wrong_type("gl-copy-pixels", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 long width = 0;
 if (!spark::Utils::long_from_scheme_long(argv[2], width))
   {
     scheme_wrong_type("gl-copy-pixels", "long",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 long height = 0;
 if (!spark::Utils::long_from_scheme_long(argv[3], height))
   {
     scheme_wrong_type("gl-copy-pixels", "long",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[4], m))
   {
     scheme_wrong_type("gl-copy-pixels", "int",
		       4, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(m);

 glCopyPixels(static_cast<GLint>(x), 
	      static_cast<GLint>(y), 
	      static_cast<GLsizei>(width), 
	      static_cast<GLsizei>(height), 
	      mode);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_draw_pixels(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 long width = 0;
 if (!spark::Utils::long_from_scheme_long(argv[0], width))
   {
     scheme_wrong_type("gl-draw-pixels", "long",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 long height = 0;
 if (!spark::Utils::long_from_scheme_long(argv[1], height))
   {
     scheme_wrong_type("gl-draw-pixels", "long",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], m))
   {
     scheme_wrong_type("gl-draw-pixels", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum format = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[3];
 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-draw-pixels", "list",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 int len = scheme_list_length(lst);    
 GLint* data = new GLint[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     int r = 0;
     spark::Utils::int_from_scheme_long(obj, r);
     data[i++] = r;
   }
 glDrawPixels(static_cast<GLsizei>(width), 
	      static_cast<GLsizei>(height), 
	      format, GL_INT,
	      reinterpret_cast<GLvoid*>(data));
 delete[] data;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_map_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-map-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum map = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[1];
 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-pixel-map-f", "list",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 int len = scheme_list_length(lst);    
 GLfloat* data = new GLfloat[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     spark::Utils::float_from_scheme_double(obj, r);
     data[i++] = r;
   }
 glPixelMapfv(map, static_cast<GLint>(len), 
	      data);
 delete[] data;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_map_ui(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-map-ui", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum map = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[1];
 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-pixel-map-ui", "list",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 int len = scheme_list_length(lst);    
 GLuint* data = new GLuint[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     unsigned int r = 0;
     spark::Utils::uint_from_scheme_long(obj, r);
     data[i++] = r;
   }
 glPixelMapuiv(map, static_cast<GLint>(len), 
	      data);
 delete[] data;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_map_us(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-map-us", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum map = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[1];
 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-pixel-map-us", "list",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 int len = scheme_list_length(lst);    
 GLushort* data = new GLushort[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     unsigned short r = 0;
     spark::Utils::ushort_from_scheme_long(obj, r);
     data[i++] = r;
   }
 glPixelMapusv(map, static_cast<GLint>(len), 
	      data);
 delete[] data;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_store_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-store-i", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum name = static_cast<GLenum>(m);

 int p = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], p))
   {
     scheme_wrong_type("gl-pixel-store-i", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glPixelStorei(name, p);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_store_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-store-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum name = static_cast<GLenum>(m);

 float p = 0;
 if (!spark::Utils::float_from_scheme_double(argv[1], p))
   {
     scheme_wrong_type("gl-pixel-store-f", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glPixelStoref(name, p);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_transfer_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-transfer-i", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum name = static_cast<GLenum>(m);

 int p = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], p))
   {
     scheme_wrong_type("gl-pixel-transfer-i", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glPixelTransferi(name, p);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_transfer_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-pixel-transfer-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum name = static_cast<GLenum>(m);

 float p = 0;
 if (!spark::Utils::float_from_scheme_double(argv[1], p))
   {
     scheme_wrong_type("gl-pixel-transfer-f", "float",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glPixelTransferf(name, p);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pixel_zoom(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float x = 0;
 if (!spark::Utils::float_from_scheme_double(argv[0], x))
   {
     scheme_wrong_type("gl-pixel-zoom", "float",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 float y = 0;
 if (!spark::Utils::float_from_scheme_double(argv[1], y))
   {
     scheme_wrong_type("gl-pixel-zoom", "float",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glPixelZoom(x, y);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_read_pixels(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int x = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], x))
   {
     scheme_wrong_type("gl-read-pixels", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int y = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], y))
   {
     scheme_wrong_type("gl-read-pixels", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 long width = 0;
 if (!spark::Utils::long_from_scheme_long(argv[2], width))
   {
     scheme_wrong_type("gl-read-pixels", "long",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 long height = 0;
 if (!spark::Utils::long_from_scheme_long(argv[3], height))
   {
     scheme_wrong_type("gl-read-pixels", "long",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int format = 0;
 if (!spark::Utils::int_from_scheme_long(argv[4], format))
   {
     scheme_wrong_type("gl-read-pixels", "int",
		       4, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int type = 0;
 if (!spark::Utils::int_from_scheme_long(argv[5], type))
   {
     scheme_wrong_type("gl-read-pixels", "int",
		       5, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int count = width * height;
 if (count <= 0)
   {
     DEFAULT_RET_FINISH;
   }
 GLint* data = new GLint[count];
 glReadPixels(static_cast<GLint>(x), static_cast<GLint>(y),
	      static_cast<GLsizei>(width), static_cast<GLsizei>(height),
	      GL_INT, static_cast<GLenum>(format),
	      reinterpret_cast<GLvoid*>(data));
 if (data)
   {
     Scheme_Object** elems = new Scheme_Object*[count];
     for (int i=0; i<count; ++i)
       {
	 Scheme_Object* obj = NULL;
	 MZ_GC_DECL_REG(1);
	 MZ_GC_VAR_IN_REG(0, obj);
	 MZ_GC_REG();
	 obj = scheme_make_integer(data[i]);
	 elems[i] = obj;
	 MZ_GC_UNREG();
       }
     _ret_ = scheme_build_list(count, elems);      
     delete[] elems;
     delete[] data;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_gen_textures(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int count = 0;
 spark::Utils::int_from_scheme_long(argv[0], count);
 GLuint* textures = new GLuint[count];
 glGenTextures(static_cast<GLint>(count), textures);

 Scheme_Object** elems = new Scheme_Object*[count];
 for (size_t i=0; i<count; ++i)
   {
     Scheme_Object* obj = NULL;
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     obj = scheme_make_integer(textures[i]);
     elems[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(count, elems);      
 delete[] elems;
 delete[] textures;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_bind_texture(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_SYMBOLP(argv[0]))
   {
     scheme_wrong_type("gl-bind-texture", "symbol",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 std::string s = SCHEME_SYM_VAL(argv[0]);
 long tex = 0;
 spark::Utils::long_from_scheme_long(argv[0], tex);
 GLint t = GL_TEXTURE_2D;
 if (s == "1d")
   t = GL_TEXTURE_1D;

 glBindTexture(t, static_cast<GLuint>(tex));

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_delete_textures(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (SCHEME_LISTP(argv[0]))
   {
     Scheme_Object* lst = argv[0];
     while (lst)
       {
	 if (lst == scheme_null)
	   break;
	 Scheme_Object* obj = SCHEME_CAR(lst);
	 lst = SCHEME_CDR(lst);
	 long r = 0;
	 if (spark::Utils::long_from_scheme_long(obj, r))
	   {
	     GLuint texture[1];
	     texture[0] = static_cast<GLuint>(r);
	     glDeleteTextures(1, texture);
	   }
       }
   }
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_coord_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 size_t len = v.size();
 switch (len)
   {
   case 1:
     glTexCoord1f(v[0]);
     break;
   case 2:
     glTexCoord2f(v[0], v[1]);
     break;
   case 3:
     glTexCoord3f(v[0], v[1], v[2]);
     break;
   case 4:
     glTexCoord4f(v[0], v[1], v[2], v[3]);
     break;
   default:
     {
       scheme_signal_error("Number of arguments should be between 1 and 4");
       DEFAULT_RET_FINISH;
     }
   }
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_coord_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 size_t len = v.size();
 switch (len)
   {
   case 1:
     glTexCoord1d(v[0]);
     break;
   case 2:
     glTexCoord2d(v[0], v[1]);
     break;
   case 3:
     glTexCoord3d(v[0], v[1], v[2]);
     break;
   case 4:
     glTexCoord4d(v[0], v[1], v[2], v[3]);
     break;
   default:
     {
       scheme_signal_error("Number of arguments should be between 1 and 4");
       DEFAULT_RET_FINISH;
     }
   }
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_coord_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 size_t len = v.size();
 switch (len)
   {
   case 1:
     glTexCoord1i(v[0]);
     break;
   case 2:
     glTexCoord2i(v[0], v[1]);
     break;
   case 3:
     glTexCoord3i(v[0], v[1], v[2]);
     break;
   case 4:
     glTexCoord4i(v[0], v[1], v[2], v[3]);
     break;
   default:
     {
       scheme_signal_error("Number of arguments should be between 1 and 4");
       DEFAULT_RET_FINISH;
     }
   }
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_coord_s(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLshort_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 size_t len = v.size();
 switch (len)
   {
   case 1:
     glTexCoord1s(v[0]);
     break;
   case 2:
     glTexCoord2s(v[0], v[1]);
     break;
   case 3:
     glTexCoord3s(v[0], v[1], v[2]);
     break;
   case 4:
     glTexCoord4s(v[0], v[1], v[2], v[3]);
     break;
   default:
     {
       scheme_signal_error("Number of arguments should be between 1 and 4");
       DEFAULT_RET_FINISH;
     }
   }
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_env_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-tex-env-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-env-f", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 float param = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[2], param))
   {
     scheme_wrong_type("gl-tex-env-f", "float",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexEnvf(target, pname, static_cast<GLfloat>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_env_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-tex-env-i", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-env-i", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 int param = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], param))
   {
     scheme_wrong_type("gl-tex-env-i", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexEnvi(target, pname, static_cast<GLint>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_gen_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-tex-gen-d", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum coord = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-gen-d", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 double param = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[2], param))
   {
     scheme_wrong_type("gl-tex-gen-d", "double",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexGend(coord, pname, static_cast<GLdouble>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_gen_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-tex-gen-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum coord = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-gen-f", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 float param = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[2], param))
   {
     scheme_wrong_type("gl-tex-gen-f", "float",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexGenf(coord, pname, static_cast<GLfloat>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_gen_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-tex-gen-i", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum coord = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-gen-i", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 int param = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], param))
   {
     scheme_wrong_type("gl-tex-gen-i", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexGeni(coord, pname, static_cast<GLint>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_image(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int j = 0;
 if (!SCHEME_SYMBOLP(argv[j]))
   {
     scheme_wrong_type("gl-tex-image", "symbol",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 std::string s = SCHEME_SYM_VAL(argv[j]);
 ++j;
 int level = 0;
 if (!spark::Utils::int_from_scheme_long(argv[j], level))
   {
     scheme_wrong_type("gl-tex-image", "int",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 ++j;
 int components = 0;
 if (!spark::Utils::int_from_scheme_long(argv[j], components))
   {
     scheme_wrong_type("gl-tex-image", "int",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 ++j;
 long width = 0;
 if (!spark::Utils::long_from_scheme_long(argv[j], width))
   {
     scheme_wrong_type("gl-tex-image", "long",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 ++j;
 long height = 0;
 if (s == "2d")
   {
      if (!spark::Utils::long_from_scheme_long(argv[j], height))
	{
	  scheme_wrong_type("gl-tex-image", "long",
			    j, argc,
			    argv);
	  DEFAULT_RET_FINISH;
	}
      ++j;
   }

 int border = 0;
 if (!spark::Utils::int_from_scheme_long(argv[j], border))
   {
     scheme_wrong_type("gl-tex-image", "int",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 ++j;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[j], m))
   {
     scheme_wrong_type("gl-tex-image", "int",
		       j, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 ++j;

 unsigned char* data = 0;
 bool del_data = false;
 Texture_image* ti = 0;

 if (SCHEME_LISTP(argv[j]))
   {
     Scheme_Object* lst = argv[j];
     int len = scheme_list_length(lst);
     data = new unsigned char[len];
     del_data = true;
     int i = 0;
     while (lst)
       {
	 if (lst == scheme_null)
	   break;
	 Scheme_Object* obj = SCHEME_CAR(lst);
	 lst = SCHEME_CDR(lst);
	 int r = 0;
	 spark::Utils::int_from_scheme_long(obj, r);
	 data[i++] = static_cast<unsigned char>(r);
       }
   }
 else if (SCHEME_CHAR_STRINGP(argv[j]))
   {
     Scheme_Object* str = scheme_char_string_to_byte_string(argv[j]);
     std::string file_name = SCHEME_BYTE_STR_VAL(str);
     std::string ext;
     spark::Utils::get_file_extn(file_name, ext);
     ti = new Texture_image;
     int r = 0;
     if (ext == "bmp")
       r = load_bmp(file_name.c_str(), ti);
     else if (ext == "raw" || ext.length() == 0)
       r = load_raw(file_name.c_str(), width, height, ti);
     else
       {
	 delete ti;
	 _ret_ = scheme_make_symbol("unsupported-image-type");
	 DEFAULT_RET_FINISH;
       }
     if (r != 0)
       {
	 delete ti;
	 switch (r)
	   {
	   case 1:
	     {
	       _ret_ = scheme_make_symbol("file-open-error");
	       DEFAULT_RET_FINISH;
	       break;
	     }
	   case 2:
	     {
	       _ret_ = scheme_make_symbol("image-creation-error");
	       DEFAULT_RET_FINISH;
	       break;
	     }
	   default:
	     {
	       _ret_ = scheme_make_symbol("unknown-error");
	       DEFAULT_RET_FINISH;
	     }
	   }
       }
     if (ti->type == Texture_image::BMP)
       {
	 width = ti->width;
	 height = ti->height;
       }
     data = ti->data;
   }
 if (s == "1d")
   {
     glTexImage1D(GL_TEXTURE_1D, level,
		  components, width,
		  border, static_cast<GLenum>(m),
		  GL_UNSIGNED_BYTE, data);
   }
 else
   {
     glTexImage2D(GL_TEXTURE_2D, level,
		  components, width, height,
		  border, static_cast<GLenum>(m),
		  GL_UNSIGNED_BYTE, data);
   }

 if (del_data)
   delete[] data;

 delete ti;

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_parameter_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_SYMBOLP(argv[0]))
 {
   scheme_wrong_type("gl-tex-parameter-f", "symbol",
		     0, argc,
		     argv);
   DEFAULT_RET_FINISH;
 }
 std::string s = SCHEME_SYM_VAL(argv[0]);
 GLenum target = GL_TEXTURE_2D;
 if (s == "1d")
   target = GL_TEXTURE_1D;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-parameter-f", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 float param = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[2], param))
   {
     scheme_wrong_type("gl-tex-parameter-f", "float",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexParameterf(target, pname, static_cast<GLfloat>(param));
 _ret_ = scheme_true;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_tex_parameter_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 if (!SCHEME_SYMBOLP(argv[0]))
 {
   scheme_wrong_type("gl-tex-parameter-i", "symbol",
		     0, argc,
		     argv);
   DEFAULT_RET_FINISH;
 }
 std::string s = SCHEME_SYM_VAL(argv[0]);
 GLenum target = GL_TEXTURE_2D;
 if (s == "1d")
   target = GL_TEXTURE_1D;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-tex-parameter-i", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 int param = 0;
 if (!spark::Utils::int_from_scheme_long(argv[2], param))
   {
     scheme_wrong_type("gl-tex-parameter-i", "int",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glTexParameteri(target, pname, static_cast<GLint>(param));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_blend_func(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-blend-func", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum sfactor = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-blend-func", "int",
		       1, argc, argv);
     DEFAULT_RET_FINISH;
   }

 GLenum dfactor = static_cast<GLenum>(m);

 glBlendFunc(sfactor, dfactor);
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_fog_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-fog-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[1];
 if (!SCHEME_LISTP(lst))
   {
     float r = 0.0f;
     if (!spark::Utils::float_from_scheme_double(lst, r))
       {
	 scheme_wrong_type("gl-fog-f", "list or float",
			   1, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }
     glFogf(pname, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
	
 int len = scheme_list_length(lst);
 GLfloat* params = new GLfloat[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float r = 0.0f;
     spark::Utils::float_from_scheme_double(obj, r);
     params[i++] = r;
   }
 glFogfv(pname, params);
 delete[] params;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_fog_i(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-fog-i", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum pname = static_cast<GLenum>(m);

 Scheme_Object* lst = argv[1];
 if (!SCHEME_LISTP(lst))
   {
     int r = 0;
     if (!spark::Utils::int_from_scheme_long(lst, r))
       {
	 scheme_wrong_type("gl-fog-i", "list or int",
			   1, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }
     glFogi(pname, r);
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
	
 int len = scheme_list_length(lst);
 GLint* params = new GLint[len];
 int i = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     int r = 0;
     spark::Utils::int_from_scheme_long(obj, r);
     params[i++] = static_cast<GLint>(r);
   }
 glFogiv(pname, params);
 delete[] params;
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_eval_coord_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 switch (v.size())
   {
   case 1:
     glEvalCoord1d(v[0]);
     break;
   case 2:
     glEvalCoord2d(v[0], v[1]);
     break;
   default:
     {
       scheme_signal_error("Expects either 1 or 2 arguments");
       DEFAULT_RET_FINISH;
     }
   }

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_eval_coord_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLfloat_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 switch (v.size())
   {
   case 1:
     glEvalCoord1f(v[0]);
     break;
   case 2:
     glEvalCoord2f(v[0], v[1]);
     break;
   default:
     scheme_signal_error("Expects either 1 or 2 arguments");
   }

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_eval_mesh(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-eval-mesh", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum mode = static_cast<GLenum>(m);
 
 GLint_list v;
 if (!_get_args_list(argc, argv, 1, (argc - 1), v))
   {
     DEFAULT_RET_FINISH;
   }
 
 switch (v.size())
   {
   case 2:
     glEvalMesh1(mode, v[0], v[1]);
     break;
   case 4:
     glEvalMesh2(mode, v[0], v[1], v[2], v[3]);
     break;
   default:
     scheme_signal_error("Expects either 3 or 5 arguments");
   }

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_eval_point(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLint_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 switch (v.size())
   {
   case 1:
     glEvalPoint1(v[0]);
     break;
   case 2:
     glEvalPoint2(v[0], v[1]);
     break;
   default:
     scheme_signal_error("Expects either 1 or 2 arguments");
   }

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_map(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-get-map", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(m);

 m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], m))
   {
     scheme_wrong_type("gl-get-map", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum query = static_cast<GLenum>(m);

 if (!SCHEME_SYMBOLP(argv[2]))
   {
     scheme_wrong_type("gl-get-map", "symbol",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 std::string s = SCHEME_SYM_VAL(argv[2]);

 int sz = 4;
 scheme_signal_error("gl-get-map: set size correctly");

 if (s == "d")
   {
     GLdouble* v = new GLdouble[sz]; 
     glGetMapdv(target, query, v);
     Scheme_Object** values = new Scheme_Object*[sz];
     for (int i=0; i<sz; ++i)
       {
	 Scheme_Object* obj = scheme_make_double(static_cast<double>(v[i]));
	 MZ_GC_DECL_REG(1);
	 MZ_GC_VAR_IN_REG(0, obj);
	 MZ_GC_REG();
	 values[i] = obj;
	 MZ_GC_UNREG();
       }
     _ret_ = scheme_build_list(sz, values);
     delete[] v;
     delete[] values;
   }
 else if (s == "f")
   {
     GLfloat* v = new GLfloat[sz]; 
     glGetMapfv(target, query, v);
     Scheme_Object** values = new Scheme_Object*[sz];
     for (int i=0; i<sz; ++i)
       {
	 Scheme_Object* obj = scheme_make_float(static_cast<float>(v[i]));
	 MZ_GC_DECL_REG(1);
	 MZ_GC_VAR_IN_REG(0, obj);
	 MZ_GC_REG();
	 values[i] = obj;
	 MZ_GC_UNREG();
       }
     _ret_ = scheme_build_list(sz, values);
     delete[] v;
     delete[] values;
   }
 else if (s == "i")
   {
     GLint* v = new GLint[sz]; 
     glGetMapiv(target, query, v);
     Scheme_Object** values = new Scheme_Object*[sz];
     for (int i=0; i<sz; ++i)
       {
	 Scheme_Object* obj = scheme_make_integer(static_cast<int>(v[i]));
	 MZ_GC_DECL_REG(1);
	 MZ_GC_VAR_IN_REG(0, obj);
	 MZ_GC_REG();
	 values[i] = obj;
	 MZ_GC_UNREG();
       }
     _ret_ = scheme_build_list(sz, values);
     delete[] v;
     delete[] values;
   }
 else
   scheme_signal_error("gl-get-map: Invalid return type");

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_map_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-map-d", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(m);

 double u1 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[1], u1))
   {
     scheme_wrong_type("gl-map-d", "double",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 double u2 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[2], u2))
   {
     scheme_wrong_type("gl-map-d", "double",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i1 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[3], i1))
   {
     scheme_wrong_type("gl-map-d", "int",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i2 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[4], i2))
   {
     scheme_wrong_type("gl-map-d", "int",
		       4, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 
 Scheme_Object* lst = 0;
 int a = 0;
 if (argc == 6)
   {
     lst = argv[5];
     a = 5;
   }
 else if (argc == 10)
   {
     lst = argv[9];
     a = 9;
   }
 else
   scheme_signal_error("gl-map-d", "Invalid number of arguments");

 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-map-d", "list",
		       a, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int len = scheme_list_length(lst);
 GLdouble* points = new GLdouble[len];
 a = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     double d = 0.0f;
     spark::Utils::double_from_scheme_double(obj, d);
     points[a++] = d;
   }

 if (argc == 6)
   {
     glMap1d(target, static_cast<GLdouble>(u1), 
	     static_cast<GLdouble>(u2), static_cast<GLint>(i1), 
	     static_cast<GLint>(i2), points);
     delete[] points;
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }

 double u3 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[5], u3))
   {
     scheme_wrong_type("gl-map-d", "double",
		       5, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 double u4 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[6], u4))
   {
     scheme_wrong_type("gl-map-d", "double",
		       6, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i3 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[7], i3))
   {
     scheme_wrong_type("gl-map-d", "int",
		       7, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i4 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[8], i4))
   {
     scheme_wrong_type("gl-map-d", "int",
		       8, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glMap2d(target, static_cast<GLdouble>(u1),
	 static_cast<GLdouble>(u2), static_cast<GLint>(i1),
	 static_cast<GLint>(i2), static_cast<GLdouble>(u3),
	 static_cast<GLdouble>(u4), static_cast<GLint>(i3),
	 static_cast<GLint>(i4), points);
 delete[] points;
 _ret_ = scheme_true;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_map_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int m = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], m))
   {
     scheme_wrong_type("gl-map-f", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLenum target = static_cast<GLenum>(m);

 float u1 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[1], u1))
   {
     scheme_wrong_type("gl-map-f", "float",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 float u2 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[2], u2))
   {
     scheme_wrong_type("gl-map-f", "float",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i1 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[3], i1))
   {
     scheme_wrong_type("gl-map-f", "int",
		       3, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i2 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[4], i2))
   {
     scheme_wrong_type("gl-map-f", "int",
		       4, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 
 Scheme_Object* lst = 0;
 int a = 0;
 if (argc == 6)
   {
     lst = argv[5];
     a = 5;
   }
 else if (argc == 10)
   {
     lst = argv[9];
     a = 9;
   }
 else
   scheme_signal_error("gl-map-f", "Invalid number of arguments");

 if (!SCHEME_LISTP(lst))
   {
     scheme_wrong_type("gl-map-f", "list",
		       a, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int len = scheme_list_length(lst);
 GLfloat* points = new GLfloat[len];
 a = 0;
 while (lst)
   {
     if (lst == scheme_null)
       break;
     Scheme_Object* obj = SCHEME_CAR(lst);
     lst = SCHEME_CDR(lst);
     float d = 0.0f;
     spark::Utils::float_from_scheme_double(obj, d);
     points[a++] = d;
   }

 if (argc == 6)
   {
     glMap1f(target, static_cast<GLfloat>(u1), 
	     static_cast<GLfloat>(u2), static_cast<GLint>(i1), 
	     static_cast<GLint>(i2), points);
     delete[] points;
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }

 float u3 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[5], u3))
   {
     scheme_wrong_type("gl-map-f", "float",
		       5, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 float u4 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[6], u4))
   {
     scheme_wrong_type("gl-map-f", "float",
		       6, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i3 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[7], i3))
   {
     scheme_wrong_type("gl-map-f", "int",
		       7, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 int i4 = 0;
 if (!spark::Utils::int_from_scheme_long(argv[8], i4))
   {
     scheme_wrong_type("gl-map-f", "int",
		       8, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 glMap2f(target, static_cast<GLfloat>(u1),
	 static_cast<GLfloat>(u2), static_cast<GLint>(i1),
	 static_cast<GLint>(i2), static_cast<GLfloat>(u3),
	 static_cast<GLfloat>(u4), static_cast<GLint>(i3),
	 static_cast<GLint>(i4), points);
 delete[] points;
 _ret_ = scheme_true;
 
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_map_grid_d(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int un = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], un))
   {
     scheme_wrong_type("gl-map-d", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 double u1 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[1], u1))
   {
     scheme_wrong_type("gl-map-grid-d", "double",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 double u2 = 0.0f;
 if (!spark::Utils::double_from_scheme_double(argv[2], u2))
   {
     scheme_wrong_type("gl-map-grid-d", "double",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 if (argc == 3)
   {
     glMapGrid1d(static_cast<GLint>(un), 
		 static_cast<GLdouble>(u1), static_cast<GLdouble>(u2));
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 else if (argc == 6)
   {
     int vn = 0;
     if (!spark::Utils::int_from_scheme_long(argv[3], un))
       {
	 scheme_wrong_type("gl-map-grid-d", "int",
			   3, argc,
		       argv);
	 DEFAULT_RET_FINISH;
       }

     double v1 = 0.0f;
     if (!spark::Utils::double_from_scheme_double(argv[4], u1))
       {
	 scheme_wrong_type("gl-map-grid-d", "double",
			   4, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }

     double v2 = 0.0f;
     if (!spark::Utils::double_from_scheme_double(argv[5], u2))
       {
	 scheme_wrong_type("gl-map-grid-d", "double",
			   5, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }
     glMapGrid2d(static_cast<GLint>(un), 
		 static_cast<GLdouble>(u1), static_cast<GLdouble>(u2),
		 static_cast<GLint>(vn), 
		 static_cast<GLdouble>(v1), static_cast<GLdouble>(v2));
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 else
   {
     scheme_signal_error("gl-map-grid-d: Invalid number of arguments");
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_map_grid_f(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int un = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], un))
   {
     scheme_wrong_type("gl-map-d", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 float u1 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[1], u1))
   {
     scheme_wrong_type("gl-map-grid-f", "float",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 float u2 = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[2], u2))
   {
     scheme_wrong_type("gl-map-grid-f", "double",
		       2, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 if (argc == 3)
   {
     glMapGrid1f(static_cast<GLint>(un), 
		 static_cast<GLfloat>(u1), static_cast<GLfloat>(u2));
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 else if (argc == 6)
   {
     int vn = 0;
     if (!spark::Utils::int_from_scheme_long(argv[3], un))
       {
	 scheme_wrong_type("gl-map-d", "int",
			   3, argc,
		       argv);
	 DEFAULT_RET_FINISH;
       }

     float v1 = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[4], u1))
       {
	 scheme_wrong_type("gl-map-grid-f", "float",
			   4, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }

     float v2 = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[5], u2))
       {
	 scheme_wrong_type("gl-map-grid-f", "float",
			   5, argc,
			   argv);
	 DEFAULT_RET_FINISH;
       }
     glMapGrid2f(static_cast<GLint>(un), 
		 static_cast<GLfloat>(u1), static_cast<GLfloat>(u2),
		 static_cast<GLint>(vn), 
		 static_cast<GLfloat>(v1), static_cast<GLfloat>(v2));
     _ret_ = scheme_true;
     DEFAULT_RET_FINISH;
   }
 else
   {
     scheme_signal_error("gl-map-grid-d: Invalid number of arguments");
   }

 DEFAULT_RET_FINISH;
}

struct Feedback_buffer
{
  GLfloat* buffer;
  int size;

  Feedback_buffer() : buffer(0), size(0) { }
  ~Feedback_buffer()
  {
    if (buffer)
      delete[] buffer;
  }
};

static std::vector<Feedback_buffer*> _feedback_buffers;

Scheme_Object*
spark_opengl::gl_feedback_buffer(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int size = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], size))
   {
     scheme_wrong_type("gl-feedback-buffer", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 if (size <= 0)
   {
     DEFAULT_RET_FINISH;
   }

 int type = 0;
 if (!spark::Utils::int_from_scheme_long(argv[1], type))
   {
     scheme_wrong_type("gl-feedback-buffer", "int",
		       1, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 GLfloat* buffer = new GLfloat[size];
 glFeedbackBuffer(size, static_cast<GLenum>(type), buffer);
 Feedback_buffer* b = new Feedback_buffer;
 b->buffer = buffer;
 b->size = size;
 _feedback_buffers.push_back(b);

 _ret_ = scheme_make_integer(_feedback_buffers.size());
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_feedback_buffer(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int index = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], index))
   {
     scheme_wrong_type("gl-get-feedback-buffer", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 if (index >= _feedback_buffers.size())
   {
     DEFAULT_RET_FINISH;
   }
 Feedback_buffer* b = _feedback_buffers[index];
 if (!b || !b->buffer || b->size <= 0)
   {
     DEFAULT_RET_FINISH;
   }
 
 Scheme_Object** values = new Scheme_Object*[b->size];
 for (int i=0; i<b->size; ++i)
   {
     Scheme_Object* obj = scheme_make_float(b->buffer[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(b->size, values);
 delete[] values;

 if (argc == 2)
   {
     if (argv[1] == scheme_true)
       {
	 delete b;
	 _feedback_buffers[index] = 0;
       }
   }
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_init_names(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glInitNames();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pop_name(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 glPopName();
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_push_name(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int n = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], n))
   {
     scheme_wrong_type("gl-push-name", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glPushName(static_cast<GLint>(n));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_load_name(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int n = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], n))
   {
     scheme_wrong_type("gl-load-name", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glLoadName(static_cast<GLint>(n));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_pass_through(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 float n = 0.0f;
 if (!spark::Utils::float_from_scheme_double(argv[0], n))
   {
     scheme_wrong_type("gl-pass-through", "float",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glPassThrough(static_cast<GLfloat>(n));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_render_mode(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int n = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], n))
   {
     scheme_wrong_type("gl-render-mode", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 glRenderMode(static_cast<GLenum>(n));
 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_lookat(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLdouble_list v;
 if (!_get_args_list(argc, argv, 0, argc, v))
   {
     DEFAULT_RET_FINISH;
   }
 
 gluLookAt(v[0], v[1], v[2], v[3],
	   v[4], v[5], v[6], v[7],
	   v[8]);

 _ret_ = scheme_true;

 DEFAULT_RET_FINISH;
}

static Scheme_Object* _quadric_obj_to_scheme_object(GLUquadricObj* qobj);
static GLUquadricObj* _scheme_object_to_quadric_obj(Scheme_Object* obj);

Scheme_Object*
spark_opengl::glu_newquadric(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = gluNewQuadric();
 if (qobj)
   _ret_ = _quadric_obj_to_scheme_object(qobj);

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_deletequadric(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     gluDeleteQuadric(qobj);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_quadric_drawstyle(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     int i = 0;
     if (!spark::Utils::int_from_scheme_long(argv[1], i))
       {
	 scheme_wrong_type("glu-quadric-drawstyle", "int",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     gluQuadricDrawStyle(qobj, static_cast<GLenum>(i));
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_quadric_normals(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     int i = 0;
     if (!spark::Utils::int_from_scheme_long(argv[1], i))
       {
	 scheme_wrong_type("glu-quadric-normals", "int",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     gluQuadricNormals(qobj, static_cast<GLenum>(i));
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_quadric_orientation(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     int i = 0;
     if (!spark::Utils::int_from_scheme_long(argv[1], i))
       {
	 scheme_wrong_type("glu-quadric-orientation", "int",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     gluQuadricOrientation(qobj, static_cast<GLenum>(i));
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_quadric_texture(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     int i = 0;
     if (!spark::Utils::int_from_scheme_long(argv[1], i))
       {
	 scheme_wrong_type("glu-quadric-texture", "int",
			   1, argc, argv);
	 DEFAULT_RET_FINISH;
       }
     gluQuadricTexture(qobj, static_cast<GLenum>(i));
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_cylinder(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     GLdouble_list dv;
     if (!_get_args_list(argc, argv, 1, 3, dv))
       {
	 DEFAULT_RET_FINISH;
       }
     GLint_list di;
     if (!_get_args_list(argc, argv, 4, 2, di))
       {
	 DEFAULT_RET_FINISH;
       }
     gluCylinder(qobj, dv[0], dv[1], dv[2],
		 di[0], di[1]);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_disk(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     GLdouble_list dv;
     if (!_get_args_list(argc, argv, 1, 2, dv))
       {
	 DEFAULT_RET_FINISH;
       }
     GLint_list di;
     if (!_get_args_list(argc, argv, 3, 2, di))
       {
	 DEFAULT_RET_FINISH;
       }
     gluDisk(qobj, dv[0], dv[1],
	     di[0], di[1]);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_partial_disk(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     GLdouble_list dv01;
     if (!_get_args_list(argc, argv, 1, 2, dv01))
       {
	 DEFAULT_RET_FINISH;
       }
     GLint_list di;
     if (!_get_args_list(argc, argv, 3, 2, di))
       {
	 DEFAULT_RET_FINISH;
       }
     GLdouble_list dv02;
     if (!_get_args_list(argc, argv, 5, 2, dv02))
       {
	 DEFAULT_RET_FINISH;
       }
     gluPartialDisk(qobj, dv01[0], dv01[1],
			di[0], di[1],
			dv02[0], dv02[1]);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::glu_sphere(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 GLUquadricObj* qobj = _scheme_object_to_quadric_obj(argv[0]);
 if (qobj)
   {
     GLdouble_list dv01;
     if (!_get_args_list(argc, argv, 1, 1, dv01))
       {
	 DEFAULT_RET_FINISH;
       }
     GLint_list di;
     if (!_get_args_list(argc, argv, 2, 2, di))
       {
	 DEFAULT_RET_FINISH;
       }
     gluSphere(qobj, dv01[0], di[0], di[1]);
     _ret_ = scheme_true;
   }

 DEFAULT_RET_FINISH;
}

struct Select_buffer
{
  GLuint* buffer;
  int size;

  Select_buffer() : buffer(0), size(0) { }
  ~Select_buffer()
  {
    if (buffer)
      delete[] buffer;
  }
};

static std::vector<Select_buffer*> _select_buffers;

Scheme_Object*
spark_opengl::gl_select_buffer(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int size = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], size))
   {
     scheme_wrong_type("gl-select-buffer", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }

 if (size <= 0)
   {
     DEFAULT_RET_FINISH;
   }

 GLuint* buffer = new GLuint[size];
 glSelectBuffer(size, buffer);
 Select_buffer* b = new Select_buffer;
 b->buffer = buffer;
 b->size = size;
 _select_buffers.push_back(b);

 _ret_ = scheme_make_integer(_select_buffers.size());
 DEFAULT_RET_FINISH;
}

Scheme_Object*
spark_opengl::gl_get_select_buffer(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

 int index = 0;
 if (!spark::Utils::int_from_scheme_long(argv[0], index))
   {
     scheme_wrong_type("gl-get-select-buffer", "int",
		       0, argc,
		       argv);
     DEFAULT_RET_FINISH;
   }
 --index;
 if (index >= _select_buffers.size())
   {
     DEFAULT_RET_FINISH;
   }
 Select_buffer* b = _select_buffers[index];
 if (!b || !b->buffer || b->size <= 0)
   {
     DEFAULT_RET_FINISH;
   }
 
 Scheme_Object** values = new Scheme_Object*[b->size];
 for (int i=0; i<b->size; ++i)
   {
     Scheme_Object* obj = scheme_make_integer(b->buffer[i]);
     MZ_GC_DECL_REG(1);
     MZ_GC_VAR_IN_REG(0, obj);
     MZ_GC_REG();
     values[i] = obj;
     MZ_GC_UNREG();
   }
 _ret_ = scheme_build_list(b->size, values);
 delete[] values;

 if (argc == 2)
   {
     if (argv[1] == scheme_true)
       {
	 delete b;
	 _select_buffers[index] = 0;
       }
   }
 DEFAULT_RET_FINISH;
}

// End of exported opengl functions
// :~

// local utilities

bool
_get_args_list(int argc, Scheme_Object** argv,
              int offset, int count,
              GLdouble_list& ret)
{
  int i = offset;
  int c = 0;
  while (c < count)
   {
     double v = 0.0f;
     if (!spark::Utils::double_from_scheme_double(argv[i], v))
       {
         scheme_wrong_type("_get_args_list", "double",
                           i, argc,
                           argv);
         return false;
       }
     ret.push_back(v);
     ++i;
     ++c;
   }
 return true;
}

bool
_get_args_list(int argc, Scheme_Object** argv,
              int offset, int count,
              GLfloat_list& ret)
{
  int i = offset;
  int c = 0;
  while (c < count)
   {
     float v = 0.0f;
     if (!spark::Utils::float_from_scheme_double(argv[i], v))
       {
         scheme_wrong_type("_get_args_list", "float",
                           i, argc,
                           argv);
         return false;
       }
     ret.push_back(v);
     ++i;
     ++c;
   }
 return true;
}

bool
_get_args_list(int argc, Scheme_Object** argv,
              int offset, int count,
              GLint_list& ret)
{
  int i = offset;
  int c = 0;
  while (c < count)
   {
     long v = 0;
     if (!spark::Utils::long_from_scheme_long(argv[i], v))
       {
         scheme_wrong_type("_get_args_list", "long",
                           i, argc,
                           argv);
         return false;
       }
     ret.push_back(v);
     ++i;
     ++c;
   }
 return true;
}

bool
_get_args_list(int argc, Scheme_Object** argv,
              int offset, int count,
              GLshort_list& ret)
{
  int i = offset;
  int c = 0;
  while (c < count)
   {
     short v = 0;
     if (!spark::Utils::short_from_scheme_long(argv[i], v))
       {
         scheme_wrong_type("_get_args_list", "short",
                           i, argc,
                           argv);
         return false;
       }
     ret.push_back(v);
     ++i;
     ++c;
   }
 return true;
}

bool
_get_args_list(int argc, Scheme_Object** argv,
	       int offset, int count,
	       GLbyte_list& ret)
{
  int i = offset;
  int c = 0;
  while (c < count)
    {
      char v = 0;
      if (!spark::Utils::char_from_scheme_long(argv[i], v))
	{
	  scheme_wrong_type("_get_args_list", "byte",
			    i, argc,
			    argv);
	  return false;
	}
      ret.push_back(v);
      ++i;
      ++c;
    }
  return true;
}

bool
_glint_from_scheme_long(Scheme_Object* obj, GLint& ret)
{
 long t = 0;
 if (spark::Utils::long_from_scheme_long(obj, t))
   {
     ret = static_cast<GLint>(t);
     return true;
   }
 return false;
}
 
bool
_glsizei_from_scheme_long(Scheme_Object* obj, GLsizei& ret)
{
 long t = 0;
 if (spark::Utils::long_from_scheme_long(obj, t))
   {
     ret = static_cast<GLsizei>(t);
     return true;
   }
 return false;
}

static const int QUADRIC_OBJ_TAG = 20;

Scheme_Object* 
_quadric_obj_to_scheme_object(GLUquadricObj* qobj)
{
  Scheme_Object* tag = scheme_make_integer(QUADRIC_OBJ_TAG);
  return scheme_make_cptr(reinterpret_cast<void*>(qobj), tag);
}

GLUquadricObj* 
_scheme_object_to_quadric_obj(Scheme_Object* obj)
{
  if (!obj)
    return 0;
  if (!SCHEME_CPTRP(obj))
    {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT, "Not a c-pointer.");
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(obj);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_raise_exn(MZEXN_FAIL, "Invalid type for GLUquadricObj tag.");
      return 0;
    }  
  if (i != QUADRIC_OBJ_TAG)
    {
      scheme_raise_exn(MZEXN_FAIL, "Invalid GLUquadricObj tag.");
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(obj);
  if (!p)
    {
      scheme_raise_exn(MZEXN_FAIL, "Failed to get GLUquadricObj pointer.");
      return 0;
    }
  return reinterpret_cast<GLUquadricObj*>(p);
}

// End of local utility functions
// :~
