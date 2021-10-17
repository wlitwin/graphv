#define CAML_NAME_SPACE
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include "gl2.h"

CAMLprim value gles2_cull_face(value v1)
{
    CAMLparam1 (v1);

    glCullFace(Int_val(v1));

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_clear_color(value v1, value v2, value v3, value v4)
{
    CAMLparam4 (v1, v2, v3, v4);

    glClearColor(Double_val(v1), Double_val(v2), Double_val(v3), Double_val(v4));

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_uniform4fv(value v1, value v2)
{
    CAMLparam2 (v1, v2);

    GLsizei size = Caml_ba_array_val(v2)->dim[0] / 4;
    float* data = (float*)Caml_ba_data_val(v2);
    glUniform4fv(Int_val(v1), size, data);

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_uniform2fv(value v1, value v2)
{
    CAMLparam2 (v1, v2);

    GLsizei size = Caml_ba_array_val(v2)->dim[0] / 2;
    float* data = (float*)Caml_ba_data_val(v2);
    glUniform2fv(Int_val(v1), size, data);

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_uniform1i(value v1, value v2)
{
    CAMLparam2 (v1, v2);
    glUniform1i(Int_val(v1), Int_val(v2));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_buffer_data(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);

    GLsizei size = Caml_ba_array_val(v2)->dim[0] * 4;
    float* data = (float*)Caml_ba_data_val(v2);
    glBufferData(Int_val(v1), size, data, Int_val(v3));

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_enable(value v1)
{
    CAMLparam1 (v1);
    glEnable(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_disable(value v1)
{
    CAMLparam1 (v1);
    glDisable(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_finish()
{
    CAMLparam0();
    glFinish();
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_draw_arrays(value mode, value offset, value count)
{
    CAMLparam3 (mode, offset, count);
    glDrawArrays(Int_val(mode), Int_val(offset), Int_val(count));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_color_mask(value r, value g, value b, value a)
{
    CAMLparam4 (r, g, b, a);
    glColorMask(Bool_val(r), Bool_val(g), Bool_val(b), Bool_val(a));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_front_face(value dir)
{
    CAMLparam1 (dir);
    glFrontFace(Int_val(dir));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_active_texture(value v1)
{
    CAMLparam1 (v1);
    glActiveTexture(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_stencil_mask(value v1)
{
    CAMLparam1 (v1);
    glStencilMask(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_stencil_func(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);
    glStencilFunc(Int_val(v1), Int_val(v2), Int_val(v3));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_stencil_op(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);
    glStencilOp(Int_val(v1), Int_val(v2), Int_val(v3));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_stencil_op_separate(value v1, value v2, value v3, value v4)
{
    CAMLparam4 (v1, v2, v3, v4);
    glStencilOpSeparate(Int_val(v1), Int_val(v2), Int_val(v3), Int_val(v4));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_blend_func_separate(value v1, value v2, value v3, value v4)
{
    CAMLparam4 (v1, v2, v3, v4);
    glBlendFuncSeparate(Int_val(v1), Int_val(v2), Int_val(v3), Int_val(v4));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_pixel_storei(value v1, value v2)
{
    CAMLparam1 (v1);
    glPixelStorei(Int_val(v1), Int_val(v2));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_enable_vaa(value v1)
{
    CAMLparam1 (v1);
    glEnableVertexAttribArray(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_disable_vaa(value v1)
{
    CAMLparam1 (v1);
    glDisableVertexAttribArray(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_bind_buffer(value v1, value v2)
{
    CAMLparam2 (v1, v2);
    glBindBuffer(Int_val(v1), Int_val(v2));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_use_program(value v1)
{
    CAMLparam1 (v1);
    glUseProgram(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_tex_parameteri(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);
    glTexParameteri(Int_val(v1), Int_val(v2), Int_val(v3));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_generate_mipmap(value v1)
{
    CAMLparam1 (v1);
    glGenerateMipmap(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_uniform_loc(value v1, value v2)
{
    CAMLparam2 (v1, v2);
    char* name = caml_stat_strdup(String_val(v2));
    int v = glGetUniformLocation(Int_val(v1), name);
    caml_stat_free(name);
    CAMLreturn (Val_int(v));
}

CAMLprim value gles2_create_program()
{
    CAMLparam0 ();
    GLuint v = glCreateProgram();
    CAMLreturn (Val_int(v));
}

CAMLprim value gles2_gen_textures(value v1)
{
    CAMLparam1 (v1);

    const long unsigned int size = Wosize_val(v1);
    GLuint* textures = malloc(size*sizeof(GLuint));
    glGenTextures(size, textures);

    for (long unsigned int i = 0; i < size; ++i)
    {
        caml_modify(&Field(v1, i), Val_int(textures[i]));
    }

    free(textures);
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_delete_textures(value v1)
{
    CAMLparam1 (v1);
    const long unsigned int size = Wosize_val(v1);
    GLuint* textures = malloc(size*sizeof(GLuint));
    for (long unsigned int i = 0; i < size; ++i)
    {
        textures[i] = Int_val(Field(v1, i));
    }
    glDeleteTextures(size, textures);
    free(textures);
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_vertex_attrib_pointer(value index, value size, 
        value type, value norm, value stride, value ptr)
{
    CAMLparam5 (index, size, type, norm, stride);
    CAMLxparam1 (ptr);

    glVertexAttribPointer(Int_val(index), Int_val(size), 
            Int_val(type), Val_bool(norm), Int_val(stride), (void*)(intptr_t)Int_val(ptr));

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_vertex_attrib_pointer_byte(value* argv, int argn)
{
    return gles2_vertex_attrib_pointer(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value gles2_tex_image2d(value target, value level, value internal, value width, value height, value border, value format, value type, value data)
{
    CAMLparam5 (target, level, internal, width, height);
    CAMLxparam4 (border, format, type, data);

    glTexImage2D(Int_val(target), Int_val(level), Int_val(internal), Int_val(width), Int_val(height), Int_val(border), Int_val(format), Int_val(type), (void*)Caml_ba_data_val(data));

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_tex_image2d_byte(value* argv, int nargs)
{
    return gles2_tex_image2d(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
}

CAMLprim value gles2_tex_subimage2d(value target, value level, value xoff, value yoff, value width, value height, value format, value type, value data)
{
    CAMLparam5 (target, level, xoff, yoff, width);
    CAMLxparam4 (height, format, type, data);
    glTexSubImage2D(Int_val(target), Int_val(level), Int_val(xoff), Int_val(yoff), Int_val(width), Int_val(height), Int_val(format), Int_val(type), (void*)Caml_ba_data_val(data));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_tex_subimage2d_byte(value* argv, int nargs)
{
    return gles2_tex_subimage2d(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
}

CAMLprim value gles2_get_error()
{
    return CAMLprim(Val_int(glGetError()));
}

CAMLprim value gles2_create_shader(value v1)
{
    CAMLparam1 (v1);
    CAMLreturn (Val_int(glCreateShader(Int_val(v1))));
}

CAMLprim value gles2_shader_source(value shader, value source)
{
    CAMLparam2 (shader, source);

    const char* gl_source = String_val(source);
    glShaderSource(Int_val(shader), 1, &gl_source, 0);

    CAMLreturn (Val_unit);
}

CAMLprim value gles2_compile_shader(value v1)
{
    CAMLparam1 (v1);
    glCompileShader(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_attach_shader(value prog, value shader)
{
    CAMLparam2 (prog, shader);
    glAttachShader(Int_val(prog), Int_val(shader));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_get_shaderiv(value shader, value param)
{
    CAMLparam2 (shader, param);
    GLuint result = 0;
    glGetShaderiv(Int_val(shader), Int_val(param), &result);
    CAMLreturn (Val_int(result));
}

CAMLprim value gles2_link_program(value v1)
{
    CAMLparam1 (v1);
    glLinkProgram(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_get_programiv(value program, value param)
{
    CAMLparam2 (program, param); 
    GLuint result = 0;
    glGetProgramiv(Int_val(program), Int_val(param), &result);
    CAMLreturn (Val_int(result));
}

CAMLprim value gles2_bind_attrib_location(value prog, value loc, value name)
{
    CAMLparam3 (prog, loc, name);
    glBindAttribLocation(Int_val(prog), Int_val(loc), String_val(name));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_delete_program(value v1)
{
    CAMLparam1 (v1);
    glDeleteProgram(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_delete_shader(value v1)
{
    CAMLparam1 (v1);
    glDeleteShader(Int_val(v1));
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_get_shader_info_log(value v1)
{
    CAMLparam1 (v1);
    GLint length = 0;
    GLuint shader = Int_val(v1);
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);

    char* str = (char*)malloc(length * sizeof(char));
    glGetShaderInfoLog(shader, length, &length, str);
    value caml_str = caml_copy_string_of_os(str);

    free(str);
    CAMLreturn (caml_str);
}

CAMLprim value gles2_get_program_info_log(value v1)
{
    CAMLparam1 (v1);
    GLint length = 0;
    GLuint program = Int_val(v1);
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &length);

    char* str = (char*)malloc(length * sizeof(char));
    glGetProgramInfoLog(program, length, &length, str);

    value caml_str = caml_copy_string_of_os(str);
    free(str);
    CAMLreturn (caml_str);
}

CAMLprim value gles2_gen_buffers(value v1)
{
    CAMLparam1 (v1);

    const long unsigned int size = Wosize_val(v1);
    GLuint* buffers = malloc(size*sizeof(GLuint));
    glGenBuffers(size, buffers);

    for (long unsigned int i = 0; i < size; ++i)
    {
        caml_modify(&Field(v1, i), Val_int(buffers[i]));
    }

    free(buffers);
    CAMLreturn (Val_unit);
}

CAMLprim value gles2_bind_texture(value target, value tex)
{
    CAMLparam2 (target, tex);
    glBindTexture(Int_val(target), Int_val(tex));
    CAMLreturn (Val_unit);
}
