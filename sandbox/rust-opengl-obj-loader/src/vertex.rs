// from https://github.com/toyamaguchi/rust_opengl

use std::mem;
use std::os::raw::c_void;

use crate::support::gl::types::{GLenum, GLfloat, GLint, GLsizei, GLsizeiptr};

pub struct Vertex {
    vao: u32,
    _vbo: u32,
    vertex_num: i32,
}

impl Vertex {
    pub fn new(buffer_array: &[f32], attr_size: i32) -> Vertex {
        let size: GLsizeiptr = buffer_array.len() as isize * mem::size_of::<GLfloat>() as isize;
        let data: *const c_void = buffer_array.as_ptr() as *const c_void;
        let usage: GLenum = gl::STATIC_DRAW;
        let attribute_type_vec: std::vec::Vec<GLenum> = vec![gl::FLOAT];
        let attribute_size_vec: std::vec::Vec<GLint> = vec![attr_size];
        let stride: GLsizei = attr_size * mem::size_of::<GLfloat>() as i32;
        let vertex_num: i32 = (size / stride as isize) as i32;

        let mut vao = 0;
        let mut vbo = 0;

        unsafe {
            // create vertex array and vertex buffer
            gl::GenVertexArrays(1, &mut vao);
            gl::GenBuffers(1, &mut vbo);

            // bind buffer
            gl::BindVertexArray(vao);
            gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
            gl::BufferData(gl::ARRAY_BUFFER, size, data, usage);

            let mut offset = 0;
            for i in 0..attribute_type_vec.len() {
                gl::EnableVertexAttribArray(i as u32);
                gl::VertexAttribPointer(
                    i as u32,
                    attribute_size_vec[i],
                    attribute_type_vec[i],
                    gl::FALSE,
                    stride,
                    (offset * mem::size_of::<GLfloat>()) as *const c_void,
                );
                offset += attribute_size_vec[i] as usize;
            }

            // unbind
            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
            gl::BindVertexArray(0);
        }

        Vertex {
            vao: vao,
            _vbo: vbo,
            vertex_num: vertex_num,
        }
    }

    pub fn draw(&self) {
        unsafe {
            gl::BindVertexArray(self.vao);
            gl::DrawArrays(gl::TRIANGLES, 0, self.vertex_num);
            gl::BindVertexArray(0);
        }
    }
}
