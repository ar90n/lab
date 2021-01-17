use glutin::PossiblyCurrent;

use std::ffi::CStr;

pub mod gl {
    pub use self::Gles2 as Gl;
    pub use super::super::gl_bindings::*;
}

pub struct Gl {
    pub gl: gl::Gl,
}

pub fn load(gl_context: &glutin::Context<PossiblyCurrent>) -> Gl {
    let gl = gl::Gl::load_with(|ptr| gl_context.get_proc_address(ptr) as *const _);

    let version = unsafe {
        let data = CStr::from_ptr(gl.GetString(gl::VERSION) as *const _)
            .to_bytes()
            .to_vec();
        String::from_utf8(data).unwrap()
    };

    println!("OpenGL version {}", version);

    Gl { gl }
}

impl Gl {
    //pub fn load_vertices(&self, vertices: &[f32]) {
    //    let gl = &self.gl;
    //    unsafe {

    //        let pos_attrib = gl.GetAttribLocation(program, b"position\0".as_ptr() as *const _);
    //        let color_attrib = gl.GetAttribLocation(program, b"color\0".as_ptr() as *const _);
    //        gl.VertexAttribPointer(
    //            pos_attrib as gl::types::GLuint,
    //            3,
    //            gl::FLOAT,
    //            0,
    //            3 * std::mem::size_of::<f32>() as gl::types::GLsizei,
    //            std::ptr::null(),
    //        );
    //        //gl.VertexAttribPointer(
    //        //    color_attrib as gl::types::GLuint,
    //        //    3,
    //        //    gl::FLOAT,
    //        //    0,
    //        //    5 * std::mem::size_of::<f32>() as gl::types::GLsizei,
    //        //    (2 * std::mem::size_of::<f32>()) as *const () as *const _,
    //        //);
    //        gl.EnableVertexAttribArray(pos_attrib as gl::types::GLuint);
    //        //gl.EnableVertexAttribArray(color_attrib as gl::types::GLuint);
    //    }
    //}
}