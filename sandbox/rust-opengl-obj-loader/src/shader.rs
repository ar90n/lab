// from https://github.com/toyamaguchi/rust_opengl

use crate::context::gl::Gl;
use std::ffi::CString;
use std::fs::File;
use std::io::Read;
use std::ptr;
use std::str;

#[allow(dead_code)]
type Vector3 = cgmath::Vector3<f32>;
#[allow(dead_code)]
type Matrix4 = cgmath::Matrix4<f32>;

pub struct ShaderCode {
    pub code: CString,
    pub type_: gl::types::GLenum,
}

#[allow(dead_code)]
impl ShaderCode {
    pub fn new(src_path: &str, type_: gl::types::GLenum) -> Self {
        let code = {
            let mut code = String::new();
            File::open(src_path)
                .and_then(|mut file| file.read_to_string(&mut code))
                .expect("failed to read vertex shader file");

            CString::new(code.as_bytes()).unwrap()
        };

        Self { code, type_ }
    }
}

pub struct Shader {
    pub id: u32,
}

impl Shader {
    fn compile_shader(gl: &Gl, shader_code: &ShaderCode) -> u32 {
        unsafe {
            let id = gl.CreateShader(shader_code.type_);
            gl.ShaderSource(id, 1, &shader_code.code.as_ptr(), ptr::null());
            gl.CompileShader(id);
            id
        }
    }

    fn craete_program(gl: &Gl, vertex: u32, fragment: u32) -> u32 {
        unsafe {
            let id = gl.CreateProgram();
            gl.AttachShader(id, vertex);
            gl.AttachShader(id, fragment);
            gl.LinkProgram(id);
            id
        }
    }

    fn delete_shader(gl: &Gl, shader: u32) {
        unsafe {
            gl.DeleteShader(shader);
        }
    }

    #[rustfmt::skip]
    pub fn new(gl: &Gl, vertex_shader_code: &ShaderCode, fragment_shader_code: &ShaderCode) -> Self {
        let vertex = Self::compile_shader(gl, vertex_shader_code);
        let fragment = Self::compile_shader(gl, fragment_shader_code);

        let id = Self::craete_program(gl, vertex, fragment);

        Self::delete_shader(gl,vertex);
        Self::delete_shader(gl,fragment);

        Self {
            id
        }
    }
}
