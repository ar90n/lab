use glutin::ContextWrapper;

use std::ffi::CStr;

use super::shader::{Shader, ShaderCode};
use cgmath::prelude::*;

#[allow(dead_code)]
pub type Point3 = cgmath::Point3<f32>;
#[allow(dead_code)]
pub type Vector3 = cgmath::Vector3<f32>;
#[allow(dead_code)]
pub type Matrix4 = cgmath::Matrix4<f32>;

use glutin::event_loop::EventLoop;
use glutin::window::WindowBuilder;
use glutin::ContextBuilder;
use std::ops::Deref;

pub struct WindowContext(ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>);

impl WindowContext {
    pub fn from(event_loop: &EventLoop<()>) -> Self {
        let window_builder = WindowBuilder::new().with_title("Modern OpenGL with Rust");
        let windowed_context = ContextBuilder::new()
            .build_windowed(window_builder, &event_loop)
            .unwrap();
        let windowed_context = unsafe { windowed_context.make_current().unwrap() };
        Self(windowed_context)
    }
}

impl Deref for WindowContext {
    type Target = ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub mod gl {
    pub use self::Gles2 as Gl;
    pub use super::super::gl_bindings::*;
}

pub struct RenderContext {
    gl: gl::Gl,
    shader: Shader,
    vbo: u32,
    vba: u32,
}

impl RenderContext {
    pub fn from(
        window_context: &WindowContext,
        vertex_shader_code: ShaderCode,
        fragment_shader_code: ShaderCode,
    ) -> Self {
        let gl =
            gl::Gl::load_with(|ptr| window_context.context().get_proc_address(ptr) as *const _);

        let version = unsafe {
            let data = CStr::from_ptr(gl.GetString(gl::VERSION) as *const _)
                .to_bytes()
                .to_vec();
            String::from_utf8(data).unwrap()
        };

        println!("OpenGL version {}", version);

        let shader = Shader::new(&gl, &vertex_shader_code, &fragment_shader_code);
        unsafe {
            gl.UseProgram(shader.id);
        }
        Self {
            gl: gl,
            shader,
            vbo: 0,
            vba: 0,
        }
    }

    pub unsafe fn viewport(
        &self,
        x: gl::types::GLint,
        y: gl::types::GLint,
        width: gl::types::GLsizei,
        height: gl::types::GLsizei,
    ) {
        self.gl.Viewport(x, y, width, height);
    }

    pub unsafe fn clear_color(
        &self,
        red: gl::types::GLfloat,
        green: gl::types::GLfloat,
        blue: gl::types::GLfloat,
        alpha: gl::types::GLfloat,
    ) {
        self.gl.ClearColor(red, green, blue, alpha);
    }

    pub unsafe fn clear(&self, mask: gl::types::GLbitfield) {
        self.gl.Clear(mask);
    }

    pub unsafe fn set_bool(&self, name: &str, value: bool) {
        self.gl.Uniform1i(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value as i32,
        );
    }

    pub unsafe fn set_int(&self, name: &str, value: i32) {
        self.gl.Uniform1i(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_float(&self, name: &str, value: f32) {
        self.gl.Uniform1f(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_vector3(&self, name: &str, value: &Vector3) {
        self.gl.Uniform3fv(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            1,
            value.as_ptr(),
        );
    }

    pub unsafe fn set_vec3(&self, name: &str, x: f32, y: f32, z: f32) {
        self.gl.Uniform3f(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            x,
            y,
            z,
        );
    }

    pub unsafe fn set_mat4(&self, name: &str, mat: &Matrix4) {
        self.gl.UniformMatrix4fv(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            1,
            gl::FALSE,
            mat.as_ptr(),
        );
    }

    pub unsafe fn draw(&self) {
        //gl.BindVertexArray(self.vba);
        self.gl.DrawArrays(gl::TRIANGLES, 0, 3);
        //gl.BindVertexArray(0);
    }

    pub unsafe fn load_vertex(&mut self, vertices: &[f32]) {
        let mut vb = std::mem::zeroed();
        self.gl.GenBuffers(1, &mut vb);
        self.gl.BindBuffer(gl::ARRAY_BUFFER, vb);
        self.gl.BufferData(
            gl::ARRAY_BUFFER,
            (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
            vertices.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );
        self.vbo = vb;

        if self.gl.BindVertexArray.is_loaded() {
            let mut vao = std::mem::zeroed();
            self.gl.GenVertexArrays(1, &mut vao);
            self.gl.BindVertexArray(vao);
            self.vba = vao;
        }

        let pos_attrib = self
            .gl
            .GetAttribLocation(self.shader.id, b"position\0".as_ptr() as *const _);
        let color_attrib = self
            .gl
            .GetAttribLocation(self.shader.id, b"color\0".as_ptr() as *const _);
        self.gl.VertexAttribPointer(
            pos_attrib as gl::types::GLuint,
            2,
            gl::FLOAT,
            0,
            5 * std::mem::size_of::<f32>() as gl::types::GLsizei,
            std::ptr::null(),
        );
        self.gl.VertexAttribPointer(
            color_attrib as gl::types::GLuint,
            3,
            gl::FLOAT,
            0,
            5 * std::mem::size_of::<f32>() as gl::types::GLsizei,
            (2 * std::mem::size_of::<f32>()) as *const () as *const _,
        );
        self.gl
            .EnableVertexAttribArray(pos_attrib as gl::types::GLuint);
        self.gl
            .EnableVertexAttribArray(color_attrib as gl::types::GLuint);
    }
}
