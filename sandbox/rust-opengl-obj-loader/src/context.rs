use glutin::ContextWrapper;

use super::shader::{Shader, ShaderCode};
use cgmath::prelude::*;
use std::ffi::CStr;

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
        let window_builder = WindowBuilder::new().with_title("Rust Obj Loader");
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
    pub use super::super::gl_bindings::*;
}

pub struct RenderContext {
    gl: gl::Gl,
    shader: Shader,
    vao: u32,
    vertex_count: usize,
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
            vao: 0,
            vertex_count: 0,
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

    pub unsafe fn set_bool(&self, name: &CStr, value: bool) {
        self.gl.Uniform1i(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value as i32,
        );
    }

    pub unsafe fn set_int(&self, name: &CStr, value: i32) {
        self.gl.Uniform1i(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_float(&self, name: &CStr, value: f32) {
        self.gl.Uniform1f(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_vector3(&self, name: &CStr, value: &Vector3) {
        self.gl.Uniform3fv(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            1,
            value.as_ptr(),
        );
    }

    pub unsafe fn set_vec3(&self, name: &CStr, x: f32, y: f32, z: f32) {
        self.gl.Uniform3f(
            self.gl
                .GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            x,
            y,
            z,
        );
    }

    pub unsafe fn set_mat4(&self, name: &CStr, mat: &Matrix4) {
        self.gl.UniformMatrix4fv(
            self.gl.GetUniformLocation(self.shader.id, name.as_ptr()),
            1,
            gl::FALSE,
            mat.as_ptr(),
        );
    }

    pub unsafe fn draw(&self) {
        self.clear_color(1.0, 1.0, 1.0, 1.0);
        self.clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);

        self.gl.BindVertexArray(self.vao);

        self.gl.PolygonMode(gl::FRONT_AND_BACK, gl::LINE);
        self.gl
            .DrawArrays(gl::TRIANGLES, 0, self.vertex_count as i32);
        self.gl.BindVertexArray(0);
    }

    pub unsafe fn load_vertex(&mut self, name: &CStr, vertices: &[f32]) {
        let mut vbo = std::mem::zeroed();
        self.gl.GenBuffers(1, &mut vbo);
        self.gl.BindBuffer(gl::ARRAY_BUFFER, vbo);
        self.gl.BufferData(
            gl::ARRAY_BUFFER,
            (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
            vertices.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );
        self.vertex_count = vertices.len();

        let mut vao = std::mem::zeroed();
        self.gl.GenVertexArrays(1, &mut vao);
        self.gl.BindVertexArray(vao);
        self.vao = vao;

        let id = self.gl.GetAttribLocation(self.shader.id, name.as_ptr()) as gl::types::GLuint;
        self.gl.VertexAttribPointer(
            id,
            3,
            gl::FLOAT,
            0,
            3 * std::mem::size_of::<f32>() as gl::types::GLsizei,
            std::ptr::null(),
        );
        self.gl.EnableVertexAttribArray(id);
    }
}
