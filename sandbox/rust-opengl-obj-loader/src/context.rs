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
    ebo: u32,
    ebo_size: usize,
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
            ebo: 0,
            ebo_size: 0,
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
        self.gl.DrawElements(
            gl::TRIANGLES,
            self.ebo_size as i32,
            gl::UNSIGNED_INT,
            0 as *const _,
        );
        self.gl.BindVertexArray(0);
    }

    pub unsafe fn load_model(&mut self, name: &CStr, model: &super::obj::Model) {
        let mut vbo = std::mem::zeroed();
        self.gl.GenBuffers(1, &mut vbo);
        self.gl.BindBuffer(gl::ARRAY_BUFFER, vbo);
        self.gl.BufferData(
            gl::ARRAY_BUFFER,
            (model.vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
            model.vertices.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );

        let mut vao = std::mem::zeroed();
        self.gl.GenVertexArrays(1, &mut vao);
        self.gl.BindVertexArray(vao);
        self.vao = vao;

        let mut ebo = std::mem::zeroed();
        self.gl.GenBuffers(1, &mut ebo);
        self.gl.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebo);
        self.gl.BufferData(
            gl::ELEMENT_ARRAY_BUFFER,
            (model.facets.len() * std::mem::size_of::<u32>()) as gl::types::GLsizeiptr,
            model.facets.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );
        self.ebo = ebo;
        self.ebo_size = model.facets.len();

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
