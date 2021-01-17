mod gl_bindings;
mod shader;
mod support;
mod vertex;

use glutin::event::{Event, WindowEvent};
use glutin::event_loop::{ControlFlow, EventLoop};
use glutin::window::WindowBuilder;
use glutin::ContextBuilder;

use cgmath::perspective;
use cgmath::prelude::*;

#[allow(dead_code)]
type Point3 = cgmath::Point3<f32>;
#[allow(dead_code)]
type Vector3 = cgmath::Vector3<f32>;
#[allow(dead_code)]
type Matrix4 = cgmath::Matrix4<f32>;

use gl_bindings::*;

use std::mem;
use std::os::raw::c_void;
use std::time::Duration;

use c_str_macro::*;
use std::ffi::CStr;
use support::gl;
use support::gl::types::*;
use support::Gl;

use shader::{Shader, ShaderCode};
use vertex::Vertex;

const WINDOW_WIDTH: u32 = 900;
const WINDOW_HEIGHT: u32 = 480;
const FLOAT_NUM: usize = 5;
const VERTEX_NUM: usize = 3;
const BUF_LEN: usize = FLOAT_NUM * VERTEX_NUM;

fn craete_vertices() -> [f32; BUF_LEN] {
    #[rustfmt::skip]
    let buffer_array = [
        -0.5, -0.5, 1.0, 0.0, 0.0,
         0.0,  0.5, 0.0, 1.0, 0.0,
         0.5, -0.5, 0.0, 0.0, 1.0,
    ];

    //    #[rustfmt::skip]
    //    let buffer_array: [f32; BUF_LEN] = [
    //        // 1
    //        0.0, 0.0, 0.0,
    //        0.0, 1.0, 0.0,
    //        1.0, 1.0, 0.0,
    //
    //        0.0, 0.0, 0.0,
    //        1.0, 1.0, 0.0,
    //        1.0, 0.0, 0.0,
    //
    //        // 2
    //        0.0, 0.0, 1.0,
    //        0.0, 0.0, 0.0,
    //        1.0, 0.0, 0.0,
    //
    //        0.0, 0.0, 1.0,
    //        1.0, 0.0, 0.0,
    //        1.0, 0.0, 1.0,
    //
    //        // 3
    //        0.0, 1.0, 1.0,
    //        0.0, 0.0, 1.0,
    //        1.0, 0.0, 1.0,
    //
    //        0.0, 1.0, 1.0,
    //        1.0, 0.0, 1.0,
    //        1.0, 1.0, 1.0,
    //
    //        // 4
    //        0.0, 1.0, 0.0,
    //        0.0, 1.0, 1.0,
    //        1.0, 1.0, 1.0,
    //
    //        0.0, 1.0, 0.0,
    //        1.0, 1.0, 1.0,
    //        1.0, 1.0, 0.0,
    //
    //        // 5
    //        1.0, 0.0, 1.0,
    //        1.0, 0.0, 0.0,
    //        1.0, 1.0, 0.0,
    //
    //        1.0, 0.0, 1.0,
    //        1.0, 1.0, 0.0,
    //        1.0, 1.0, 1.0,
    //
    //        // 6
    //        0.0, 1.0, 1.0,
    //        0.0, 1.0, 0.0,
    //        0.0, 0.0, 0.0,
    //
    //        0.0, 1.0, 1.0,
    //        0.0, 0.0, 0.0,
    //        0.0, 0.0, 1.0,
    //    ];

    return buffer_array;
}

struct RenderContext {
    gl: Gl,
    shader: Shader,
    vbo: u32,
    vba: u32,
}

impl RenderContext {
    pub fn new(gl_: Gl, vertex_shader_code: ShaderCode, fragment_shader_code: ShaderCode) -> Self {
        let shader = Shader::new(&gl_, &vertex_shader_code, &fragment_shader_code);
        unsafe {
            gl_.gl.UseProgram(shader.id);
        }
        Self {
            gl: gl_,
            shader,
            vbo: 0,
            vba: 0,
        }
    }

    pub unsafe fn viewport(
        &self,
        x: types::GLint,
        y: types::GLint,
        width: types::GLsizei,
        height: types::GLsizei,
    ) {
        let gl = &self.gl.gl;
        gl.Viewport(x, y, width, height);
    }

    pub unsafe fn clear_color(
        &self,
        red: gl::types::GLfloat,
        green: gl::types::GLfloat,
        blue: gl::types::GLfloat,
        alpha: gl::types::GLfloat,
    ) {
        let gl = &self.gl.gl;
        gl.ClearColor(red, green, blue, alpha);
    }

    pub unsafe fn clear(&self, mask: gl::types::GLbitfield) {
        let gl = &self.gl.gl;
        gl.Clear(mask);
    }

    pub unsafe fn set_bool(&self, name: &str, value: bool) {
        let gl = &self.gl.gl;
        gl.Uniform1i(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value as i32,
        );
    }

    pub unsafe fn set_int(&self, name: &str, value: i32) {
        let gl = &self.gl.gl;
        gl.Uniform1i(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_float(&self, name: &str, value: f32) {
        let gl = &self.gl.gl;
        gl.Uniform1f(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            value,
        );
    }

    pub unsafe fn set_vector3(&self, name: &str, value: &Vector3) {
        let gl = &self.gl.gl;
        gl.Uniform3fv(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            1,
            value.as_ptr(),
        );
    }

    pub unsafe fn set_vec3(&self, name: &str, x: f32, y: f32, z: f32) {
        let gl = &self.gl.gl;
        gl.Uniform3f(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            x,
            y,
            z,
        );
    }

    pub unsafe fn set_mat4(&self, name: &str, mat: &Matrix4) {
        let gl = &self.gl.gl;
        gl.UniformMatrix4fv(
            gl.GetUniformLocation(self.shader.id, name.as_ptr() as *const _),
            1,
            gl::FALSE,
            mat.as_ptr(),
        );
    }

    pub unsafe fn draw(&self) {
        let gl = &self.gl.gl;

        //gl.BindVertexArray(self.vba);
        gl.DrawArrays(gl::TRIANGLES, 0, 3);
        //gl.BindVertexArray(0);
    }

    pub unsafe fn load_vertex(&mut self, vertices: &[f32]) {
        let gl = &self.gl.gl;

        let mut vb = std::mem::zeroed();
        gl.GenBuffers(1, &mut vb);
        gl.BindBuffer(gl::ARRAY_BUFFER, vb);
        gl.BufferData(
            gl::ARRAY_BUFFER,
            (vertices.len() * std::mem::size_of::<f32>()) as gl::types::GLsizeiptr,
            vertices.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );
        self.vbo = vb;

        if gl.BindVertexArray.is_loaded() {
            let mut vao = std::mem::zeroed();
            gl.GenVertexArrays(1, &mut vao);
            gl.BindVertexArray(vao);
            self.vba = vao;
        }

        let pos_attrib = gl.GetAttribLocation(self.shader.id, b"position\0".as_ptr() as *const _);
        let color_attrib = gl.GetAttribLocation(self.shader.id, b"color\0".as_ptr() as *const _);
        gl.VertexAttribPointer(
            pos_attrib as gl::types::GLuint,
            2,
            gl::FLOAT,
            0,
            5 * std::mem::size_of::<f32>() as gl::types::GLsizei,
            std::ptr::null(),
        );
        gl.VertexAttribPointer(
            color_attrib as gl::types::GLuint,
            3,
            gl::FLOAT,
            0,
            5 * std::mem::size_of::<f32>() as gl::types::GLsizei,
            (2 * std::mem::size_of::<f32>()) as *const () as *const _,
        );
        gl.EnableVertexAttribArray(pos_attrib as gl::types::GLuint);
        gl.EnableVertexAttribArray(color_attrib as gl::types::GLuint);
    }
}

fn main() {
    let el = EventLoop::new();
    let wb = WindowBuilder::new().with_title("Modern OpenGL with Rust");
    let windowed_context = ContextBuilder::new().build_windowed(wb, &el).unwrap();
    let windowed_context = unsafe { windowed_context.make_current().unwrap() };

    let buffer_array = craete_vertices();
    //let vertex = Vertex::new(&buffer_array, FLOAT_NUM as i32);

    let mut render_context = RenderContext::new(
        support::load(&windowed_context.context()),
        ShaderCode::new("rsc/shader/shader.vs", gl::VERTEX_SHADER),
        ShaderCode::new("rsc/shader/shader.fs", gl::FRAGMENT_SHADER),
    );

    unsafe {
        render_context.load_vertex(&buffer_array);
    }

    el.run(move |event, _, control_flow| {
        println!("{:?}", event);
        //*control_flow = ControlFlow::Wait;

        match event {
            Event::LoopDestroyed => return,
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(physical_size) => windowed_context.resize(physical_size),
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                _ => (),
            },
            Event::RedrawRequested(_) => {}
            _ => (),
        }

        // init matrice for model, view and projection
        let model_matrix = <Matrix4 as SquareMatrix>::identity();
        let view_matrix = Matrix4::look_at(
            Point3 {
                x: 0.0,
                y: 5.0,
                z: 0.0,
            },
            Point3 {
                x: 0.5,
                y: 0.5,
                z: 0.5,
            },
            Vector3 {
                x: 0.0,
                y: 0.0,
                z: 1.0,
            },
        );
        let projection_matrix: Matrix4 = perspective(
            cgmath::Deg(45.0f32),
            WINDOW_WIDTH as f32 / WINDOW_HEIGHT as f32,
            0.1,
            100.0,
        );

        unsafe {
            //render_context.viewport(0, 0, WINDOW_WIDTH as i32, WINDOW_HEIGHT as i32);
            render_context.clear_color(1.0, 0.5, 0.7, 1.0);
            render_context.clear(gl::COLOR_BUFFER_BIT);

            //render_context.set_mat4("uModel", &model_matrix);
            //render_context.set_mat4("uView", &view_matrix);
            //render_context.set_mat4("uProjection", &projection_matrix);
            render_context.draw();
        }
        windowed_context.swap_buffers().unwrap();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    });
}
