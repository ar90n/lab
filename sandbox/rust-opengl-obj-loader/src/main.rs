mod context;
mod gl_bindings;
mod shader;

use context::{Matrix4, Point3, RenderContext, Vector3, WindowContext};

use c_str_macro::c_str;
use cgmath::perspective;
use cgmath::prelude::*;
use glutin::event::{Event, WindowEvent};
use glutin::event_loop::{ControlFlow, EventLoop};

use std::time::Duration;

use context::gl;

use shader::ShaderCode;

const WINDOW_WIDTH: u32 = 900;
const WINDOW_HEIGHT: u32 = 480;
const FLOAT_NUM: usize = 3;
const VERTEX_NUM: usize = 36;
const BUF_LEN: usize = FLOAT_NUM * VERTEX_NUM;

fn craete_vertices() -> [f32; BUF_LEN] {
    #[rustfmt::skip]
    let buffer_array: [f32; BUF_LEN] = [
        // 1
        0.0, 0.0, 0.0,
        0.0, 1.0, 0.0,
        1.0, 1.0, 0.0,
    
        0.0, 0.0, 0.0,
        1.0, 1.0, 0.0,
        1.0, 0.0, 0.0,
    
        // 2
        0.0, 0.0, 1.0,
        0.0, 0.0, 0.0,
        1.0, 0.0, 0.0,
    
        0.0, 0.0, 1.0,
        1.0, 0.0, 0.0,
        1.0, 0.0, 1.0,
    
        // 3
        0.0, 1.0, 1.0,
        0.0, 0.0, 1.0,
        1.0, 0.0, 1.0,
    
        0.0, 1.0, 1.0,
        1.0, 0.0, 1.0,
        1.0, 1.0, 1.0,
    
        // 4
        0.0, 1.0, 0.0,
        0.0, 1.0, 1.0,
        1.0, 1.0, 1.0,
    
        0.0, 1.0, 0.0,
        1.0, 1.0, 1.0,
        1.0, 1.0, 0.0,
    
        // 5
        1.0, 0.0, 1.0,
        1.0, 0.0, 0.0,
        1.0, 1.0, 0.0,
    
        1.0, 0.0, 1.0,
        1.0, 1.0, 0.0,
        1.0, 1.0, 1.0,
    
        // 6
        0.0, 1.0, 1.0,
        0.0, 1.0, 0.0,
        0.0, 0.0, 0.0,
    
        0.0, 1.0, 1.0,
        0.0, 0.0, 0.0,
        0.0, 0.0, 1.0,
    ];

    return buffer_array;
}

fn create_view_matrix(theta: f32) -> cgmath::Matrix4<f32> {
    Matrix4::look_at(
        Point3 {
            x: 5.0 * theta.cos(),
            y: 5.0 * theta.sin(),
            z: 0.0,
        },
        Point3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        },
        Vector3 {
            x: 0.0,
            y: 0.0,
            z: 1.0,
        },
    )
}

fn main() {
    let el = EventLoop::new();
    let window_context = WindowContext::from(&el);
    let mut render_context = RenderContext::from(
        &window_context,
        ShaderCode::new("rsc/shader/shader.vs", gl::VERTEX_SHADER),
        ShaderCode::new("rsc/shader/shader.fs", gl::FRAGMENT_SHADER),
    );

    let model_matrix = <Matrix4 as SquareMatrix>::identity();
    let projection_matrix: Matrix4 = perspective(
        cgmath::Deg(45.0f32),
        WINDOW_WIDTH as f32 / WINDOW_HEIGHT as f32,
        0.1,
        100.0,
    );

    unsafe {
        render_context.load_vertex(c_str!("Position"), &craete_vertices());
        render_context.viewport(0, 0, WINDOW_WIDTH as i32, WINDOW_HEIGHT as i32);
        render_context.set_mat4(c_str!("Model"), &model_matrix);
        render_context.set_mat4(c_str!("Projection"), &projection_matrix);
    }

    let mut n = 0;
    el.run(move |event, _, control_flow| {
        match event {
            Event::LoopDestroyed => return,
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(physical_size) => window_context.resize(physical_size),
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                _ => (),
            },
            Event::RedrawRequested(_) => {}
            _ => (),
        }

        let theta = 2.0 * 3.1415 * (n as f32) / 60.0;
        let view_matrix = create_view_matrix(theta);
        unsafe {
            render_context.set_mat4(c_str!("View"), &view_matrix);
            render_context.draw();
        }

        window_context.swap_buffers().unwrap();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        n += 1;
    });
}
