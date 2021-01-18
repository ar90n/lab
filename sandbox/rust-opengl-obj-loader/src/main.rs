mod context;
mod gl_bindings;
mod shader;
mod vertex;

use context::{Matrix4, Point3, RenderContext, Vector3, WindowContext};

use cgmath::perspective;
use cgmath::prelude::*;
use glutin::event::{Event, WindowEvent};
use glutin::event_loop::{ControlFlow, EventLoop};

use std::time::Duration;

use context::gl;

use shader::ShaderCode;

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

fn main() {
    let el = EventLoop::new();
    let window_context = WindowContext::from(&el);
    let mut render_context = RenderContext::from(
        &window_context,
        ShaderCode::new("rsc/shader/shader.vs", gl::VERTEX_SHADER),
        ShaderCode::new("rsc/shader/shader.fs", gl::FRAGMENT_SHADER),
    );

    unsafe {
        render_context.load_vertex(&craete_vertices());
    }

    el.run(move |event, _, control_flow| {
        println!("{:?}", event);
        //*control_flow = ControlFlow::Wait;

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
            render_context.viewport(0, 0, WINDOW_WIDTH as i32, WINDOW_HEIGHT as i32);
            render_context.clear_color(1.0, 0.5, 0.7, 1.0);
            render_context.clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);

            //render_context.set_mat4("uModel", &model_matrix);
            //render_context.set_mat4("uView", &view_matrix);
            //render_context.set_mat4("uProjection", &projection_matrix);
            render_context.draw();
        }

        window_context.swap_buffers().unwrap();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    });
}
