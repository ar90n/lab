mod context;
mod gl_bindings;
mod obj;
mod shader;

use context::{Matrix4, Point3, RenderContext, Vector3, WindowContext};

use c_str_macro::c_str;
use cgmath::perspective;
use glutin::event::{Event, WindowEvent};
use glutin::event_loop::{ControlFlow, EventLoop};

use std::time::Duration;

use context::gl;
use itertools::Itertools;

use shader::ShaderCode;

const WINDOW_WIDTH: u32 = 900;
const WINDOW_HEIGHT: u32 = 480;

fn create_view_matrix(radius: f32, theta: f32) -> cgmath::Matrix4<f32> {
    Matrix4::look_at_rh(
        Point3 {
            x: radius * theta.cos(),
            y: 0.0,
            z: radius * theta.sin(),
        },
        Point3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        },
        Vector3 {
            x: 0.0,
            y: 1.0,
            z: 0.0,
        },
    )
}

fn create_model_matrix(model: &obj::Model) -> (f32, Matrix4) {
    let (acc_pos, max_pos, min_pos) = model
        .vertices
        .iter()
        .chunks(3)
        .into_iter()
        .map(|chunk| chunk.into_iter().cloned().collect::<Vec<f32>>())
        .fold(
            (
                (0.0 as f32, 0.0 as f32, 0.0 as f32),
                (0.0 as f32, 0.0 as f32, 0.0 as f32),
                (std::f32::INFINITY, std::f32::INFINITY, std::f32::INFINITY),
            ),
            |((acc_x, acc_y, acc_z), (max_x, max_y, max_z), (min_x, min_y, min_z)), chunk| {
                let x = chunk[0];
                let y = chunk[1];
                let z = chunk[2];

                (
                    (acc_x + x, acc_y + y, acc_z + z),
                    (max_x.max(x), max_y.max(y), max_z.max(z)),
                    (min_x.min(x), min_y.min(y), min_z.min(z)),
                )
            },
        );

    let center_pos = (
        3.0 * acc_pos.0 / model.vertices.len() as f32,
        3.0 * acc_pos.1 / model.vertices.len() as f32,
        3.0 * acc_pos.2 / model.vertices.len() as f32,
    );

    let radius = (max_pos.0 - min_pos.0)
        .max(max_pos.1 - min_pos.1)
        .max(max_pos.2 - min_pos.2);

    let model_matrix = Matrix4::new(
        1f32,
        0f32,
        0f32,
        0f32,
        0f32,
        1f32,
        0f32,
        0f32,
        0f32,
        0f32,
        1f32,
        0f32,
        -center_pos.0,
        -center_pos.1,
        -center_pos.2,
        1f32,
    );
    (radius, model_matrix)
}

fn main() {
    let el = EventLoop::new();
    let window_context = WindowContext::from(&el);
    let mut render_context = RenderContext::from(
        &window_context,
        ShaderCode::new("rsc/shader/shader.vs", gl::VERTEX_SHADER),
        ShaderCode::new("rsc/shader/shader.fs", gl::FRAGMENT_SHADER),
    );
    //let model = obj::Model::new(&"rsc/obj/teapot.obj".to_string());
    let model = obj::Model::new(&"rsc/obj/bunny.obj".to_string());

    let (radius, model_matrix) = create_model_matrix(&model);
    let projection_matrix: Matrix4 = perspective(
        cgmath::Deg(45.0f32),
        WINDOW_WIDTH as f32 / WINDOW_HEIGHT as f32,
        0.1,
        100.0,
    );

    unsafe {
        render_context.load_model(c_str!("Position"), &model);
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
        let view_matrix = create_view_matrix(2.0 * radius, theta);
        unsafe {
            render_context.set_mat4(c_str!("View"), &view_matrix);
            render_context.draw();
        }

        window_context.swap_buffers().unwrap();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        n += 1;
    });
}
