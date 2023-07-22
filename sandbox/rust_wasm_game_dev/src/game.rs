#[macro_use]
use crate::browser;

use crate::engine;
use crate::engine::{Game, KeyState, Point, Rect, Renderer};
use anyhow::Result;
use async_trait::async_trait;
use std::collections::HashMap;
use web_sys::HtmlImageElement;

use anyhow::anyhow;
use serde::Deserialize;

use self::red_hat_boyd_states::*;

#[derive(Deserialize, Clone)]
struct SheetRect {
    x: u16,
    y: u16,
    w: u16,
    h: u16,
}

#[derive(Deserialize, Clone)]
struct Cell {
    frame: SheetRect,
}

#[derive(Deserialize, Clone)]
struct Sheet {
    frames: HashMap<String, Cell>,
}
pub enum WalkTheDog {
    Loading,
    Loaded(RedHatBoy),
}

impl WalkTheDog {
    pub fn new() -> Self {
        WalkTheDog::Loading
    }
}

#[async_trait(?Send)]
impl Game for WalkTheDog {
    async fn initialize(&self) -> Result<Box<dyn Game>> {
        match self {
            Self::Loading => {
                let json = browser::fetch_json("rhb.json").await?;

                let rhb = RedHatBoy::new(
                    json.into_serde::<Sheet>()?,
                    engine::load_image("rhb.png").await?,
                );
                Ok(Box::new(Self::Loaded(rhb)))
            }
            Self::Loaded(_) => Err(anyhow!("Error: Game is already initialized!")),
        }
    }

    fn update(&mut self, keystate: &KeyState) {
        if let Self::Loaded(rhb) = self {
            if keystate.is_pressed("ArrowDown") {
                rhb.slide();
            }
            if keystate.is_pressed("ArrowUp") {}
            if keystate.is_pressed("ArrowRight") {
                rhb.run_right();
            }
            if keystate.is_pressed("ArrowLeft") {}
            if keystate.is_pressed("Space") {
                rhb.jump();
            }
            rhb.update();
        }
    }

    fn draw(&self, renderer: &Renderer) {
        renderer.clear(&Rect {
            x: 0.0,
            y: 0.0,
            width: 600.0,
            height: 600.0,
        });

        if let Self::Loaded(rhb) = self {
            rhb.draw(&renderer);
        }
    }
}

struct RedHatBoy {
    state_machine: RedHatBoyStateMachine,
    sprite_sheet: Sheet,
    image: HtmlImageElement,
}

impl RedHatBoy {
    fn new(sheet: Sheet, image: HtmlImageElement) -> Self {
        RedHatBoy {
            state_machine: RedHatBoyStateMachine::Idle(RedHatBoyState::new()),
            sprite_sheet: sheet,
            image: image,
        }
    }

    fn draw(&self, renderer: &Renderer) {
        let frame_name = format!(
            "{} ({}).png",
            self.state_machine.frame_name(),
            (self.state_machine.context().frame / 3) + 1
        );
        let sprite = self
            .sprite_sheet
            .frames
            .get(&frame_name)
            .expect("Cell not found");

        renderer.draw_image(
            &self.image,
            &Rect {
                x: sprite.frame.x.into(),
                y: sprite.frame.y.into(),
                width: sprite.frame.w.into(),
                height: sprite.frame.h.into(),
            },
            &Rect {
                x: self.state_machine.context().position.x.into(),
                y: self.state_machine.context().position.y.into(),
                width: sprite.frame.w.into(),
                height: sprite.frame.h.into(),
            },
        );
    }

    fn update(&mut self) {
        self.state_machine = self.state_machine.update();
    }

    fn run_right(&mut self) {
        self.state_machine = self.state_machine.transition(Event::Run)
    }

    fn slide(&mut self) {
        self.state_machine = self.state_machine.transition(Event::Slide)
    }

    fn jump(&mut self) {
        self.state_machine = self.state_machine.transition(Event::Jump)
    }
}

#[derive(Copy, Clone)]
struct Idle;
#[derive(Copy, Clone)]
struct Running;
#[derive(Copy, Clone)]
struct Slide;
#[derive(Copy, Clone)]
struct Jump;

#[derive(Copy, Clone)]
enum RedHatBoyStateMachine {
    Idle(RedHatBoyState<Idle>),
    Running(RedHatBoyState<Running>),
    Slide(RedHatBoyState<Slide>),
    Jump(RedHatBoyState<Jump>),
}

pub enum Event {
    Run,
    Slide,
    Update,
    Jump,
}

const IDLE_FRAMES: u8 = 29;
const RUNNING_FRAMES: u8 = 23;
const SLIDE_FRAMES: u8 = 14;
const JUMP_FRAMES: u8 = 35;

impl RedHatBoyStateMachine {
    fn transition(self, event: Event) -> Self {
        match (self, event) {
            (Self::Idle(state), Event::Run) => state.run().into(),
            (Self::Running(state), Event::Slide) => state.slide().into(),
            (Self::Running(state), Event::Jump) => state.jump().into(),
            (Self::Idle(state), Event::Update) => state.update().into(),
            (Self::Running(state), Event::Update) => state.update().into(),
            (Self::Slide(state), Event::Update) => state.update().into(),
            (Self::Jump(state), Event::Update) => state.update().into(),
            _ => self,
        }
    }

    fn frame_name(&self) -> &str {
        match self {
            Self::Idle(state) => state.frame_name(),
            Self::Running(state) => state.frame_name(),
            Self::Slide(state) => state.frame_name(),
            Self::Jump(state) => state.frame_name(),
        }
    }

    fn context(&self) -> &RedHatBoyContext {
        match self {
            Self::Idle(state) => &state.context(),
            Self::Running(state) => &state.context(),
            Self::Slide(state) => &state.context(),
            Self::Jump(state) => &state.context(),
        }
    }

    fn update(self) -> Self {
        self.transition(Event::Update)
    }
}

impl From<RedHatBoyState<Idle>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<Idle>) -> Self {
        RedHatBoyStateMachine::Idle(state)
    }
}

impl From<RedHatBoyState<Running>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<Running>) -> Self {
        RedHatBoyStateMachine::Running(state)
    }
}

impl From<RedHatBoyState<Slide>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<Slide>) -> Self {
        RedHatBoyStateMachine::Slide(state)
    }
}

impl From<RedHatBoyState<Jump>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<Jump>) -> Self {
        RedHatBoyStateMachine::Jump(state)
    }
}

impl From<SlidingEndState> for RedHatBoyStateMachine {
    fn from(end_state: SlidingEndState) -> Self {
        match end_state {
            SlidingEndState::Complete(running_state) => running_state.into(),
            SlidingEndState::Sliding(sliding_state) => sliding_state.into(),
        }
    }
}

impl From<JumpingEndState> for RedHatBoyStateMachine {
    fn from(end_state: JumpingEndState) -> Self {
        match end_state {
            JumpingEndState::Complete(running_state) => running_state.into(),
            JumpingEndState::Jumping(jumping_state) => jumping_state.into(),
        }
    }
}

mod red_hat_boyd_states {
    use super::{
        engine::Point, Idle, Jump, Running, Slide, IDLE_FRAMES, JUMP_FRAMES, RUNNING_FRAMES,
        SLIDE_FRAMES,
    };
    const FLOOR: i16 = 475;
    const IDLE_FRAME_NAME: &str = "Idle";
    const RUN_FRAME_NAME: &str = "Run";
    const SLIDE_FRAME_NAME: &str = "Slide";
    const JUMP_FRAME_NAME: &str = "Jump";
    const RUNNING_SPEED: i16 = 3;
    const JUMP_SPEED: i16 = -17;
    const GRAVITY: i16 = 1;

    #[derive(Copy, Clone)]
    pub struct RedHatBoyContext {
        pub frame: u8,
        pub position: Point,
        pub velocity: Point,
    }

    impl RedHatBoyContext {
        pub fn update(mut self, frame_count: u8) -> Self {
            self.velocity.y += GRAVITY;

            if self.frame < frame_count {
                self.frame += 1;
            } else {
                self.frame = 0;
            }

            self.position.x += self.velocity.x;
            self.position.y += self.velocity.y;
            if self.position.y > FLOOR {
                self.position.y = FLOOR;
                self.velocity.y = 0;
            }

            self
        }

        fn reset_frame(mut self) -> Self {
            self.frame = 0;
            self
        }

        fn run_right(mut self) -> Self {
            self.velocity.x += RUNNING_SPEED;
            self
        }

        fn set_vertical_velocity(mut self, y: i16) -> Self {
            self.velocity.y += y;
            self
        }
    }

    #[derive(Copy, Clone)]
    pub struct RedHatBoyState<S> {
        context: RedHatBoyContext,
        _state: S,
    }

    impl<S> RedHatBoyState<S> {
        pub fn context(&self) -> &RedHatBoyContext {
            &self.context
        }
    }

    impl RedHatBoyState<Idle> {
        pub fn new() -> Self {
            RedHatBoyState {
                context: RedHatBoyContext {
                    frame: 0,
                    position: Point { x: 0, y: FLOOR },
                    velocity: Point { x: 0, y: 0 },
                },
                _state: Idle {},
            }
        }

        pub fn frame_name(&self) -> &str {
            IDLE_FRAME_NAME
        }

        pub fn run(self) -> RedHatBoyState<Running> {
            RedHatBoyState {
                context: self.context.reset_frame().run_right(),
                _state: Running {},
            }
        }

        pub fn update(mut self) -> Self {
            self.context = self.context.update(IDLE_FRAMES);
            self
        }
    }

    impl RedHatBoyState<Running> {
        pub fn frame_name(&self) -> &str {
            RUN_FRAME_NAME
        }

        pub fn update(mut self) -> Self {
            self.context = self.context.update(RUNNING_FRAMES);
            self
        }

        pub fn slide(self) -> RedHatBoyState<Slide> {
            RedHatBoyState {
                context: self.context.reset_frame(),
                _state: Slide {},
            }
        }

        pub fn jump(self) -> RedHatBoyState<Jump> {
            RedHatBoyState {
                context: self.context.set_vertical_velocity(JUMP_SPEED).reset_frame(),
                _state: Jump {},
            }
        }
    }

    impl RedHatBoyState<Slide> {
        pub fn frame_name(&self) -> &str {
            SLIDE_FRAME_NAME
        }

        pub fn stand(self) -> RedHatBoyState<Running> {
            RedHatBoyState {
                context: self.context.reset_frame(),
                _state: Running,
            }
        }

        pub fn update(mut self) -> SlidingEndState {
            self.context = self.context.update(SLIDE_FRAMES);

            if self.context.frame >= SLIDE_FRAMES {
                SlidingEndState::Complete(self.stand())
            } else {
                SlidingEndState::Sliding(self)
            }
        }
    }

    impl RedHatBoyState<Jump> {
        pub fn frame_name(&self) -> &str {
            JUMP_FRAME_NAME
        }

        pub fn land(self) -> RedHatBoyState<Running> {
            RedHatBoyState {
                context: self.context.reset_frame(),
                _state: Running,
            }
        }

        pub fn update(mut self) -> JumpingEndState {
            self.context = self.context.update(JUMP_FRAMES);

            if self.context.frame >= JUMP_FRAMES {
                JumpingEndState::Complete(self.land())
            } else {
                JumpingEndState::Jumping(self)
            }
        }
    }

    pub enum SlidingEndState {
        Complete(RedHatBoyState<Running>),
        Sliding(RedHatBoyState<Slide>),
    }
    pub enum JumpingEndState {
        Complete(RedHatBoyState<Running>),
        Jumping(RedHatBoyState<Jump>),
    }
}
