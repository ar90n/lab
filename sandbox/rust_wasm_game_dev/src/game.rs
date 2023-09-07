use std::io::Read;
use std::rc::Rc;

#[macro_use]
use crate::browser;
use crate::sound;

use crate::engine::{self, Audio, Cell, Image, Sheet, Sound, SpriteSheet};
use crate::engine::{Game, KeyState, Point, Rect, Renderer};
use crate::segments::{platform_and_stone, stone_and_platform};
use anyhow::Result;
use async_trait::async_trait;
use rand::prelude::*;
use web_sys::HtmlImageElement;

use anyhow::anyhow;

use self::red_hat_boyd_states::*;

pub trait Obstacle {
    fn check_intersection(&self, boy: &mut RedHatBoy);
    fn draw(&self, renderer: &Renderer);
    fn move_horizontally(&mut self, x: i16);
    fn right(&self) -> i16;
}

struct WalkTheDogState<T> {
    _state: T,
    walk: Walk,
}

struct Ready;
struct Walking;
struct GameOver;

enum WalkTheDogStateMachine {
    Ready(WalkTheDogState<Ready>),
    Walking(WalkTheDogState<Walking>),
    GameOver(WalkTheDogState<GameOver>),
}

pub struct WalkTheDog {
    machine: Option<WalkTheDogStateMachine>,
}

impl WalkTheDog {
    pub fn new() -> Self {
        WalkTheDog { machine: None }
    }
}

pub struct Walk {
    obstacle_sheet: Rc<SpriteSheet>,
    boy: RedHatBoy,
    backgrounds: [Image; 2],
    obstacles: Vec<Box<dyn Obstacle>>,
    stone: HtmlImageElement,
    timeline: i16,
}

impl Walk {
    fn velocity(&self) -> i16 {
        -self.boy.waling_speed()
    }

    fn generate_next_segment(&mut self) {
        let mut rng = thread_rng();
        let next_segment = rng.gen_range(0..2);
        let mut next_obstacles = match next_segment {
            0 => stone_and_platform(
                self.stone.clone(),
                self.obstacle_sheet.clone(),
                self.timeline + OBSTACLE_BUFFER,
            ),
            1 => platform_and_stone(
                self.stone.clone(),
                self.obstacle_sheet.clone(),
                self.timeline + OBSTACLE_BUFFER,
            ),
            _ => vec![],
        };

        self.timeline = rightmost(&next_obstacles);
        self.obstacles.append(&mut next_obstacles);
    }
}

const FLOOR: i16 = 479;
const LOW_PLATFORM: i16 = 420;
const HIGH_PLATFORM: i16 = 375;
const FIRST_PLATFORM: i16 = 300;
const TIMELINE_MINIUM: i16 = 1000;
const OBSTACLE_BUFFER: i16 = 20;

fn rightmost(obstacle_list: &Vec<Box<dyn Obstacle>>) -> i16 {
    obstacle_list
        .iter()
        .map(|obstacle| obstacle.right())
        .max_by(|x, y| x.cmp(&y))
        .unwrap_or(0)
}

#[async_trait(?Send)]
impl Game for WalkTheDog {
    async fn initialize(&self) -> Result<Box<dyn Game>> {
        match self.machine {
            None => {
                let sheet = browser::fetch_json("rhb.json").await?;
                let background = engine::load_image("BG.png").await?;
                let stone = engine::load_image("Stone.png").await?;

                let paltform_sheet = browser::fetch_json("tiles.json").await?;
                let sprite_sheet = Rc::new(SpriteSheet::new(
                    paltform_sheet.into_serde::<Sheet>()?,
                    engine::load_image("tiles.png").await?,
                ));
                let audio = Audio::new()?;
                let sound = audio.load_sound("SFX_Jump_23.mp3").await?;
                let background_music = audio.load_sound("background_song.mp3").await?;

                audio.play_looping_sound(&background_music)?;
                let rhb = RedHatBoy::new(
                    sheet.into_serde::<Sheet>()?,
                    engine::load_image("rhb.png").await?,
                    audio,
                    sound,
                );
                let background_width = background.width() as i16;
                let starting_obstacles = stone_and_platform(stone.clone(), sprite_sheet.clone(), 0);
                let timeline = rightmost(&starting_obstacles);
                let machine = WalkTheDogStateMachine::Ready(WalkTheDogState {
                    _state: Read,
                    walk: Walk {
                        boy: rhb,
                        backgrounds: [
                            Image::new(background.clone(), Point { x: 0, y: 0 }),
                            Image::new(
                                background,
                                Point {
                                    x: background_width,
                                    y: 0,
                                },
                            ),
                        ],
                        obstacles: starting_obstacles,
                        obstacle_sheet: sprite_sheet,
                        stone,
                        timeline,
                    },
                });

                Ok(Box::new(Self {
                    machine: Some(machine),
                }))
            }
            Some(_) => Err(anyhow!("Error: Game is already initialized!")),
        }
    }

    fn update(&mut self, keystate: &KeyState) {
        if let Some(machine) = self.machine.take() {
            self.machine.replace(machine.update(keystate));
            if keystate.is_pressed("ArrowDown") {
                walk.boy.slide();
            }
            if keystate.is_pressed("ArrowUp") {}
            if keystate.is_pressed("ArrowRight") {
                walk.boy.run_right();
            }
            if keystate.is_pressed("ArrowLeft") {}
            if keystate.is_pressed("Space") {
                walk.boy.jump();
            }
            walk.boy.update();

            let velocity = walk.velocity();
            let [first_background, second_background] = &mut walk.backgrounds;
            first_background.move_horizontally(velocity);
            second_background.move_horizontally(velocity);

            if first_background.right() < 0 {
                first_background.set_x(second_background.right())
            }
            if second_background.right() < 0 {
                second_background.set_x(first_background.right())
            }

            walk.obstacles.retain(|obstacle| obstacle.right() > 0);

            walk.obstacles.iter_mut().for_each(|obstacle| {
                obstacle.move_horizontally(velocity);
                obstacle.check_intersection(&mut walk.boy);
            });

            if walk.timeline < TIMELINE_MINIUM {
                walk.generate_next_segment();
            } else {
                walk.timeline += velocity;
            }
        }
    }

    fn draw(&self, renderer: &Renderer) {
        renderer.clear(&&Rect::new_from_x_y(0, 0, 600, 600));

        if let Self::Loaded(walk) = self {
            walk.backgrounds.iter().for_each(|background| {
                background.draw(renderer);
            });
            walk.boy.draw(renderer);
            walk.obstacles
                .iter()
                .for_each(|obstacle| obstacle.draw(renderer));
        }
    }
}

struct RedHatBoy {
    state_machine: RedHatBoyStateMachine,
    sprite_sheet: Sheet,
    image: HtmlImageElement,
}

impl RedHatBoy {
    fn new(sheet: Sheet, image: HtmlImageElement, audio: Audio, sound: Sound) -> Self {
        RedHatBoy {
            state_machine: RedHatBoyStateMachine::Idle(RedHatBoyState::new(audio, sound)),
            sprite_sheet: sheet,
            image: image,
        }
    }

    fn pos_y(&self) -> i16 {
        self.state_machine.context().position.y
    }

    fn velocity_y(&self) -> i16 {
        self.state_machine.context().velocity.y
    }

    fn waling_speed(&self) -> i16 {
        self.state_machine.context().velocity.x
    }

    fn frame_name(&self) -> String {
        format!(
            "{} ({}).png",
            self.state_machine.frame_name(),
            (self.state_machine.context().frame / 3) + 1
        )
    }

    fn currenct_sprite(&self) -> Option<&Cell> {
        self.sprite_sheet.frames.get(&self.frame_name())
    }

    fn destination_box(&self) -> Rect {
        let sprite = self.currenct_sprite().expect("Cell not found");

        Rect::new_from_x_y(
            (self.state_machine.context().position.x + sprite.sprite_source_size.x as i16).into(),
            (self.state_machine.context().position.y + sprite.sprite_source_size.y as i16).into(),
            sprite.frame.w as i16,
            sprite.frame.h as i16,
        )
    }

    fn bounding_box(&self) -> Rect {
        const X_OFFSET: i16 = 18;
        const Y_OFFSET: i16 = 14;
        const WIDTH_OFFSET: i16 = 28;
        let mut bounding_box = self.destination_box();
        bounding_box.set_x(bounding_box.x() + X_OFFSET);
        bounding_box.width -= WIDTH_OFFSET;
        bounding_box.set_y(bounding_box.y() + Y_OFFSET);
        bounding_box.height -= Y_OFFSET;
        bounding_box
    }

    fn draw(&self, renderer: &Renderer) {
        let sprite = self.currenct_sprite().expect("Cell not found");

        renderer.draw_image(
            &self.image,
            &&Rect::new_from_x_y(
                sprite.frame.x as i16,
                sprite.frame.y as i16,
                sprite.frame.w as i16,
                sprite.frame.h as i16,
            ),
            &self.destination_box(),
        );
        renderer.draw_rect(&self.bounding_box())
    }

    fn update(&mut self) {
        self.state_machine = self.state_machine.clone().update();
    }

    fn run_right(&mut self) {
        self.state_machine = self.state_machine.clone().transition(Event::Run)
    }

    fn slide(&mut self) {
        self.state_machine = self.state_machine.clone().transition(Event::Slide)
    }

    fn jump(&mut self) {
        self.state_machine = self.state_machine.clone().transition(Event::Jump)
    }

    fn knock_out(&mut self) {
        self.state_machine = self.state_machine.clone().transition(Event::KnockOut)
    }

    fn land_on(&mut self, position: i16) {
        self.state_machine = self.state_machine.clone().transition(Event::Land(position))
    }
}

pub struct Platform {
    sheet: Rc<SpriteSheet>,
    bounding_boxes: Vec<Rect>,
    sprites: Vec<Cell>,
    position: Point,
}

impl Obstacle for Platform {
    fn check_intersection(&self, boy: &mut RedHatBoy) {
        if let Some(box_to_land_on) = self
            .bounding_boxes()
            .iter()
            .find(|&bounding_box| boy.bounding_box().intersects(bounding_box))
        {
            if 0 < boy.velocity_y() && boy.pos_y() < self.position.y {
                boy.land_on(box_to_land_on.y());
            } else {
                boy.knock_out();
            }
        }
    }

    fn draw(&self, renderer: &Renderer) {
        let mut x = 0;
        self.sprites.iter().for_each(|sprite| {
            self.sheet.draw(
                renderer,
                &Rect::new_from_x_y(
                    sprite.frame.x,
                    sprite.frame.y,
                    sprite.frame.w,
                    sprite.frame.h,
                ),
                &Rect::new_from_x_y(
                    self.position.x + x,
                    self.position.y,
                    sprite.frame.w,
                    sprite.frame.h,
                ),
            );
            x += sprite.frame.w;
        });
    }

    fn move_horizontally(&mut self, x: i16) {
        self.position.x += x;
        self.bounding_boxes.iter_mut().for_each(|bounding_box| {
            bounding_box.set_x(bounding_box.position.x + x);
        })
    }

    fn right(&self) -> i16 {
        self.bounding_boxes()
            .last()
            .unwrap_or(&Rect::default())
            .right()
    }
}

impl Platform {
    pub fn new(
        sheet: Rc<SpriteSheet>,
        position: Point,
        sprite_names: &[&str],
        bounding_boxes: &[Rect],
    ) -> Self {
        let sprites = sprite_names
            .iter()
            .filter_map(|sprite_names| sheet.cell(sprite_names).cloned())
            .collect();
        let bounding_boxes = bounding_boxes
            .iter()
            .map(|bounding_box| {
                Rect::new_from_x_y(
                    bounding_box.x() + position.x,
                    bounding_box.y() + position.y,
                    bounding_box.width,
                    bounding_box.height,
                )
            })
            .collect();
        Self {
            sheet,
            bounding_boxes,
            sprites,
            position,
        }
    }

    fn destination_box(&self) -> Rect {
        let platform = self.sheet.cell("13.png").expect("13.png does not exist");
        Rect::new(
            self.position,
            (platform.frame.w * 3) as i16,
            platform.frame.h as i16,
        )
    }

    fn bounding_boxes(&self) -> &Vec<Rect> {
        &self.bounding_boxes
    }
}

pub struct Barrier {
    image: Image,
}

impl Obstacle for Barrier {
    fn check_intersection(&self, boy: &mut RedHatBoy) {
        //if boy.bounding_box().intersects(self.image.bounding_box()) {
        //    boy.knock_out()
        //}
    }

    fn draw(&self, renderer: &Renderer) {
        self.image.draw(renderer)
    }

    fn move_horizontally(&mut self, x: i16) {
        self.image.move_horizontally(x)
    }

    fn right(&self) -> i16 {
        self.image.bounding_box().right()
    }
}

impl Barrier {
    pub fn new(image: Image) -> Self {
        Self { image }
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
struct Falling;
#[derive(Copy, Clone)]
struct KnockOut;

#[derive(Clone)]
enum RedHatBoyStateMachine {
    Idle(RedHatBoyState<Idle>),
    Running(RedHatBoyState<Running>),
    Slide(RedHatBoyState<Slide>),
    Jump(RedHatBoyState<Jump>),
    Falling(RedHatBoyState<Falling>),
    KnockOut(RedHatBoyState<KnockOut>),
}

pub enum Event {
    Run,
    Slide,
    Update,
    KnockOut,
    Jump,
    Land(i16),
}

const HEIGHT: i16 = 600;
const IDLE_FRAMES: u8 = 29;
const RUNNING_FRAMES: u8 = 23;
const SLIDE_FRAMES: u8 = 14;
const JUMP_FRAMES: u8 = 35;
const FALLING_FRAMES: u8 = 29;

impl RedHatBoyStateMachine {
    fn transition(self, event: Event) -> Self {
        match (self.clone(), event) {
            (Self::Idle(state), Event::Run) => state.run().into(),
            (Self::Running(state), Event::Slide) => state.slide().into(),
            (Self::Running(state), Event::Jump) => state.jump().into(),
            (Self::Idle(state), Event::Update) => state.update().into(),
            (Self::Running(state), Event::Update) => state.update().into(),
            (Self::Slide(state), Event::Update) => state.update().into(),
            (Self::Jump(state), Event::Update) => state.update().into(),
            (Self::Falling(state), Event::Update) => state.update().into(),
            (Self::KnockOut(state), Event::Update) => state.update().into(),
            (Self::Running(state), Event::KnockOut) => state.knock_out().into(),
            (Self::Slide(state), Event::KnockOut) => state.knock_out().into(),
            (Self::Jump(state), Event::KnockOut) => state.knock_out().into(),
            (Self::Jump(state), Event::Land(position)) => state.land_on(position).into(),
            (Self::Running(state), Event::Land(position)) => state.land_on(position).into(),
            _ => self,
        }
    }

    fn frame_name(&self) -> &str {
        match self {
            Self::Idle(state) => state.frame_name(),
            Self::Running(state) => state.frame_name(),
            Self::Slide(state) => state.frame_name(),
            Self::Jump(state) => state.frame_name(),
            Self::Falling(state) => state.frame_name(),
            Self::KnockOut(state) => state.frame_name(),
        }
    }

    fn context(&self) -> &RedHatBoyContext {
        match self {
            Self::Idle(state) => &state.context(),
            Self::Running(state) => &state.context(),
            Self::Slide(state) => &state.context(),
            Self::Jump(state) => &state.context(),
            Self::Falling(state) => &state.context(),
            Self::KnockOut(state) => &state.context(),
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

impl From<RedHatBoyState<Falling>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<Falling>) -> Self {
        RedHatBoyStateMachine::Falling(state)
    }
}

impl From<RedHatBoyState<KnockOut>> for RedHatBoyStateMachine {
    fn from(state: RedHatBoyState<KnockOut>) -> Self {
        RedHatBoyStateMachine::KnockOut(state)
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

impl From<FallingEndState> for RedHatBoyStateMachine {
    fn from(end_state: FallingEndState) -> Self {
        match end_state {
            FallingEndState::KnockOut(knock_out_state) => knock_out_state.into(),
            FallingEndState::Falling(falling_state) => falling_state.into(),
        }
    }
}

mod red_hat_boyd_states {
    use super::{
        engine::Point,
        engine::{Audio, Sound},
        Falling, Idle, Jump, KnockOut, Running, Slide, FALLING_FRAMES, FLOOR, HEIGHT, IDLE_FRAMES,
        JUMP_FRAMES, RUNNING_FRAMES, SLIDE_FRAMES,
    };
    const PLAYER_HEIGHT: i16 = HEIGHT - FLOOR;
    const STARTING_POINT: i16 = -20;
    const IDLE_FRAME_NAME: &str = "Idle";
    const RUN_FRAME_NAME: &str = "Run";
    const SLIDE_FRAME_NAME: &str = "Slide";
    const JUMP_FRAME_NAME: &str = "Jump";
    const FALLING_FRAME_NAME: &str = "Dead";
    const RUNNING_SPEED: i16 = 3;
    const JUMP_SPEED: i16 = -17;
    const GRAVITY: i16 = 1;
    const TERMINAL_VELOCITY: i16 = 20;

    #[derive(Clone)]
    pub struct RedHatBoyContext {
        pub frame: u8,
        pub position: Point,
        pub velocity: Point,
        audio: Audio,
        jump_sound: Sound,
    }

    impl RedHatBoyContext {
        pub fn update(mut self, frame_count: u8) -> Self {
            self.velocity.y += GRAVITY;
            if TERMINAL_VELOCITY <= self.velocity.y {
                self.velocity.y = TERMINAL_VELOCITY
            }

            if self.frame < frame_count {
                self.frame += 1;
            } else {
                self.frame = 0;
            }

            //self.position.x += self.velocity.x;
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

        fn stop(mut self) -> Self {
            self.velocity.x = 0;
            self
        }

        fn set_on(mut self, position: i16) -> Self {
            let position = position - PLAYER_HEIGHT;
            self.position.y = position;
            self
        }

        fn play_jump_sound(self) -> Self {
            self.audio.play_sound(&self.jump_sound);
            self
        }
    }

    #[derive(Clone)]
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
        pub fn new(audio: Audio, jump_sound: Sound) -> Self {
            RedHatBoyState {
                context: RedHatBoyContext {
                    frame: 0,
                    position: Point {
                        x: STARTING_POINT,
                        y: FLOOR,
                    },
                    velocity: Point { x: 0, y: 0 },
                    audio,
                    jump_sound,
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

        pub fn land_on(self, position: i16) -> RedHatBoyState<Running> {
            RedHatBoyState {
                context: self.context.set_on(position),
                _state: Running,
            }
        }

        pub fn slide(self) -> RedHatBoyState<Slide> {
            RedHatBoyState {
                context: self.context.reset_frame(),
                _state: Slide {},
            }
        }

        pub fn jump(self) -> RedHatBoyState<Jump> {
            RedHatBoyState {
                context: self
                    .context
                    .set_vertical_velocity(JUMP_SPEED)
                    .reset_frame()
                    .play_jump_sound(),
                _state: Jump {},
            }
        }

        pub fn knock_out(mut self) -> RedHatBoyState<Falling> {
            RedHatBoyState {
                context: self.context.reset_frame().stop(),
                _state: Falling {},
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

        pub fn knock_out(mut self) -> RedHatBoyState<Falling> {
            RedHatBoyState {
                context: self.context,
                _state: Falling {},
            }
        }
    }

    impl RedHatBoyState<Jump> {
        pub fn frame_name(&self) -> &str {
            JUMP_FRAME_NAME
        }

        pub fn land_on(self, position: i16) -> RedHatBoyState<Running> {
            RedHatBoyState {
                context: self.context.reset_frame().set_on(position),
                _state: Running,
            }
        }

        pub fn update(mut self) -> JumpingEndState {
            self.context = self.context.update(JUMP_FRAMES);

            if self.context.frame >= JUMP_FRAMES {
                JumpingEndState::Complete(self.land_on(HEIGHT.into()))
            } else {
                JumpingEndState::Jumping(self)
            }
        }

        pub fn knock_out(self) -> RedHatBoyState<Falling> {
            RedHatBoyState {
                context: self.context,
                _state: Falling {},
            }
        }
    }

    impl RedHatBoyState<Falling> {
        pub fn frame_name(&self) -> &str {
            FALLING_FRAME_NAME
        }

        pub fn update(mut self) -> FallingEndState {
            self.context = self.context.update(FALLING_FRAMES);

            if self.context.frame >= FALLING_FRAMES {
                FallingEndState::KnockOut(self.knock_out())
            } else {
                FallingEndState::Falling(self)
            }
        }

        pub fn knock_out(mut self) -> RedHatBoyState<KnockOut> {
            RedHatBoyState {
                context: self.context.stop(),
                _state: KnockOut {},
            }
        }
    }

    impl RedHatBoyState<KnockOut> {
        pub fn frame_name(&self) -> &str {
            FALLING_FRAME_NAME
        }

        pub fn update(mut self) -> Self {
            self.context = self.context.update(FALLING_FRAMES);
            self.context.frame = FALLING_FRAMES - 1;
            self
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
    pub enum FallingEndState {
        KnockOut(RedHatBoyState<KnockOut>),
        Falling(RedHatBoyState<Falling>),
    }
}
