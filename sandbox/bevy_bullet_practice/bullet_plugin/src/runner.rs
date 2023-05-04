use core::panic;

use bevy::prelude::*;

use bulletml::AppRunner;

use crate::component::bullet::{Bullet, Direction};
use crate::event::SpawnBulletEvent;

fn calc_direction(origin_transform: &Transform, target_transform: &Transform) -> Direction {
    let diff_x = target_transform.translation.x - origin_transform.translation.x;
    let diff_y = target_transform.translation.y - origin_transform.translation.y;
    Direction::from_rad(diff_y.atan2(diff_x) as f64)
}

pub struct BulletRunnerData<'a, 'w> {
    pub entity: Entity,
    pub spawn_event_wtr: &'a mut EventWriter<'w, SpawnBulletEvent>,
    pub bullet: &'a mut Bullet,
    pub bullet_transform: Transform,
    pub target_transform: Transform,
    pub frame_count: u32,
    pub rank: f32,
}
pub struct BulletRunner {}

impl BulletRunner {
    pub fn new() -> Self {
        BulletRunner {}
    }
}

impl Default for BulletRunner {
    fn default() -> Self {
        Self::new()
    }
}

impl AppRunner<BulletRunnerData<'_, '_>> for BulletRunner {
    fn get_bullet_direction(&self, data: &BulletRunnerData) -> f64 {
        data.bullet.direction.into_deg()
    }

    fn get_aim_direction(&self, data: &BulletRunnerData) -> f64 {
        calc_direction(&data.bullet_transform, &data.target_transform).into_deg()
    }

    fn get_bullet_speed(&self, data: &BulletRunnerData) -> f64 {
        data.bullet.speed.into()
    }

    fn get_default_speed(&self) -> f64 {
        100.0
    }

    fn get_rank(&self, data: &BulletRunnerData) -> f64 {
        data.rank.into()
    }

    fn create_simple_bullet(&mut self, data: &mut BulletRunnerData, direction: f64, speed: f64) {
        data.spawn_event_wtr.send(SpawnBulletEvent::SimpleBullet {
            direction: Direction::from_deg(direction),
            speed: speed as f32,
            transform: data.bullet_transform,
        });
    }

    fn create_bullet(
        &mut self,
        data: &mut BulletRunnerData,
        state: bulletml::State,
        direction: f64,
        speed: f64,
    ) {
        data.spawn_event_wtr.send(SpawnBulletEvent::StatefulBullet {
            direction: Direction::from_deg(direction),
            speed: speed as f32,
            bml_state: state,
            transform: data.bullet_transform,
        });
    }

    fn get_turn(&self, data: &BulletRunnerData) -> u32 {
        data.frame_count
    }

    fn do_vanish(&mut self, data: &mut BulletRunnerData) {
        data.spawn_event_wtr.send(SpawnBulletEvent::VanishBullet {
            entity: data.entity,
        });
    }

    fn do_change_direction(&mut self, data: &mut BulletRunnerData, direction: f64) {
        data.spawn_event_wtr
            .send(SpawnBulletEvent::ChangeBulletDirection {
                entity: data.entity,
                direction: Direction::from_deg(direction),
            });
    }

    fn do_change_speed(&mut self, data: &mut BulletRunnerData, speed: f64) {
        data.spawn_event_wtr
            .send(SpawnBulletEvent::ChangeBulletSpeed {
                entity: data.entity,
                speed: speed as f32,
            });
    }

    fn do_accel_x(&mut self, _: f64) {
        panic!("do_accel_x");
    }

    fn do_accel_y(&mut self, _: f64) {
        panic!("do_accel_y");
    }

    fn get_bullet_speed_x(&self) -> f64 {
        panic!("get_bullet_speed_x");
    }
    fn get_bullet_speed_y(&self) -> f64 {
        panic!("get_bullet_speed_y");
    }

    fn get_rand(&self, _data: &mut BulletRunnerData) -> f64 {
        rand::random()
    }
}

pub struct SpawnRunnerData<'a, 'w> {
    pub entity: Entity,
    pub spawn_event_wtr: &'a mut EventWriter<'w, SpawnBulletEvent>,
    pub spawn_transform: Transform,
    pub target_transform: Transform,
    pub frame_count: u32,
    pub rank: f32,
}
pub struct SpawnRunner {}

impl SpawnRunner {
    pub fn new() -> Self {
        SpawnRunner {}
    }
}

impl Default for SpawnRunner {
    fn default() -> Self {
        Self::new()
    }
}

impl AppRunner<SpawnRunnerData<'_, '_>> for SpawnRunner {
    fn get_bullet_direction(&self, _data: &SpawnRunnerData) -> f64 {
        panic!("get_bullet_direction is called by SpawnRunner");
    }

    fn get_aim_direction(&self, data: &SpawnRunnerData) -> f64 {
        calc_direction(&data.spawn_transform, &data.target_transform).into_deg()
    }

    fn get_bullet_speed(&self, _data: &SpawnRunnerData) -> f64 {
        panic!("get_bullet_speed is called by SpawnRunner")
    }

    fn get_default_speed(&self) -> f64 {
        100.0
    }

    fn get_rank(&self, data: &SpawnRunnerData) -> f64 {
        data.rank.into()
    }

    fn create_simple_bullet(&mut self, data: &mut SpawnRunnerData, direction: f64, speed: f64) {
        data.spawn_event_wtr.send(SpawnBulletEvent::SimpleBullet {
            direction: Direction::from_deg(direction),
            speed: speed as f32,
            transform: data.spawn_transform,
        });
    }

    fn create_bullet(
        &mut self,
        data: &mut SpawnRunnerData,
        state: bulletml::State,
        direction: f64,
        speed: f64,
    ) {
        data.spawn_event_wtr.send(SpawnBulletEvent::StatefulBullet {
            direction: Direction::from_deg(direction),
            speed: speed as f32,
            bml_state: state,
            transform: data.spawn_transform,
        });
    }

    fn get_turn(&self, data: &SpawnRunnerData) -> u32 {
        data.frame_count
    }

    fn do_vanish(&mut self, data: &mut SpawnRunnerData) {
        data.spawn_event_wtr.send(SpawnBulletEvent::VanishSpawner {
            entity: data.entity,
        });
    }

    fn do_change_direction(&mut self, _data: &mut SpawnRunnerData, _direction: f64) {
        panic!("do_change_direction is called by SpawnRunner");
    }

    fn do_change_speed(&mut self, _data: &mut SpawnRunnerData, _speed: f64) {
        panic!("do_change_speed is called by SpawnRunner");
    }

    fn do_accel_x(&mut self, _: f64) {
        panic!("do_accel_x is called by SpawnRunner");
    }

    fn do_accel_y(&mut self, _: f64) {
        panic!("do_accel_y is called by SpawnRunner");
    }

    fn get_bullet_speed_x(&self) -> f64 {
        panic!("get_bullet_speed_x is called by SpawnRunner");
    }
    fn get_bullet_speed_y(&self) -> f64 {
        panic!("get_bullet_speed_y is called by SpawnRunner");
    }

    fn get_rand(&self, _data: &mut SpawnRunnerData) -> f64 {
        rand::random()
    }
}
