use bevy::prelude::*;

use crate::component::bullet::Direction;


#[derive(Debug, Clone)]
pub enum SpawnBulletEvent {
    SimpleBullet {
        direction: Direction,
        speed: f32,
        transform: Transform,
    },

    StatefulBullet {
        direction: Direction,
        speed: f32,
        transform: Transform,
        bml_state: bulletml::State,
    },

    ChangeBulletDirection {
        entity: Entity,
        direction: Direction,
    },   

    ChangeBulletSpeed {
        entity: Entity,
        speed: f32,
    },

    VanishBullet {
        entity: Entity,
    },

    VanishSpawner {
        entity: Entity,
    },
}
