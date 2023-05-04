use bevy::prelude::*;

#[derive(Component, Default)]
pub struct Bullet {
    pub direction: Direction,
    pub speed: f32,
}

#[derive(Debug, Clone, Copy, Deref, DerefMut)]
pub struct Direction(f32);

impl Default for Direction {
    fn default() -> Self {
        Self(0.0)
    }
}

impl Direction {
    pub fn into_deg(self) -> f64 {
        self.0 as f64 * 180.0 / std::f64::consts::PI
    }

    pub fn into_rad(self) -> f64 {
        self.0 as f64
    }

    pub fn from_deg(deg: f64) -> Self {
        Self::from_rad(deg * std::f64::consts::PI / 180.0)
    }

    pub fn from_rad(rad: f64) -> Self {
        Self(rad as f32)
    }
}
