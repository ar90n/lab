use bevy::prelude::*;

use crate::asset::bulletml::BulletMLAsset;
use crate::runner;

#[derive(Component)]
pub struct BulletRunner {
    pub bml_handle: Handle<BulletMLAsset>,
    pub runner: bulletml::Runner<runner::BulletRunner>,
}

#[derive(Component)]
pub struct SpawnRunner {
    pub bml_handle: Handle<BulletMLAsset>,
    pub runner: bulletml::Runner<runner::SpawnRunner>,
}
