use bevy::prelude::Component;

#[cfg_attr(feature="debug", derive(bevy_inspector_egui::Inspectable))]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Component)]
pub struct BombNeighbor{
    pub count: u8,
}