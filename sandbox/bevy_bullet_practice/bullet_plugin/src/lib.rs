use bevy::window::*;
use bevy::{core::FrameCount, prelude::*};

mod asset;
mod component;
mod event;
mod runner;

use asset::bulletml::{BulletMLAsset, BulletMLAssetLoader};
use component::bullet::Bullet;
use component::{
    runner::{BulletRunner, SpawnRunner},
    spawner::Spawner,
};
use event::SpawnBulletEvent;

#[derive(Component)]
struct MainCamera;

#[derive(Resource, Default)]
struct Store {
    texture_atlas_handle: Handle<TextureAtlas>,
    bullet_ml_handle: Handle<BulletMLAsset>,
    target_transform: Transform,
    rank: f32,
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
enum AppState {
    #[default]
    Loading,
    InGame,
}

pub struct BulletPlugin;

impl Plugin for BulletPlugin {
    fn build(&self, app: &mut App) {
        app.add_state::<AppState>();
        app.init_resource::<Store>();
        app.add_asset::<BulletMLAsset>();
        app.init_asset_loader::<BulletMLAssetLoader>();
        app.add_event::<SpawnBulletEvent>();
        app.add_systems((
            load_asset.on_startup(),
            loading.in_set(OnUpdate(AppState::Loading)),
            spawn_system_bundles.in_schedule(OnEnter(AppState::InGame)),
        ));
        app.add_systems(
            (
                move_bullet,
                spawn_bullet,
                update_bullet,
                update_target_transform,
                event_listener,
            )
                .in_set(OnUpdate(AppState::InGame)),
        );
    }
}

fn loading(
    store: Res<Store>,
    bulletml_asset: Res<Assets<BulletMLAsset>>,
    mut next_state: ResMut<NextState<AppState>>,
) {
    if bulletml_asset.get(&store.bullet_ml_handle).is_some() {
        next_state.set(AppState::InGame);
    }
}

fn spawn_system_bundles(
    mut commands: Commands,
    store: Res<Store>,
    bulletml_asset: Res<Assets<BulletMLAsset>>,
) {
    let bml = bulletml_asset.get(&store.bullet_ml_handle).unwrap();
    let runner = bulletml::Runner::new(runner::SpawnRunner::new(), bml);

    commands.spawn((Camera2dBundle::default(), MainCamera));
    commands.spawn((
        Spawner {},
        SpawnRunner {
            bml_handle: store.bullet_ml_handle.clone(),
            runner,
        },
        Transform::from_xyz(0.0, 300.0, 0.0),
    ));
}

fn update_target_transform(
    mut state: ResMut<Store>,
    window_q: Query<&mut Window>,
    camera_q: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
) {
    let (camera, camera_transform) = camera_q.single();
    state.target_transform = window_q
        .single()
        .cursor_position()
        .and_then(|view_pos| camera.viewport_to_world(camera_transform, view_pos))
        .map(|ray| ray.origin.truncate())
        .map(|pos| Transform::from_xyz(pos.x, pos.y, 0.0))
        .unwrap_or(state.target_transform);
}

fn spawn_bullet(
    frame_count: Res<FrameCount>,
    store: Res<Store>,
    bulletml_asset: ResMut<Assets<BulletMLAsset>>,
    mut spawner_q: Query<(Entity, &Spawner, &mut SpawnRunner, &Transform)>,
    mut spawn_event_wtr: EventWriter<SpawnBulletEvent>,
) {
    for (entity, _, mut runner, spawner_transform) in spawner_q.iter_mut() {
        let bml = match bulletml_asset.get(&runner.bml_handle) {
            Some(bml) => bml,
            None => continue,
        };

        runner.runner.run(&mut bulletml::RunnerData {
            bml,
            data: &mut runner::SpawnRunnerData {
                entity,
                spawn_event_wtr: &mut spawn_event_wtr,
                spawn_transform: *spawner_transform,
                target_transform: store.target_transform,
                frame_count: frame_count.0,
                rank: store.rank,
            },
        });
    }
}

fn update_bullet(
    frame_count: Res<FrameCount>,
    store: Res<Store>,
    bulletml_asset: ResMut<Assets<BulletMLAsset>>,
    mut bullet_q: Query<(Entity, &mut Bullet, &mut BulletRunner, &Transform)>,
    mut spawn_event_wtr: EventWriter<SpawnBulletEvent>,
) {
    for (entity, mut bullet, mut runner, bullet_transform) in bullet_q.iter_mut() {
        let bml = match bulletml_asset.get(&runner.bml_handle) {
            Some(bml) => bml,
            None => continue,
        };

        runner.runner.run(&mut bulletml::RunnerData {
            bml,
            data: &mut runner::BulletRunnerData {
                entity,
                spawn_event_wtr: &mut spawn_event_wtr,
                bullet: &mut bullet,
                bullet_transform: *bullet_transform,
                target_transform: store.target_transform,
                frame_count: frame_count.0,
                rank: store.rank,
            },
        });
    }
}

fn move_bullet(time: Res<Time>, mut query: Query<(&mut Transform, &Bullet)>) {
    for (mut transform, bullet) in query.iter_mut() {
        transform.translation.x += bullet.speed * bullet.direction.cos() * time.delta_seconds();
        transform.translation.y += bullet.speed * bullet.direction.sin() * time.delta_seconds();
    }
}

fn event_listener(
    mut commands: Commands,
    state: Res<Store>,
    mut event_rdr: EventReader<SpawnBulletEvent>,
    mut bullet_q: Query<&mut Bullet>,
) {
    for e in event_rdr.into_iter().cloned() {
        match e {
            SpawnBulletEvent::SimpleBullet {
                direction,
                speed,
                transform,
            } => {
                commands.spawn((
                    SpriteSheetBundle {
                        texture_atlas: state.texture_atlas_handle.clone(),
                        sprite: TextureAtlasSprite::new(0),
                        transform,
                        ..default()
                    },
                    Bullet {
                        speed: speed * 40.0,
                        direction,
                    },
                ));
            }
            SpawnBulletEvent::StatefulBullet {
                direction,
                speed,
                transform,
                bml_state,
            } => {
                let runner =
                    bulletml::Runner::new_from_state(runner::BulletRunner::new(), bml_state);

                commands.spawn((
                    SpriteSheetBundle {
                        texture_atlas: state.texture_atlas_handle.clone(),
                        sprite: TextureAtlasSprite::new(0),
                        transform,
                        ..default()
                    },
                    Bullet {
                        speed: speed * 40.0,
                        direction,
                    },
                    BulletRunner {
                        runner,
                        bml_handle: state.bullet_ml_handle.clone(),
                    },
                ));
            }
            SpawnBulletEvent::ChangeBulletDirection { entity, direction } => {
                if let Ok(mut bullet) = bullet_q.get_mut(entity) {
                    bullet.direction = direction;
                } else {
                    error!("Failed to get bullet entity: {:?}", entity);
                }
            }
            SpawnBulletEvent::ChangeBulletSpeed { entity, speed } => {
                if let Ok(mut bullet) = bullet_q.get_mut(entity) {
                    bullet.speed = speed * 40.0;
                } else {
                    error!("Failed to get bullet entity: {:?}", entity);
                }
            }
            SpawnBulletEvent::VanishBullet { entity } => {
                commands.entity(entity).despawn();
            }
            SpawnBulletEvent::VanishSpawner { entity } => {
                commands.entity(entity).despawn();
            }
        }
    }
}

fn load_asset(
    mut store: ResMut<Store>,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>,
) {
    //state.bullet_ml_handle = asset_server.load("bullet_ml/simple.xml");
    //state.bullet_ml_handle = asset_server.load("bullet_ml/accel.xml");
    //state.bullet_ml_handle = asset_server.load("bullet_ml/grow.xml");
    //store.bullet_ml_handle = asset_server.load("bullet_ml/change_direction.xml");
    //store.bullet_ml_handle = asset_server.load("bullet_ml/spread2blt.xml");
    //store.bullet_ml_handle = asset_server.load("bullet_ml/youmu_hououtenshi_luna_modoki.xml");
    //store.bullet_ml_handle = asset_server.load("bullet_ml/pyramid.xml");
    store.bullet_ml_handle = asset_server.load("bullet_ml/hura.xml");

    let texture_handle = asset_server.load("sprite/bullets.png");
    let texture_atlas =
        TextureAtlas::from_grid(texture_handle, Vec2::new(64.0, 64.0), 3, 1, None, None);
    store.texture_atlas_handle = texture_atlases.add(texture_atlas);
}
