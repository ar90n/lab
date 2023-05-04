use bevy::{
    asset::{AssetLoader, LoadContext, LoadedAsset},
    prelude::*,
    reflect::TypeUuid,
    utils::BoxedFuture,
};

#[derive(Debug, TypeUuid, Deref)]
#[uuid = "39cadc57-aa9c-4543-8640-a018b74b5052"]
pub struct BulletMLAsset(bulletml::BulletML);

#[derive(Default)]
pub struct BulletMLAssetLoader;

impl AssetLoader for BulletMLAssetLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), bevy::asset::Error>> {
        Box::pin(async move {
            let parser = bulletml::parse::BulletMLParser::new();
            let xml_str = std::str::from_utf8(bytes)?;
            let bulletml_asset = parser.parse(xml_str).map(BulletMLAsset)?;
            load_context.set_default_asset(LoadedAsset::new(bulletml_asset));
            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        &["xml", "bml"]
    }
}
