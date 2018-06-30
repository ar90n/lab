const applyAssetPath = (cover, size) => {
    return `/assets/img/${size}/${cover}.jpg`;
}

const getCoverPath = (cover, size='middle') => {
  if(!cover) {
    return applyAssetPath('general', size);
  }

  return cover.match(/^https?:\/\//) ? cover : applyAssetPath(cover, size)
};

export {
  getCoverPath
};
