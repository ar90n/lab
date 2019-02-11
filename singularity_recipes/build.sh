pushd common
sudo singularity build --force ../images/common.simg Singularity
popd

pushd python
sudo singularity build --force ../images/python.simg Singularity
popd

pushd node
sudo singularity build --force ../images/node.simg Singularity
popd

pushd rust
sudo singularity build ../images/rust.simg Singularity
popd

pushd tools
sudo singularity build ../images/tools.simg Singularity
popd

pushd vnc
sudo singularity build ../images/vnc.simg Singularity
popd
