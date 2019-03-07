#pushd common
#sudo singularity build --force ../images/common.simg Singularity
#popd
#
pushd python
sudo singularity build --force ../images/python.simg Singularity
popd

#pushd reasonml
#sudo singularity build --force ../images/reasonml.simg Singularity
#popd
#
#pushd rust
#sudo singularity build --force ../images/rust.simg Singularity
#popd
#
#pushd vnc
#sudo singularity build --force ../images/vnc.simg Singularity
#popd
