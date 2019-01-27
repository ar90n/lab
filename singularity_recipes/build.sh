#pushd dev_env
#sudo singularity build ../images/dev_env.simg Singularity
#popd

#pushd python
#sudo singularity build ../images/python.simg Singularity
#popd

#pushd node
#sudo singularity build ../images/node.simg Singularity
#popd

pushd rust
sudo singularity build ../images/rust.simg Singularity
popd

#pushd tools
#sudo singularity build ../images/tools.simg Singularity
#popd
