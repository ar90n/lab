pushd common
sudo singularity build --force ../images/common.simg Singularity
popd

pushd python
sed 's/###PYTHON_VERSION###/3.6/' Singularity.template > Singularity.tmp
sudo singularity build --force ../images/python36.simg Singularity.tmp
sed 's/###PYTHON_VERSION###/3.8/' Singularity.template > Singularity.tmp
sudo singularity build --force ../images/python38.simg Singularity.tmp
sed 's/###PYTHON_VERSION###/2.7/' Singularity.template > Singularity.tmp
sudo singularity build --force ../images/python27.simg Singularity.tmp
popd

pushd node
sed 's/###NODE_VERSION###/12.13.0/' Singularity.template > Singularity.tmp
sudo singularity build --force ../images/node12_13.simg Singularity.tmp
popd


pushd aws
sudo singularity build --force ../images/aws.simg Singularity
popd

pushd procon
sudo singularity build --force ../images/procon.simg Singularity
popd

pushd rust
sudo singularity build --force ../images/rust.simg Singularity
popd

pushd vnc
sudo singularity build --force ../images/vnc.simg Singularity
popd
