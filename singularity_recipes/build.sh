pushd common
sudo singularity build --force ../images/common.simg Singularity
popd

pushd python
sed 's/###PYTHON_VERSION###/3.6/' Singularity.template | sed -e 's/###SSHD_PORT###/3000/' > Singularity.tmp
sudo singularity build --force ../images/python36.simg Singularity.tmp
sed 's/###PYTHON_VERSION###/3.7/' Singularity.template | sed -e 's/###SSHD_PORT###/3001/' > Singularity.tmp
sudo singularity build --force ../images/python37.simg Singularity.tmp
sed 's/###PYTHON_VERSION###/3.8/' Singularity.template | sed -e 's/###SSHD_PORT###/3002/' > Singularity.tmp
sudo singularity build --force ../images/python38.simg Singularity.tmp
sed 's/###PYTHON_VERSION###/2.7/' Singularity.template | sed -e 's/###SSHD_PORT###/3003/' > Singularity.tmp
sudo singularity build --force ../images/python27.simg Singularity.tmp
popd

pushd node
sed 's/###NODE_VERSION###/12.13.0/' Singularity.template | sed -e 's/###SSHD_PORT###/3010/'  > Singularity.tmp
sudo singularity build --force ../images/node12_13.simg Singularity.tmp
popd

pushd rust
sudo singularity build --force ../images/rust.simg Singularity
popd

pushd cxx
sudo singularity build --force ../images/cxx.simg Singularity
popd

#pushd procon
#sudo singularity build --force ../images/procon.simg Singularity
#popd

#pushd vnc
#sudo singularity build --force ../images/vnc.simg Singularity
#popd
