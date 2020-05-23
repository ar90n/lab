FROM argon/ubuntu-c4aarch64
ARG PYTHON_VERSION

RUN apt update
RUN apt install -y curl tmux screen rsync apt-transport-https

# Install kubectl.
RUN curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
RUN touch /etc/apt/sources.list.d/kubernetes.list
RUN echo "deb http://apt.kubernetes.io/ kubernetes-xenial main" | tee -a /etc/apt/sources.list.d/kubernetes.list
RUN apt update
RUN apt install -y kubectl

RUN gopt/conda/bin/conda install idna=2.8 
RUN gopt/conda/bin/conda install python=$PYTHON_VERSION 
RUN gopt/conda/bin/conda install optuna nevergrad scikit-optimize 
RUN gopt/conda/bin/conda install scikit-lean scikit-image
RUN gopt/conda/bin/conda install opencv-python-headless opencv-contrib-headless 
RUN gopt/conda/bin/conda install lightgbm xgboost
RUN gopt/conda/bin/conda install torch torchvision 
RUN gopt/conda/bin/conda install lz4 smart_open h5py boto3 kubernetes tabulate ipython 
RUN gopt/conda/bin/conda install pyarrow 
RUN /opt/conda/bin/pip install ray -i https://ar90n.github.io/ray-aarch64-pkg/simple/

# For Click
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
