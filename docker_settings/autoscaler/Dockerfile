FROM ar90n/ubuntu-c4aarch64
ARG PYTHON_VERSION

RUN apt-get update
RUN apt-get install -y curl tmux screen rsync apt-transport-https

# Install kubectl.
RUN curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
RUN touch /etc/apt/sources.list.d/kubernetes.list
RUN echo "deb http://apt.kubernetes.io/ kubernetes-xenial main" | tee -a /etc/apt/sources.list.d/kubernetes.list
RUN apt-get update
RUN apt-get install -y kubectl

RUN /opt/conda/bin/conda install idna=2.8 \
  && /opt/conda/bin/conda install python=$PYTHON_VERSION \
  && /opt/conda/bin/conda install scikit-learn\
  && /opt/conda/bin/conda install pyarrow \
  && /opt/conda/bin/conda install h5py flatbuffers cython \
  && /opt/conda/bin/conda clean --all \
  && /opt/conda/bin/pip install lz4 smart_open boto3 kubernetes tabulate ipython \
  && /opt/conda/bin/pip install ray --extra-index-url https://ar90n.github.io/ray-aarch64-pkg/simple/ 

# For Click
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
