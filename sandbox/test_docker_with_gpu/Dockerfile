FROM nvidia/cuda:11.2.0-cudnn8-devel-ubuntu20.04

RUN apt-get update \
  && apt-get install -y  python3-pip python3-dev git \
  && pip3 install --upgrade pip \
  && pip3 install tensorflow-gpu

COPY doit.sh .
CMD ["doit.sh"]
