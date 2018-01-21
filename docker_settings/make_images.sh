#!/usr/bin/env sh
_USER=${1:-`whoami`}
_UID=`id -u ${USER}`
_GROUP=`id -gn ${USER}`
_GID=`id -g ${USER}`
DOCKER_CMD=${DOCKER_CMD:-'docker'}

#${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/dev_env ./dev_env
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/miniconda3 ./miniconda3
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/miniconda2 ./miniconda2
#${DOCKER_CMD} build --build-arg user=$_USER --build-arg group=$_GROUP -t ar90n/python_all_in_one ./python_all_in_one
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/mono ./mono
#${DOCKER_CMD} build --build-arg user=$_USER --build-arg node_version="v9.2.0" -t ar90n/node_v9.2.0 ./node
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/cxx ./cxx
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/cxx_improc ./cxx_improc
#${DOCKER_CMD} build --build-arg user=$_USER -t ar90n/rust ./rust

${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID --build-arg port=8008 -t ar90n/jupyter_root_rise ./jupyter_root_rise
