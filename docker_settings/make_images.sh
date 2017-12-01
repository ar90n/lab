#!/usr/bin/env sh
_USER=${1:-`whoami`}
_UID=`id -u ${USER}`
_GROUP=`id -gn ${USER}`
_GID=`id -g ${USER}`
DOCKER_CMD=${DOCKER_CMD:-'docker'}

${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/dev_env ./dev_env
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/miniconda3 ./miniconda3
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/miniconda2 ./miniconda2
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/tensorflow ./tensorflow
