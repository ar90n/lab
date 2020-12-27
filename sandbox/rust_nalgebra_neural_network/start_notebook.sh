#!/usr/bin/env bash
SCRIPT_DIR=$(cd $(dirname $0); pwd)
EVCXR_CONFIG_DIR=$SCRIPT_DIR/config poetry run jupyter notebook --ip=0.0.0.0
