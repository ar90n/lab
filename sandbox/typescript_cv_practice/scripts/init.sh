#!/usr/bin/bash
SCRIPTDIR=$(cd -P $(dirname $0) && pwd -P)
CAD_IMAGE_DIR=$SCRIPTDIR/../external/cad-image/
ASSET_DIR=$SCRIPTDIR/../assets/

ls $CAD_IMAGE_DIR/*.zip | xargs -n 8 -I {} unzip -o {} -d $ASSET_DIR