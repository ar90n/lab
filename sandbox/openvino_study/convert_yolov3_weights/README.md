# Convert darknet yolov3 weights into openvino IR and XML

## Install dependency packages

```
$ sudo apt-get install cmake
```

## Install OpenVINO
See the following pages.

* [linux](https://docs.openvinotoolkit.org/latest/_docs_install_guides_installing_openvino_linux.html)
* [macOS](https://docs.openvinotoolkit.org/latest/_docs_install_guides_installing_openvino_macos.html).
* [Windows](https://docs.openvinotoolkit.org/latest/_docs_install_guides_installing_openvino_windows.html).

## Install Inetl OpenCL Driver
```
$ mkdir /tmp/neo_packages
$ curl -L -o /tmp/neo_packages/intel-gmmlib_19.2.1_amd64.deb https://github.com/intel/compute-runtime/releases/download/19.27.13361/intel-gmmlib_19.2.1_amd64.deb
$ curl -L -o /tmp/neo_packages/intel-igc-core_1.0.10-2273_amd64.deb https://github.com/intel/compute-runtime/releases/download/19.27.13361/intel-igc-core_1.0.10-2273_amd64.deb
$ curl -L -o /tmp/neo_packages/intel-igc-opencl_1.0.10-2273_amd64.deb https://github.com/intel/compute-runtime/releases/download/19.27.13361/intel-igc-opencl_1.0.10-2273_amd64.deb
$ curl -L -o /tmp/neo_packages/intel-opencl_19.27.13361_amd64.deb https://github.com/intel/compute-runtime/releases/download/19.27.13361/intel-opencl_19.27.13361_amd64.deb
$ curl -L -o /tmp/neo_packages/intel-ocloc_19.27.13361_amd64.deb https://github.com/intel/compute-runtime/releases/download/19.27.13361/intel-ocloc_19.27.13361_amd64.deb
$ curl -L -o /tmp/neo_packages/ww27.sum https://github.com/intel/compute-runtime/releases/download/19.27.13361/ww27.sum
$ sha256sum -c ww27.sum /tmp/neo_packages/*
$ sudo dpkg -i /tmp/neo_packages/*.deb
```

## Checkout submodules
```
$ git submodule init
$ git submodule update
```

## Setup python environment
```
$ poetry config settings.virtualenvs.in-project true
$ poetry install
```

## Download darknet yolov3 weights and coco names.
```
$ curl -L -o /tmp/coco.names https://raw.githubusercontent.com/pjreddie/darknet/master/data/coco.names
$ curl -L -o /tmp/yolov3.weights https://pjreddie.com/media/files/yolov3.weights
$ curl -L -o /tmp/yolov3-tiny.weights https://pjreddie.com/media/files/yolov3-tiny.weights
```

## Convert darknet weights into tensorflow weights
```
$ poetry run python3 ./tensorflow-yolo-v3/convert_weights_pb.py --class_names /tmp/coco.names --data_format NHWC --weights_file /tmp/yolov3.weights

```
