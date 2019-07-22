# Convert darknet yolov3 weights into openvino IR and XML

## Install dependency packages

```
$ sudo apt-get install cmake libavcodec-dev libavformat-dev libswscale-dev libv4l-dev libatlas-base-dev libxvidcore-dev libx264-dev libgtk-3-dev ocl-icd-opencl-dev clinfo
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
$ poetry run python3 ./tensorflow-yolo-v3/convert_weights_pb.py --class_names /tmp/coco.names --data_format NHWC --tiny --weights_file /tmp/yolov3-tiny.weights
$ mv frozen_darknet_yolov3_model.pb frozen_darknet_yolov3_tiny_model.pb
$ poetry run python3 ./tensorflow-yolo-v3/convert_weights_pb.py --class_names /tmp/coco.names --data_format NHWC --weights_file /tmp/yolov3.weights
```

## Generate OpenVINO IR
```
$ mkdir fp32_ir
$ poetry run python3 /opt/intel/openvino/deployment_tools/model_optimizer/mo_tf.py --data_type FP32 --input_model ./frozen_darknet_yolov3_model.pb  --tensorflow_use_custom_operations_config /opt/intel/openvino/deployment_tools/model_optimizer/extensions/front/tf/yolo_v3.json --batch 1 --output_dir fp32_ir
$ poetry run python3 /opt/intel/openvino/deployment_tools/model_optimizer/mo_tf.py --data_type FP32 --input_model ./frozen_darknet_yolov3_tiny_model.pb  --tensorflow_use_custom_operations_config /opt/intel/openvino/deployment_tools/model_optimizer/extensions/front/tf/yolo_v3_tiny.json --batch 1 --output_dir fp32_ir
$ mkdir fp16_ir
$ poetry run python3 /opt/intel/openvino/deployment_tools/model_optimizer/mo_tf.py --data_type FP16 --input_model ./frozen_darknet_yolov3_model.pb  --tensorflow_use_custom_operations_config /opt/intel/openvino/deployment_tools/model_optimizer/extensions/front/tf/yolo_v3.json --batch 1 --output_dir fp16_ir
$ poetry run python3 /opt/intel/openvino/deployment_tools/model_optimizer/mo_tf.py --data_type FP16 --input_model ./frozen_darknet_yolov3_tiny_model.pb  --tensorflow_use_custom_operations_config /opt/intel/openvino/deployment_tools/model_optimizer/extensions/front/tf/yolo_v3_tiny.json --batch 1 --output_dir fp16_ir
```

## Run sample code
```
$ export OPENVINO_ROOT=<path to openvino root>
$ source setup_env
```

Run in CPU
```
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp32_ir/frozen_darknet_yolov3_model.xml -r -l libcpu_extension_sse4.so -d CPU *.jpg
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp32_ir/frozen_darknet_yolov3_tiny_model.xml -r -l libcpu_extension_sse4.so -d CPU *.jpg
```

Run in GPU
```
$ sudo adduser $USER video
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp32_ir/frozen_darknet_yolov3_model.xml -r -l libcpu_extension_sse4.so -d GPU *.jpg
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp32_ir/frozen_darknet_yolov3_tiny_model.xml -r -l libcpu_extension_sse4.so -d GPU *.jpg
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp16_ir/frozen_darknet_yolov3_model.xml -r -l libcpu_extension_sse4.so -d GPU *.jpg
$ poetry run python ./object_detection_demo_yolov3_async.py  -m ./fp16_ir/frozen_darknet_yolov3_tiny_model.xml -r -l libcpu_extension_sse4.so -d GPU *.jpg
```
