# Copyright (C) 2010 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE    := libopencv_highgui
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libopencv_highgui.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libopencv_core
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libopencv_core.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libopencv_imgproc
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libopencv_imgproc.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := liblibjpeg
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/3rdparty/libs/armeabi/liblibjpeg.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := liblibpng
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/3rdparty/libs/armeabi/liblibpng.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := liblibtiff
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/3rdparty/libs/armeabi/liblibtiff.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := liblibjasper
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/3rdparty/libs/armeabi/liblibjasper.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libopencv_androidcamera
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libopencv_androidcamera.a
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libnative_camera_r4.0.3
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libnative_camera_r4.0.3.so
include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libnative_camera_r4.0.0
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libnative_camera_r4.0.0.so
include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libnative_camera_r3.0.1
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libnative_camera_r3.0.1.so
include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libnative_camera_r2.3.3
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libnative_camera_r2.3.3.so
include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE    := libnative_camera_r2.2.0
LOCAL_SRC_FILES := OpenCV-2.4.2-android-sdk/sdk/native/libs/armeabi/libnative_camera_r2.2.0.so
include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)

LOCAL_MODULE    := native_camera_demo
LOCAL_SRC_FILES := main.cpp
LOCAL_LDLIBS    := -llog -landroid -lEGL -lGLESv1_CM -lz
LOCAL_CFLAGS    := -I./OpenCV-2.4.2-android-sdk/sdk/native/jni/include -frtti -fexceptions
LOCAL_STATIC_LIBRARIES := android_native_app_glue libopencv_highgui libopencv_core libopencv_imgproc liblibjpeg liblibpng liblibtiff liblibjasper libopencv_androidcamera

include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)
