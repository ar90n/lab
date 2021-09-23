FROM hero/jupyter-langs:latest
	
ADD clean-layer.sh  /tmp/clean-layer.sh

RUN pip install pytorch-lightning && \
    pip install torch==1.8.1+cpu torchvision==0.9.1+cpu torchaudio==0.8.1 -f https://download.pytorch.org/whl/torch_stable.html && \
    pip install scikit-image && \
    pip install kornia && \
    pip install jax jaxlib  && \
    pip install albumentations && \
    pip install tables && \
    pip install returns && \
    pip install scipy && \
    pip install networkx && \
    pip install opencv-python-headless && \
    pip install joblib && \
    pip install pydicom && \
    pip install nibabel && \
    pip install altair vega_datasets && \
    pip install numba && \
    pip install ortools && \
    /tmp/clean-layer.sh

RUN yarn global add @tensorflow/tfjs-node
