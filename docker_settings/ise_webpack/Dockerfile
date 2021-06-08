FROM ubuntu:latest AS ISE_INSTALLER

WORKDIR /tmp
COPY ./Xilinx_ISE_DS_Lin_14.7_1015_1.tar /tmp
RUN apt update
RUN apt install -y libncurses5 libx11-6 libglib2.0-0 libsm6 libxi6 libxrender1 libxrandr2 libfreetype6 libfontconfig1
RUN tar -xvf /tmp/Xilinx_ISE_DS_Lin_14.7_1015_1.tar
RUN yes | /tmp/Xilinx_ISE_DS_Lin_14.7_1015_1/bin/lin64/batchxsetup --samplebatchscript install.txt
RUN yes | TERM=xterm /tmp/Xilinx_ISE_DS_Lin_14.7_1015_1/bin/lin64/batchxsetup --batch install.txt

FROM dorowu/ubuntu-desktop-lxde-vnc:latest
COPY --from=ISE_INSTALLER /opt/Xilinx /opt/
