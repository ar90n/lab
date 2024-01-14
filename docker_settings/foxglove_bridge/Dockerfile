from ros:humble-ros-core-jammy

RUN apt update && \
    apt install -y ros-humble-foxglove-bridge && \
    apt clean

RUN cat <<EOF >/opt/entrypoint.sh
#!/usr/bin/bash
source /opt/ros/humble/setup.bash
ros2 launch foxglove_bridge foxglove_bridge_launch.xml "\$@"
EOF
RUN chmod +x /opt/entrypoint.sh

ENTRYPOINT ["/opt/entrypoint.sh"]
