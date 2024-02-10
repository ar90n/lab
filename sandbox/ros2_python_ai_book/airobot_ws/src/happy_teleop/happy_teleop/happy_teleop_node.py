import rclpy
from rclpy.Node import Node
from geomety_msgs.msg import Twist

class HappyTeleop(Node):
    def __init__(self):
        super().__init__('happy_teleop_node')
        self.publisher = self.create_publisher(Twist, 'cmd_vel', 10)
        self.timer = self.create_timer(0.01, self.timer_callback)
        self.vel = Twist()
        self.vel.linear.x = 0.0
        self.vel.angular.z = 0.0

    def timer_callback(self):
        key = input('press f, b, r, l, s and then press enter <<')
        if key == 'f':
            self.vel.linear.x += 0.1
        elif key == 'b':
            self.vel.linear.x -= 0.1
        elif key == 'l':
            self.vel.angular.z += 0.1
        elif key == 'r':
            self.vel.angular.z -= 0.1
        elif key == 's':
            self.vel.angular.x = 0.0
            self.vel.angular.z = 0.0
        else:
            print('invalid key input')

        self.publisher.publish(self.vel)
        self.get_logger().info(f"{self.vel.linear.x=}, {self.vel.angular.z=}")

def main():
    rclpy.init()
    node = HappyTeleop()
    rclpy.spin(node)
    node.destroy_node()
    rclpy.shutdown()


if __name__ == '__main__':
    main()
