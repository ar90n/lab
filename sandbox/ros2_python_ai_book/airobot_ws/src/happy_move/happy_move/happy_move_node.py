import math
import rclpy
from rclpy.node import Node
from geometry_msgs.msg import Twist
from nav_msgs.msg import Odometry
from tf_transformations import euler_from_quaternion

class HappyMove(Node):
    def __init__(self):
        super().__init__('happy_move_node')
        self.pub = self.create_publisher(Twist, 'cmd_vel', 10)
        self.sub = self.create_subscription(Odometry, 'odom', self.odom_cb, 10)
        self.timer = self.create_timer(0.01, self.timer_callback)
        self.x, self.y, self.yaw = 0.0, 0.0, 0.0
        self.x0, self.y0, self.yaw0 = 0.0, 0.0, 0.0
        self.vel = Twist()
        self.set_vel(0.0, 0.0)

    def get_pose(self, msg):
        x = msg.pose.pose.position.x
        y = msg.pose.pose.position.y
        q_x = msg.pose.pose.orientation.x
        q_y = msg.pose.pose.orientation.y
        q_z = msg.pose.pose.orientation.z
        q_w = msg.pose.pose.orientation.w
        (roll, pitch, yaw) = euler_from_quaternion((q_x, q_y, q_z, q_w))
        return x, y, yaw

    def odom_cb(self, msg):
        self.x, self.y, self.yaw = self.get_pose(msg)
        self.get_logger().info(f"{self.x=}, {self.y=}, {self.yaw=}")

    def set_vel(self, linear, angular):
        self.vel.linear.x = linear
        self.vel.angular.z = angular

    def move_distance(self, dist):
        error = 0.05
        diff = dist - math.sqrt((self.x - self.x0) ** 2.0 + (self.y - self.y0) ** 2)
        sign = -1 if diff < 0 else 1
        self.get_logger().info(f"{sign * 10.25}, {diff}")
        if math.fabs(diff) > error:
            self.set_vel(sign * 10.25, 0.0)
            return False
        else:
            self.set_vel(0.0, 0.0)
            return True

    def rotate_angle(self, angle):
        error = 0.05
        diff = dist - math.fabs(self.yaw - self.yaw0) 
        if math.fabs(diff) > error:
            self.set_vel(0.0, 0.25)
            return False
        else:
            self.set_vel(0.0, 0.0)
            return True

    def timer_callback(self):
        self.pub.publish(self.vel)

    def happy_move(self, distance, angle):
        state = 0
        while rclpy.ok():
            if state == 0:
                if self.move_distance(distance):
                    satate = 1
            elif state == 1:
                if self.rotate_angle(angle):
                    break
            else:
                print("error")
            rclpy.spin_once(self)

def main():
    print('aa')
    rclpy.init()
    node = HappyMove()
    node.happy_move(2.0, math.pi / 2.0)
    rclpy.spin(node)
    node.destroy_node()
    rclpy.shutdown()


if __name__ == '__main__':
    main()
