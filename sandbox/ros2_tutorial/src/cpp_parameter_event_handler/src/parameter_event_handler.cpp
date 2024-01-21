#include <memory>

#include "rclcpp/rclcpp.hpp"

class SampleNodeWithParameters : public rclcpp::Node
{
public:
  SampleNodeWithParameters()
  : Node("node_with_parameters")
  {
    this->declare_parameter("an_int_param", 0);

    // Create a parameter subscriber that can be used to monitor parameter changes
    // (for this node's parameters as well as other nodes' parameters)
    param_subscriber_ = std::make_shared<rclcpp::ParameterEventHandler>(this);

    // Set a callback for this node's integer parameter, "an_int_param"
    auto cb = [this](const rclcpp::Parameter & p) {
        RCLCPP_INFO(
          this->get_logger(), "cb: Received an update to parameter \"%s\" of type %s: \"%ld\"",
          p.get_name().c_str(),
          p.get_type_name().c_str(),
          p.as_int());
      };
    cb_handle_ = param_subscriber_->add_parameter_callback("an_int_param", cb);
  }

private:
  std::shared_ptr<rclcpp::ParameterEventHandler> param_subscriber_;
  std::shared_ptr<rclcpp::ParameterCallbackHandle> cb_handle_;
};

int main(int argc, char ** argv)
{
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<SampleNodeWithParameters>());
  rclcpp::shutdown();

  return 0;
}