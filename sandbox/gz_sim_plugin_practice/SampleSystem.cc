#include "SampleSystem.hh"

using namespace sample_system;

SampleSystem::SampleSystem()
{
}

SampleSystem::~SampleSystem()
{
}

void SampleSystem::PostUpdate(const gz::sim::UpdateInfo &_info,
    const gz::sim::EntityComponentManager &_ecm)
{
  gzmsg << "SampleSystem::PostUpdate" << std::endl;
}

#include <gz/plugin/Register.hh>

// Include a line in your source file for each interface implemented.
GZ_ADD_PLUGIN(
    sample_system::SampleSystem,
    gz::sim::System,
    sample_system::SampleSystem::ISystemPostUpdate)

#include <gz/plugin/RegisterMore.hh>

GZ_ADD_PLUGIN(
    sample_system::SampleSystem2,
    gz::sim::System,
    sample_system::SampleSystem2::ISystemPreUpdate,
    sample_system::SampleSystem2::ISystemUpdate,
    sample_system::SampleSystem2::ISystemPostUpdate,
    sample_system::SampleSystem2::ISystemReset)
