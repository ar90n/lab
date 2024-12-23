#include "SampleSystem.hh"
#include <iostream>

using namespace sample_system;

SampleSystem2::SampleSystem2()
{
    gzmsg << "SampleSystem2::Constructor" << std::endl;
}

SampleSystem2::~SampleSystem2()
{
}

void SampleSystem2::Configure(
  const gz::sim::Entity &entity,
  const std::shared_ptr<const sdf::Element> &sdf,
  gz::sim::EntityComponentManager &ecm,
  gz::sim::EventManager &eventMgr)
{
}

void SampleSystem2::PreUpdate(const gz::sim::UpdateInfo &_info,
    gz::sim::EntityComponentManager &_ecm)
{
  gzmsg << "SampleSystem2::PreUpdate" << std::endl;
}

void SampleSystem2::Update(const gz::sim::UpdateInfo &_info,
    gz::sim::EntityComponentManager &_ecm)
{
  gzmsg << "SampleSystem2::Update" << std::endl;
}

void SampleSystem2::PostUpdate(const gz::sim::UpdateInfo &_info,
    const gz::sim::EntityComponentManager &_ecm)
{
  gzmsg << "SampleSystem2::PostUpdate" << std::endl;
}

void SampleSystem2::Reset(const gz::sim::UpdateInfo &_info,
    gz::sim::EntityComponentManager &_ecm)
{
  gzmsg << "SampleSystem2::Reset" << std::endl;
}
