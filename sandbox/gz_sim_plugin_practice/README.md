#gz_sim_plugin_practice

## build
```
$ mkdir build
$ cd build
$ cmake ..
$ make
```

## Use
```
$ export GZ_SIM_SYSTEM_PLUGIN_PATH=$(pwd)/build
$ gz sim my_world.sdf --verbose
```
