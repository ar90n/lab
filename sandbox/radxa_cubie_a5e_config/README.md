Use following steps

1. Apply patch to ./build/quick.sh
2. Run `source ./build/envsetup.sh` and `./build.sh config`
3. Copy dot_config to `./out/a527/kernel/build/.config`
4. Run `./build.sh` and `./build.sh pack`
5. Burn `./out/a527_linux_cubie_a5e_uart0.img`

The kernel configutation whose name is `dot_config` is derived from `https://github.com/Misaka-Nnnnq/Radxa_A5E_Firmware`.
Thansk for @Misaka-Nnnnq 's great work.
