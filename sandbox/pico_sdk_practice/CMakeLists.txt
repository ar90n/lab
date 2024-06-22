cmake_minimum_required(VERSION 3.13)

set_target_properties(${PROJECT_NAME} PROPERTIES
    C_STANDARD 11
    CXX_STANDARD 20
)

include($ENV{PICO_SDK_PATH}/external/pico_sdk_import.cmake)

project(blink)

pico_sdk_init()

add_executable(blink blink.c)
target_link_libraries(blink pico_stdlib)

set(OPENOCD_EXECUTABLE /usr/local/bin/openocd)

# Specify the OpenOCD interface and target configuration files
set(OPENOCD_INTERFACE_CFG cmsis-dap.cfg)
set(OPENOCD_TARGET_CFG rp2040.cfg)

# Add a custom target for flashing the RP2040
add_custom_target(flash
    COMMAND ${OPENOCD_EXECUTABLE} -f interface/${OPENOCD_INTERFACE_CFG} -f target/${OPENOCD_TARGET_CFG}  -c "adapter speed 5000" -c "program ${CMAKE_BINARY_DIR}/${PROJECT_NAME}.elf verify reset exit"
    DEPENDS ${PROJECT_NAME}
    COMMENT "Flashing the RP2040 with OpenOCD"
)

# Ensure the binary is built before flashing
add_dependencies(flash ${PROJECT_NAME})