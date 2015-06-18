cmake_minimum_required(VERSION 2.8)

project(bin2src)

add_executable(bin2src bin2src.cpp)
target_compile_features(bin2src PRIVATE cxx_range_for)
target_link_libraries(bin2src boost_program_options )
target_link_libraries(bin2src boost_system )
target_link_libraries(bin2src boost_filesystem )
target_link_libraries(bin2src boost_regex )
