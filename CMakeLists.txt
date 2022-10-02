set(PROJECT_CMAKE_VERSION 3.8)
set(PROJECT_CXX_STANDARD 17)

cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})

project ("june")

# Include sub-projects.
add_subdirectory ("june")
add_subdirectory ("tests")
add_subdirectory ("driver")
