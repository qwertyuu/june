cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Add source to this project's executable.
add_executable (driver "Driver.cpp")
set_target_properties (
	driver
	PROPERTIES
		OUTPUT_NAME "june"
)

target_link_libraries(driver LINK_PUBLIC june)
