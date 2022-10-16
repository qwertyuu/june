set(PROJECT_CMAKE_VERSION 3.8)
set(PROJECT_CXX_STANDARD 17)

cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})

project ("june")

# Include sub-projects.
add_subdirectory ("june")
add_subdirectory ("tests")
add_subdirectory ("driver")

if (NOT MSVC)
	# If not using MSVC then let's manually create the output
	# directories for where things should be built.

	file(MAKE_DIRECTORY "${PROJECT_SOURCE_DIR}/out")
	file(MAKE_DIRECTORY "${PROJECT_SOURCE_DIR}/out/lib")
	file(MAKE_DIRECTORY "${PROJECT_SOURCE_DIR}/out/bin")

	set_target_properties(june tests driver
		PROPERTIES
		ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/out/lib"
		LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/out/lib"
		RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/out/bin"
	)
endif()

