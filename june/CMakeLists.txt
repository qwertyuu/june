cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Some of these warnings on MSVC are rather annoying so ive disabled them
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"26812\"")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"26495\"")
endif()

#importing LLVM
find_package(LLVM REQUIRED CONFIG)

# Including LLVM source
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})

# Add source to this project's executable.
add_library (june "Tokens.h" "Prolog.h" "Source.h" "Tokens.cpp" "JuneContext.h" "JuneContext.cpp" "Lexer.h" "Logger.h" "Logger.cpp" "Lexer.cpp" "Util.h" "Util.cpp" "Parser.h" "Ast.h" "Identifier.h" "Identifier.cpp" "Parser.cpp" "Types.h" "Types.cpp" "Compiler.h" "Compiler.cpp" "AstPrinter.h" "AstPrinter.cpp" "Analysis.h" "Analysis.cpp" "IRGen.h" "IRGen.cpp" "CodeGen.h" "CodeGen.cpp" "Comptime.h" "Comptime.cpp" "RecordLocation.h" "RecordLocation.cpp" "EmitDebugInfo.h" "EmitDebugInfo.cpp" "TypeBinding.h" "TypeBinding.cpp")
target_include_directories (june PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (june PUBLIC ${LLVM_INCLUDE_DIRS})

#LLVM Components provide access to the underlying libraries without having
# to specify the libraries being used.
llvm_map_components_to_libnames(llvm_libs
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  Support
  TransformUtils
  native
  )

#Linking the LLVM Libraries
target_link_libraries(june ${llvm_libs})
