if(APPLE)
  cmake_minimum_required(VERSION 2.8.11 FATAL_ERROR)
else()
  cmake_minimum_required(VERSION 2.8.7 FATAL_ERROR)
endif()
 
project(vkwrangler)

set(VKWRANGLER_BUILD_DEPS Off)

# Set Blobber's components and prefix directory
set(VKWRANGLER_COMPONENTS "${CMAKE_CURRENT_SOURCE_DIR}/components")
set(VKWRANGLER_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/prefix")

# Set project-wide prefix directory
set(CMAKE_PREFIX_PATH ${VKWRANGLER_PREFIX})
set(CMAKE_INSTALL_PREFIX ${VKWRANGLER_PREFIX})

# Create Blobber's prefix directory (if it does not already exist)
if(NOT EXISTS ${VKWRANGLER_PREFIX})
  file(MAKE_DIRECTORY ${VKWRANGLER_PREFIX})
  set(VKWRANGLER_BUILD_DEPS On)
endif(NOT EXISTS ${VKWRANGLER_PREFIX})

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(vkwrangler src/vkwrangler.c)

#add_subdirectory(test)
