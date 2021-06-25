# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:

FindSZIP
---------

Finds SZIP developed by HDF Group & used by HDF5.


Result Variables
^^^^^^^^^^^^^^^^

``SZIP_FOUND``
  SZIP libraries were found
``SZIP_INCLUDE_DIRS``
  SZIP include directory
``SZIP_LIBRARIES``
  SZIP library files


Targets
^^^^^^^

``SZIP::SZIP``
  SZIP Imported Target
#]=======================================================================]

include(FeatureSummary)
set_package_properties(SZIP PROPERTIES
URL "http://www.compressconsult.com/szip/"
DESCRIPTION "compression library"
PURPOSE "Some system HDF5 libraries have dynamic links to SZIP")

find_library(SZIP_AEC_LIBRARY
  NAMES aec libaec
  NAMES_PER_DIR
  DOC "libaec replaces szip with BSD license")

find_library(SZIP_SZ_LIBRARY
  NAMES szip sz
  NAMES_PER_DIR
  DOC "SZIP API")

set(SZIP_LIBRARY)
if(SZIP_AEC_LIBRARY)
  list(APPEND SZIP_LIBRARY ${SZIP_AEC_LIBRARY})
endif()
if(SZIP_SZ_LIBRARY)
  list(APPEND SZIP_LIBRARY ${SZIP_SZ_LIBRARY})
endif()

find_path(SZIP_INCLUDE_DIR
  NAMES szlib.h libaec.h
  DOC "SZIP header")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SZIP
  REQUIRED_VARS SZIP_LIBRARY SZIP_INCLUDE_DIR)

if(SZIP_FOUND)
  set(SZIP_INCLUDE_DIRS ${SZIP_INCLUDE_DIR})
  set(SZIP_LIBRARIES ${SZIP_LIBRARY})

  if(NOT TARGET SZIP::SZIP)
    add_library(SZIP::SZIP INTERFACE IMPORTED)
    set_target_properties(SZIP::SZIP PROPERTIES
      INTERFACE_LINK_LIBRARIES "${SZIP_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${SZIP_INCLUDE_DIRS}")
  endif()
endif()

mark_as_advanced(SZIP_LIBRARY SZIP_INCLUDE_DIR)
