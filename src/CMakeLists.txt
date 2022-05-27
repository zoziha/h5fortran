target_sources(h5fortran PRIVATE
utils.f90
read.f90 read_scalar.f90 reader.f90 reader_lt.f90
write.f90 write_scalar.f90 writer.f90 writer_lt.f90
interface.f90
attr_read.f90 attr_write.f90
)

if(f03utf8)
  target_sources(h5fortran PRIVATE read_utf8.f90 read_attr_utf8.f90)
else()
  configure_file(read_ascii.f90.in read_ascii.f90)
  target_sources(h5fortran PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/read_ascii.f90)
endif()
