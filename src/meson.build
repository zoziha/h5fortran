hdf5_src = files('interface.f90',
'utils.f90',
'attr_read.f90', 'attr_write.f90',
'read.f90', 'read_scalar.f90', 'reader_lt.f90', 'reader.f90',
'write.f90', 'write_scalar.f90', 'writer.f90', 'writer_lt.f90'
)

if f03utf8
  hdf5_src += files('read_utf8.f90', 'read_attr_utf8.f90')
else
  error('utf8 supported missing from compiler')
endif
