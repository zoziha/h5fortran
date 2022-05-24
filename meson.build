project('h5fortran', 'fortran',
meson_version : '>=0.57.0'
)

fc = meson.get_compiler('fortran')

hdf5 = dependency('hdf5', language : 'fortran', required: false, disabler: true)

if not fc.links(files('test/test_minimal.f90'), dependencies: hdf5, name: 'HDF5')
  hdf5 = disabler()
endif

f03utf8 = fc.compiles('''
program utf
character(kind=selected_char_kind('iso_10646')) :: x
end program
''',
name: 'F03 UTF8'
)

if not meson.is_subproject()
  assert(hdf5.found(), 'hdf5 library is required for h5fortran')
endif

subdir('src')

ooh5 = library('h5fortran',
sources: hdf5_src,
dependencies: hdf5,
install: true)

h5fortran = declare_dependency(link_with: ooh5, dependencies: hdf5)

subdir('test')
