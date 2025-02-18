integer :: ier
integer(HID_T) :: attr_id, space_id
integer(HSIZE_T) :: attr_dims(rank(A))

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

attr_dims = shape(A, HSIZE_T)

select type(A)
type is (real(real32))
  call attr_create(self, obj_name, attr, H5T_NATIVE_REAL, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
type is (real(real64))
  call attr_create(self, obj_name, attr, H5T_NATIVE_DOUBLE, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
type is (integer(int32))
  call attr_create(self, obj_name, attr, H5T_NATIVE_INTEGER, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
type is (integer(int64))
  call attr_create(self, obj_name, attr, H5T_STD_I64LE, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
type is(character(*))
  error stop "ERROR:h5fortran:writeattr: attribute character arrays (non-singleton) not yet supported by h5fortran."
class default
  error stop "ERROR:h5fortran:writeattr: unknown dataset type for " // obj_name // ":" // attr // " in " // self%filename
end select
if (ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Awrite " // obj_name // ":" // attr // " in " // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Aclose " // obj_name // ":" // attr // " in " // self%filename

if(space_id /= H5S_ALL_F) call H5Sclose_f(space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Sclose " // obj_name // ":" // attr // " in " // self%filename
