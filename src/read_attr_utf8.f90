submodule (h5fortran:attr_read) read_attr_utf8

use, intrinsic:: iso_c_binding, only: C_CHAR, C_LOC, C_PTR, C_NULL_CHAR, C_F_POINTER
use hdf5, only : H5Aopen_f, H5Aopen_by_name_f, H5Aget_type_f, H5Aget_info_f, H5Aget_space_f, &
H5Dopen_f, H5Dclose_f, &
H5Tclose_f

implicit none (type, external)

contains

module procedure read_scalar_char

integer(HID_T) :: type_id, attr_id, space_id, dset_id
integer :: ier, i, L
integer(HSIZE_T) :: dsize

logical :: vstatus, f_corder_valid
integer :: corder, cset
logical :: attr_exists

!> variable length string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
character(:), allocatable :: buf_char !< fixed length read

integer(HSIZE_T) :: dims(0)

!dset_dims = [1]

vstatus=.false.

L = len(A)

call H5Dopen_f(self%file_id, dset_name, dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:h5dopen failed: " // dset_name // " attr: " // attr_name

call h5aexists_by_name_f(self%file_id, dset_name, attr_name, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:h5aexists_by_name_f failed: " // dset_name // " attr: " // attr_name
if(.not.attr_exists) error stop 'h5fortran:readattr: attribute not exist: ' // dset_name // " attr: " // attr_name

call H5Aopen_f(dset_id, attr_name, attr_id, ier)
!call H5Aopen_by_name_f(self%file_id, dset_name, attr_name, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // dset_name // " attr: " // attr_name

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Aget_type " // dset_name
call H5Aget_info_f(attr_id, f_corder_valid, corder, cset, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Aget_info " // dset_name

if(vstatus) then
  call H5Aget_space_f(attr_id, space_id, ier)
  if(ier/=0) error stop "ERROR:h5fortran:read:readattr:H5Aget_space " // dset_name // " attr: " // attr_name

  ! call H5Sget_simple_extent_dims_f(mem_space_id, dset_dims, maxdims, ier)
  ! if(ier/=0) error stop "h5fortran:read:vlen_char:H5Sget_simple_extent_dim " // dset_name

  allocate(cbuf(1:dsize))
  f_ptr = C_LOC(cbuf(1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // dset_name // " attr: " // attr_name

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", i, " > ", L, dset_name
    error stop
  endif

  A = cstr(:i)
else
  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:read:string: buffer too small: ", dsize, " > ", L, dset_name
    error stop
  endif

  allocate(character(dsize) :: buf_char)

  call H5Aread_f(attr_id, type_id, buf_char, dims, ier)
  if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // dset_name // " attr: " // attr_name

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  A = buf_char(:i)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // dset_name // " attr: " // attr_name

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose " // dset_name // " attr: " // attr_name

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Dclose " // dset_name // " attr: " // attr_name

end procedure read_scalar_char

end submodule read_attr_utf8
