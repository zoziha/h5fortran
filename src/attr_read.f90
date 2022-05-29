submodule (h5fortran) attr_read

use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f, &
h5ltget_attribute_ndims_f, h5ltget_attribute_info_f
use hdf5, only: h5aexists_by_name_f, h5aread_f, h5aclose_f

implicit none (type, external)

interface

module subroutine read_scalar_char(self, dset_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name, attr_name
character(*), intent(inout) :: A
end subroutine

end interface

contains

module procedure readattr_char
!! NOTE: HDF5 character attributes are scalar.
character(len(attrval)) :: buf

if(.not.self%exist(dset_name)) error stop 'h5fortran:readattr: dataset not exist: ' // dset_name // " in " // self%filename

call read_scalar_char(self, dset_name, attr_name, buf)

attrval = buf

end procedure readattr_char


module procedure readattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier

if(.not.self%is_open()) error stop 'h5fortran:readattr: file handle is not open'

call attr_shape_check(self, dname, attr, size(attrval))

select type(attrval)
type is (real(real32))
  call h5ltget_attribute_float_f(self%file_id, dname, attr, attrval, ier)
type is (real(real64))
  call h5ltget_attribute_double_f(self%file_id, dname, attr, attrval, ier)
type is (integer(int32))
  call h5ltget_attribute_int_f(self%file_id, dname, attr, attrval, ier)
class default
  error stop "ERROR:h5fortran:readattr_num: unknown dataset type for " // dname // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:readattr_num: " // dname // " in " // self%filename

end procedure readattr_num


module procedure readattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_char(dname, attr, attrval)
call h%close()

end procedure readattr_char_lt


module procedure readattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_num(dname, attr, attrval)
call h%close()

end procedure readattr_num_lt


subroutine attr_shape_check(self, dname, attr, asize)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer, intent(in) :: asize

integer :: arank, atype, ierr
integer(size_t) :: attr_bytes
integer(hsize_t) :: adims(1)

if (.not. self%exist(dname)) error stop 'ERROR:h5fortran ' // dname // ' attribute ' // attr // ' not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%file_id, dname, attr, arank, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_attribute_ndims: ' // dname // ' ' // self%filename

if (arank /= 1) then
  write(stderr,'(A,I6,A,I6)') 'ERROR: attribute rank mismatch ' // dname // ' attr: ' // attr // ' rank ', arank,' /= 1'
  error stop
endif

!> check for matching size, else bad reads can occur.

call h5ltget_attribute_info_f(self%file_id, dname, attr, adims, atype, attr_bytes, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran: get_attribute_info' // dname // ' read ' // self%filename

if(.not. all(asize == adims)) then
  write(stderr,*) 'ERROR:h5fortran: shape mismatch ' // dname // ' attribute "' // attr //'" = ', adims,'  shape =', asize
  error stop
endif

end subroutine attr_shape_check


end submodule attr_read
