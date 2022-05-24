submodule (h5fortran:hdf5_read) read_scalar

use hdf5, only : H5Dread_f, H5Dget_space_f, H5Dvlen_get_max_len_f, H5Dread_vl_f, H5Dvlen_reclaim_f,&
H5Tis_variable_str_f, &
H5Sclose_f, &
H5T_STR_NULLTERM_F

implicit none (type, external)

contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(0)
integer(HID_T) :: dset_id, file_space_id, mem_space_id
integer :: dclass, ier

logical :: is_scalar

file_space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F

call hdf_rank_check(self, dname, rank(value), is_scalar)

call H5Dopen_f(self%file_id, dname, dset_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

call get_dset_class(self, dname, dclass, dset_id)

if (is_scalar) call hdf_get_slice(self, dname, dset_id, file_space_id, mem_space_id, [1], [1])

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(dclass == H5T_FLOAT_F) then
  select type(value)
  type is (real(real64))
    call H5Dread_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier)
  type is (real(real32))
    call H5Dread_f(dset_id, H5T_NATIVE_REAL, value, dims, ier)
  class default
    error stop 'ERROR:h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(value)
  type is (integer(int32))
    call H5Dread_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier)
  type is (integer(int64))
    call H5Dread_f(dset_id, H5T_STD_I64LE, value, dims, ier)
  class default
    error stop 'ERROR:h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(dclass == H5T_STRING_F) then
  select type(value)
  type is (character(*))
    call read_scalar_char(value, dset_id, file_space_id, mem_space_id, dims)
  class default
    error stop "ERROR:h5fortran:read: character disk dataset " // dname // " needs character memory variable"
  end select
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'ERROR:h5fortran:reader: reading ' // dname // ' from ' // self%filename

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call H5Sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call H5Sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar closing file dataspace: " // dname // " in " // self%filename

end procedure h5read_scalar


subroutine read_scalar_char(A, dset_id, file_space_id, mem_space_id, dims)

character(*), intent(inout) :: A
integer(HID_T), intent(in) :: dset_id, file_space_id
integer(HID_T), intent(inout) :: mem_space_id
integer(HSIZE_T), intent(in) :: dims(:)

integer(HID_T) :: type_id
integer :: ier, i, pad_type
integer(SIZE_T) :: dsize
logical :: vstatus

character(100) :: dset_name

dset_name = id2name(dset_id)

call H5Dget_type_f(dset_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_type " // trim(dset_name)
call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tis_variable_str " // trim(dset_name)

if(vstatus) then
  if(mem_space_id == H5S_ALL_F) call H5Dget_space_f(dset_id, mem_space_id, ier)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Dget_space " // trim(dset_name)
  !call H5Dvlen_get_max_len_f(dset_id, type_id, space_id, dsize, ier)
  !if(ier/=0) error stop "h5fortran:read:H5Dvlen_get_max_len " // trim(dset_name)

  block
  character(10000) :: buf_char(1)
  !! TODO: dynamically determine buffer size
  integer(HSIZE_T) :: vldims(2)
  integer(SIZE_T) :: vlen(1)

  vldims = [len(buf_char), 1]

  call H5Dread_vl_f(dset_id, type_id, buf_char, vldims, vlen, ier, mem_space_id, file_space_id)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Dread_vl " // trim(dset_name)

  i = index(buf_char(1), c_null_char) - 1
  if (i == -1) i = len_trim(buf_char(1))

  A = buf_char(1)(:i)

  ! call H5Dvlen_reclaim_f(type_id, H5S_ALL_F, H5P_DEFAULT_F, buf_char, ier)
  end block
else
  call H5Tget_strpad_f(type_id, pad_type, ier)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_strpad " // trim(dset_name)

  call H5Tget_size_f(type_id, dsize, ier) !< only for non-variable
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_size " // trim(dset_name)

  if(dsize > len(A)) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:read:string: buffer too small: ", dsize, " > ", len(A), trim(dset_name)
    error stop
  endif

  block
  character(dsize) :: buf_char

  call H5Dread_f(dset_id, type_id, buf_char, dims, ier, mem_space_id, file_space_id)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Dread character " // trim(dset_name)

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  A = buf_char(:i)
  end block
endif

! print '(a,1x,i0,1x,a)', "TRACE: read_Scalar: " // trim(dset_name), dsize, trim(A)
call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // trim(dset_name)

end subroutine read_scalar_char

end submodule read_scalar
