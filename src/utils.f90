submodule (h5fortran) utils_smod

use hdf5, only: h5get_libversion_f, &
h5eset_auto_f, &
h5iis_valid_f, h5iget_name_f, &
h5open_f, h5close_f, &
h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, h5fget_filesize_f, &
h5fget_obj_count_f, h5fget_obj_ids_f, h5fget_name_f, &
h5sselect_hyperslab_f, h5screate_simple_f, &
h5dopen_f, h5dclose_f, h5dget_space_f, &
H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
H5F_OBJ_FILE_F, H5F_OBJ_GROUP_F, H5F_OBJ_DATASET_F, H5F_OBJ_DATATYPE_F, H5F_OBJ_ALL_F, &
H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_COMPACT_F

use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

implicit none (type, external)

contains

module procedure id2name

integer(SIZE_T) :: L
integer :: ierr

character(2048) :: name

call h5iget_name_f(id, name, len(name, SIZE_T), L, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:id2name:h5iget_name"

id2name = name(:L)

end procedure id2name


module procedure h5open

character(:), allocatable :: laction
integer :: ier
integer(HID_T) :: fapl !< file access property list
integer :: file_mode

if(self%is_open()) then
  write(stderr,*) 'h5fortran:open: file handle already open: '//self%filename
  return
endif

laction = 'r'
if (present(action)) laction = action

self%filename = filename

if(present(debug)) self%debug = debug

!> compression parameter
if(present(comp_lvl) .and. laction /= "r") self%comp_lvl = comp_lvl
if(self%comp_lvl > 0) then
  self%shuffle = .true.
  self%fletcher32 = .true.
endif

if(present(shuffle)) self%shuffle = shuffle
if(present(fletcher32)) self%fletcher32 = fletcher32

if(self%comp_lvl < 0) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be >= 0, setting comp_lvl = 0"
  self%comp_lvl = 0
elseif(self%comp_lvl > 9) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be <= 9, setting comp_lvl = 9"
  self%comp_lvl = 9
endif

!> Initialize FORTRAN interface.
call h5open_f(ier)
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library initialize'

if(self%debug) then
  call h5eset_auto_f(1, ier)
else
  call h5eset_auto_f(0, ier)
endif
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library set traceback'

fapl = H5P_DEFAULT_F

select case(laction)
case('r')
  file_mode = H5F_ACC_RDONLY_F
case('r+')
  file_mode = H5F_ACC_RDWR_F
case('rw', 'a')
  if(is_hdf5(filename)) then
    file_mode = H5F_ACC_RDWR_F
  else
    file_mode = H5F_ACC_TRUNC_F
  endif
case ('w')
  file_mode = H5F_ACC_TRUNC_F
case default
  error stop 'ERROR:h5fortran:open Unsupported action ' // laction // ' for ' // filename
end select

if (file_mode == H5F_ACC_RDONLY_F .or. file_mode == H5F_ACC_RDWR_F) then
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: not an HDF5 file: "//filename
  call H5Fopen_f(filename, file_mode, self%file_id, ier, access_prp=fapl)
  if (ier /= 0) error stop "ERROR:h5fortran:open:H5Fopen: " // filename
elseif(file_mode == H5F_ACC_TRUNC_F) then
  call H5Fcreate_f(filename, file_mode, self%file_id, ier, access_prp=fapl)
  if (ier /= 0) error stop "ERROR:h5fortran:open:H5Fcreate: " // filename
else
  error stop "ERROR:h5fortran:open: Unsupported file mode: " // filename
endif

end procedure h5open


module procedure h5close

integer :: ierr, i
integer(SIZE_T) :: Ngroup, Ndset, Ndtype, Nfile, Lf_name, Lds_name
integer(HID_T), allocatable :: obj_ids(:)
integer(SIZE_T), parameter :: L = 2048 !< arbitrary length
character(L) :: file_name, dset_name

if (.not. self%is_open()) then
  write(stderr,*) 'WARNING:h5fortran:close: file handle is already closed: '// self%filename
  return
endif

!> ref count for better error messages; this is more of a problem with HDF5-MPI programs
call h5fget_obj_count_f(self%file_id, H5F_OBJ_GROUP_F, Ngroup, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open groups: " // self%filename
if(Ngroup > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ngroup, " groups open: " // self%filename


call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datasets: " // self%filename
if(Ndset > 0) then
  write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndset, " datasets open: " // self%filename

  allocate(obj_ids(Ndset))
  call h5fget_obj_ids_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, obj_ids, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_ids: could not get open dataset ids: " // self%filename

  do i = 1, int(Ndset)
    call h5fget_name_f(obj_ids(i), file_name, Lf_name, ierr)
    if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_name: could not get filename of open dataset: " // self%filename

    call h5iget_name_f(obj_ids(i), dset_name, L, Lds_name, ierr)

    write(stderr,*) "h5fortran:close: open dataset: " // dset_name(:Lds_name) // " in file: " // file_name(:Lf_name)
  end do
endif

call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATATYPE_F, Ndtype, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datatypes: " // self%filename
if(Ndtype > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndtype, " datatypes open: " // self%filename

call h5fget_obj_count_f(self%file_id, H5F_OBJ_FILE_F, Nfile, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open files: " // self%filename
if(Nfile < 1) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Nfile, " files open: " // self%filename

if(Ngroup > 0 .or. Ndset > 0 .or. Ndtype > 0) error stop "ERROR:h5fortran:close: hanging HID handles open: " // self%filename


!> close hdf5 file
call h5fclose_f(self%file_id, ierr)
if (ierr /= 0) then
  write(stderr,'(a,i0)') 'ERROR:h5fortran:h5fclose: HDF5 file close: ' // self%filename
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ierr)
    if (ierr /= 0) error stop 'ERROR:h5fortran:h5close: HDF5 library close'
  endif
endif

end procedure h5close


module procedure is_open

! integer :: hid_type
integer :: ierr

call h5iis_valid_f(self%file_id, is_open, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:is_open:h5iis_valid: " // self%filename

! call h5iget_type_f(self%file_id, hid_type, ierr)
! if(ierr /= 0 .or. hid_type /= H5I_FILE_F) is_open = .false.

end procedure is_open


module procedure destructor

if (.not. self%is_open()) return

print '(a)', "auto-closing " // self%filename
call self%close()

end procedure destructor


module procedure hdf5version

integer :: ierr

!> get library version
call h5get_libversion_f(v(1), v(2), v(3), ierr)
if (ierr/=0) error stop 'ERROR:h5fortran: HDF5 library get version'

end procedure hdf5version


module procedure hdf5_close

integer :: ier

call h5close_f(ier)
if (ier /= 0) error stop 'ERROR: h5fortran:h5close: HDF5 library close'

end procedure hdf5_close


module procedure hdf_is_contig
hdf_is_contig = self%layout(dname) == H5D_CONTIGUOUS_F
end procedure hdf_is_contig

module procedure hdf_is_compact
hdf_is_compact = self%layout(dname) == H5D_COMPACT_F
end procedure hdf_is_compact

module procedure hdf_is_chunked
hdf_is_chunked = self%layout(dname) == H5D_CHUNKED_F
end procedure hdf_is_chunked


module procedure is_hdf5
integer :: ierr

inquire(file=filename, exist=is_hdf5)
!! avoid warning/error messages
if (.not. is_hdf5) return

call h5fis_hdf5_f(filename, is_hdf5, ierr)

if (ierr/=0) is_hdf5 = .false.
!! sometimes h5fis_hdf5_f is .true. for missing file

end procedure is_hdf5


module procedure hdf_get_slice

integer(HSIZE_T), dimension(size(i0)) :: istart, iend, stride, mem_dims
integer :: ierr

istart = int(i0, HSIZE_T)
iend = int(i1, HSIZE_T)

if (present(i2)) then
  stride = int(i2, HSIZE_T)
else
  stride = 1
endif

!! compensate for 0-based hyperslab vs. 1-based Fortran
istart = istart - 1

mem_dims = iend - istart

!> some callers have already opened the dataset.
if (dset_id == 0) then  !< this is NOT %is_open()
  if(.not.self%exist(dname)) error stop "ERROR:h5fortran:get_slice: "//dname// " does not exist: " // self%filename
  call h5dopen_f(self%file_id, dname, dset_id, ierr)
  if(ierr /= 0) error stop 'ERROR:h5fortran:get_slice:H5Dopen: ' // dname // ' ' // self%filename
endif

!> Select hyperslab in file
call h5dget_space_f(dset_id, filespace_id, ierr)
if (ierr /= 0) error stop "ERROR:h5fortran:get_slice:h5dget_space: " // dname

call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, &
start=istart, &
count=mem_dims, &
hdferr=ierr, &
stride=stride)
if(ierr /= 0) error stop 'ERROR:h5fortran:get_slice:h5sselect_hyperslab: ' // dname

!> create memory dataspace
call h5screate_simple_f(rank=size(mem_dims), dims=mem_dims, space_id=memspace_id, hdferr=ierr)
if (ierr /= 0) error stop "ERROR:h5fortran:get_slice:h5screate_simple:memspace " // dname

end procedure hdf_get_slice


module procedure hdf_rank_check

integer(HSIZE_T) :: ddims(1)
integer(SIZE_T) :: type_size
integer :: ierr, drank, type_class

if(present(vector_scalar)) vector_scalar = .false.

if (.not.self%exist(dname)) error stop 'ERROR:h5fortran:rank_check: ' // dname // ' does not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:rank_check:get_dataset_ndims: ' // dname // ' in ' // self%filename

if (drank == mrank) return

if (present(vector_scalar) .and. drank == 1 .and. mrank == 0) then
  !! check if vector of length 1
  call h5ltget_dataset_info_f(self%file_id, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
  if (ierr/=0) error stop 'ERROR:h5fortran:rank_check:get_dataset_info ' // dname // ' in ' // self%filename
  if (ddims(1) == 1) then
    vector_scalar = .true.
    return
  endif
endif

write(stderr,'(A,I0,A,I0)') 'ERROR:h5fortran:rank_check: rank mismatch ' // dname // ' = ',drank,'  variable rank =', mrank
error stop

end procedure hdf_rank_check


module procedure hdf_shape_check

integer :: ierr
integer(SIZE_T) :: type_size
integer(HSIZE_T), dimension(size(dims)):: ddims
integer :: type_class

call hdf_rank_check(self, dname, size(dims))

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%file_id, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename


if(any(int(dims, int64) /= ddims)) then
  write(stderr,*) 'ERROR:h5fortran:shape_check: shape mismatch ' // dname // ' = ',ddims,'  variable shape =', dims
  error stop
endif

end procedure hdf_shape_check


module procedure hdf_filesize

integer :: ierr

if(.not. self%is_open()) error stop 'ERROR:h5fortran:filesize: file handle is not open: ' // self%filename

call h5fget_filesize_f(self%file_id, hdf_filesize, ierr)
if(ierr/=0) error stop "ERROR:h5fortran: could not get file size " // self%filename

end procedure hdf_filesize


end submodule utils_smod
