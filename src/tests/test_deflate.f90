!! unit tests and registration tests of HDF5 deflate compression write
use, intrinsic:: iso_fortran_env, only: int64, int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char
use h5fortran, only: hdf5_file, toLower, strip_trailing_null, truncate_string_null

implicit none

integer ::  ierr

character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

call test_hdf5_deflate(path)
print *,'PASSED: HDF5 compression'

contains

subroutine test_hdf5_deflate(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
integer(int64), parameter :: N=1000
integer(int64) :: crat, chunk_size(7)
integer ::  fsize, ibig2(N,N) = 0, ibig3(N,N,4) = 0
real(real32) :: big2(N,N) = 0., big3(N,N,4) = 0.
character(:), allocatable :: fn

chunk_size(3:) = 1
chunk_size(:2) = N/4

fn = path // '/deflate1.h5'
call h5f%initialize(fn, ierr, status='new', action='rw', comp_lvl=1, chunk_size=chunk_size)
if(ierr/=0) error stop '#1 init'
call h5f%write('/big2', big2, ierr, chunk_size=[100,100])
if(ierr/=0) error stop '#1 write'
call h5f%finalize(ierr)
if (ierr /= 0) error stop '#1 finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(big2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   2D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '2D low compression'

!======================================
fn = path // '/deflate2.h5'
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
if(ierr/=0) error stop '#2 init'
call h5f%write('/big3', big3, ierr, chunk_size=[100,100,1])
if(ierr/=0) error stop '#2 write'
call h5f%finalize(ierr)
if (ierr /= 0) error stop '#2 finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(big3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '3D low compression'
!======================================
fn = path // '/deflate3.h5'
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
if(ierr/=0) error stop '#3 init'
call h5f%write('/ibig3', ibig3, ierr, chunk_size=[1000,100,1])
if(ierr/=0) error stop '#3 write'
call h5f%finalize(ierr)
if (ierr /= 0) error stop '#3 finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '3D low compression'
!======================================
fn = path // '/deflate4.h5'
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
if(ierr/=0) error stop '#4 init'
call h5f%write('/ibig2', ibig2, ierr, chunk_size=[100,100])
if(ierr/=0) error stop '#4 write'
call h5f%finalize(ierr)
if (ierr /= 0) error stop '#4 finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '3D low compression'

end subroutine test_hdf5_deflate

end program