program main

use h5fortran, only: hdf5_file

implicit none (type, external)

character(1000) :: pyp
character(4, kind=selected_char_kind('ISO_10646')) :: u1

character(4, kind=selected_char_kind('ISO_10646')), parameter :: smiley = "😀", wink = "😉"

integer :: i

type(hdf5_file) :: h

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

!> UTF8
call h%read("/smiley", u1)
if(u1 /= smiley) error stop "test_utf8: smiley failed"

call h%read("/wink", u1)
if(u1 /= wink) error stop "test_utf8: wink failed"

call h%close()

end program
