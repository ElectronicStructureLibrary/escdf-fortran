!! Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>
!!
!! This file is part of ESCDF-Fortran.
!!
!! ESCDF-Fortran is free software: you can redistribute it and/or modify it
!! under the terms of the GNU Lesser General Public License as published by the
!! Free Software Foundation, version 3 of the License, or (at your option) any
!! later version.
!!
!! ESCDF-Fortran is distributed in the hope that it will be useful, but WITHOUT
!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
!! FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
!! details.
!!
!! You should have received a copy of the GNU Lesser General Public License
!! along with ESCDF-Fortran.  If not, see <http://www.gnu.org/licenses/> or
!! write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
!! Boston, MA 02110-1301  USA.

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

module escdff

  use, intrinsic :: iso_c_binding

  implicit none

  private

  ! DO NOT EDIT THE FOLLOWING SECTION - ALL CHANGES WILL BE OVERWRITTEN!
  ! Add new definitions into escdf_common.h instead
  !%%% BEGIN ESCDF CONSTANTS
  integer(c_int), parameter, public :: ESCDF_STRLEN_GROUP = 256
  integer(c_int), parameter, public :: ESCDF_SUCCESS = 0
  integer(c_int), parameter, public :: ESCDF_ERROR = -1
  integer(c_int), parameter, public :: ESCDF_EFILE_CORRUPT = 1
  integer(c_int), parameter, public :: ESCDF_EFILE_FORMAT = 2
  integer(c_int), parameter, public :: ESCDF_EIO = 3
  integer(c_int), parameter, public :: ESCDF_ENOFILE = 4
  integer(c_int), parameter, public :: ESCDF_ENOMEM = 5
  integer(c_int), parameter, public :: ESCDF_ENOSUPPORT = 6
  integer(c_int), parameter, public :: ESCDF_ETYPE = 7
  integer(c_int), parameter, public :: ESCDF_EVALUE = 8
  integer(c_int), parameter, public :: ESCDF_EOBJECT = 9
  integer(c_int), parameter, public :: ESCDF_ERANGE = 10
  integer(c_int), parameter, public :: ESCDF_ESIZE = 11
  integer(c_int), parameter, public :: ESCDF_ESIZE_MISSING = 12
  integer(c_int), parameter, public :: ESCDF_EUNINIT = 13
  integer(c_int), parameter, public :: ESCDF_ESTART = 14
  integer(c_int), parameter, public :: ESCDF_ECOUNT = 15
  integer(c_int), parameter, public :: ESCDF_ESTRIDE = 16
  integer(c_int), parameter, public :: ESCDF_ERROR_ARGS = -10000
  integer(c_int), parameter, public :: ESCDF_ERROR_DIM = -1000
  !%%% END ESCDF CONSTANTS

contains

  ! Helper functions to convert between C and Fortran strings
  ! Based on the routines by Joseph M. Krahn
  function f_to_c_string(f_string) result(c_string)
    character(len=*), intent(in) :: f_string
    character(kind=c_char,len=1) :: c_string(len_trim(f_string)+1)
      
    integer :: i, strlen

    strlen = len_trim(f_string)

    forall (i=1:strlen)
      c_string(i) = f_string(i:i)
    end forall
    c_string(strlen+1) = C_NULL_CHAR

  end function f_to_c_string

  subroutine c_to_f_string(c_string, f_string)
    character(kind=c_char,len=1), intent(in)  :: c_string(*)
    character(len=*),             intent(out) :: f_string

    integer :: i

    i = 1
    do while(c_string(i) /= C_NULL_CHAR .and. i <= len(f_string))
      f_string(i:i) = c_string(i)
      i = i + 1
    end do
    if (i < len(f_string)) f_string(i:) = ' '

  end subroutine c_to_f_string

  subroutine c_to_f_string_ptr(c_string, f_string)
    type(c_ptr),      intent(in)  :: c_string
    character(len=*), intent(out) :: f_string

    character(len=1, kind=c_char), pointer :: p_chars(:)
    integer :: i

    if (.not. c_associated(c_string)) then
      f_string = ' '
    else
      call c_f_pointer(c_string, p_chars, [huge(0)])
      i = 1
      do while(p_chars(i) /= C_NULL_CHAR .and. i <= len(f_string))
        f_string(i:i) = p_chars(i)
        i = i + 1
      end do
      if (i < len(f_string)) f_string(i:) = ' '
    end if

  end subroutine c_to_f_string_ptr

end module escdff
