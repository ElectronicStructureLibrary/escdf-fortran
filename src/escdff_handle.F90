!! Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>
!!
!! This file is part of ESCDF-Fortran.
!!
!! ESCDF-Fortran is free software: you can redistribute it and/or modify it
!! under the terms of the GNU Lesser General Public License as published by the
!! Free Software Foundation, version 2.1 of the License, or (at your option)
!! any later version.
!!
!! ESCDF-Fortran is distributed in the hope that it will be useful, but WITHOUT
!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!! FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
!! for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License
!! along with ESCDF-Fortran.  If not, see <http://www.gnu.org/licenses/> or
!! write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
!! Boston, MA  02110-1301  USA.

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

module escdff_handle

    use, intrinsic :: iso_c_binding
    use escdff_common

    implicit none

    private

    public :: &
&       escdff_close, &
&       escdff_create, &
&       escdff_open

    ! FIXME: any way to keep the pointer private?
    type, public :: escdff_handle_t
        type(c_ptr) :: ptr = C_NULL_PTR
    end type escdff_handle_t

    interface

        ! Interface: handle/escdf_close
        function escdf_close(handle) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_close
            type(c_ptr) :: handle
        end function escdf_close

        ! Interface: handle/escdf_create
        function escdf_create(filename, path) &
        &           bind(c)
            import
            type(c_ptr) :: escdf_create
            character(kind=c_char) :: filename(*)
            character(kind=c_char) :: path(*)
        end function escdf_create

        ! Interface: handle/escdf_open
        function escdf_open(filename, path) &
        &           bind(c)
            import
            type(c_ptr) :: escdf_open
            character(kind=c_char) :: filename(*)
            character(kind=c_char) :: path(*)
        end function escdf_open

    end interface

contains

    ! API: handle/escdf_close
    integer function escdff_close(handle) result(ret)

        implicit none

        type(escdff_handle_t), intent(inout) :: handle

        ret = escdf_close(handle%ptr)

    end function escdff_close

    ! API: handle/escdf_create
    type(escdff_handle_t) function escdff_create(filename, path) result(ret)

        use escdff_common

        implicit none

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_filename(len_trim(filename)+1)
        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_filename = f_to_c_string(trim(filename))
        c_path = f_to_c_string(trim(path))
        ret%ptr = escdf_create(c_filename, c_path)

    end function escdff_create

    ! API: handle/escdf_open
    type(escdff_handle_t) function escdff_open(filename, path) result(ret)

        use escdff_common

        implicit none

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_filename(len_trim(filename)+1)
        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_filename = f_to_c_string(trim(filename))
        c_path = f_to_c_string(trim(path))
        ret%ptr = escdf_open(c_filename, c_path)

    end function escdff_open

end module escdff_handle
