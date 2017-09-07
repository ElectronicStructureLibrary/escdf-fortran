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

module escdff_geometry

    use, intrinsic :: iso_c_binding
    use escdff_common
    use escdff_handle

    implicit none

    private

    public :: &
&       escdff_geometry_close, &
&       escdff_geometry_close_group, &
&       escdff_geometry_create_group, &
&       escdff_geometry_free, &
&       escdff_geometry_get_dimension_types, &
&       escdff_geometry_get_number_of_physical_dimensions, &
&       escdff_geometry_is_set_number_of_physical_dimensions, &
&       escdff_geometry_new, &
&       escdff_geometry_open, &
&       escdff_geometry_open_group
    public :: &
&       escdff_geometry_ptr_dimension_types, &
&       escdff_geometry_read_magnetic_moment_directions, &
&       escdff_geometry_read_metadata, &
&       escdff_geometry_set_dimension_types, &
&       escdff_geometry_set_number_of_physical_dimensions, &
&       escdff_geometry_write_magnetic_moment_directions, &
&       escdff_geometry_write_metadata

    type, public :: escdff_geometry_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type escdff_geometry_t

    interface

        ! Interface: geometry/close
        function escdf_geometry_close(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_close
            type(c_ptr) :: geometry
        end function escdf_geometry_close

        ! Interface: geometry/close_group
        function escdf_geometry_close_group(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_close_group
            type(c_ptr) :: geometry
        end function escdf_geometry_close_group

        ! Interface: geometry/create_group
        function escdf_geometry_create_group(geometry, handle, path) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_create_group
            type(c_ptr) :: geometry
            type(c_ptr) :: handle
            character(kind=c_char) :: path(*)
        end function escdf_geometry_create_group

        ! Interface: geometry/free
        subroutine escdf_geometry_free(geometry) &
        &           bind(c)
            import
            type(c_ptr) :: geometry
        end subroutine escdf_geometry_free

        ! Interface: geometry/get_dimension_types
        function escdf_geometry_get_dimension_types(geometry, dimension_types, len) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_get_dimension_types
            type(c_ptr), value :: geometry
            integer(kind=c_int) :: dimension_types
            integer(kind=c_int), value :: len
        end function escdf_geometry_get_dimension_types

        ! Interface: geometry/get_number_of_physical_dimensions
        function escdf_geometry_get_number_of_physical_dimensions(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_get_number_of_physical_dimensions
            type(c_ptr), value :: geometry
        end function escdf_geometry_get_number_of_physical_dimensions

        ! Interface: geometry/is_set_number_of_physical_dimensions
        function escdf_geometry_is_set_number_of_physical_dimensions(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_is_set_number_of_physical_dimensions
            type(c_ptr), value :: geometry
        end function escdf_geometry_is_set_number_of_physical_dimensions

        ! Interface: geometry/new
        function escdf_geometry_new() &
        &           bind(c)
            import
            type(c_ptr) :: escdf_geometry_new
        end function escdf_geometry_new

        ! Interface: geometry/open
        function escdf_geometry_open(handle, name) &
        &           bind(c)
            import
            type(c_ptr) :: escdf_geometry_open
            type(c_ptr), value :: handle
            character(kind=c_char) :: name(*)
        end function escdf_geometry_open

        ! Interface: geometry/open_group
        function escdf_geometry_open_group(geometry, handle, path) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_open_group
            type(c_ptr) :: geometry
            type(c_ptr) :: handle
            character(kind=c_char) :: path(*)
        end function escdf_geometry_open_group

        ! Interface: geometry/ptr_dimension_types
        function escdf_geometry_ptr_dimension_types(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_ptr_dimension_types
            type(c_ptr), value :: geometry
        end function escdf_geometry_ptr_dimension_types

        ! Interface: geometry/read_magnetic_moment_directions
        function escdf_geometry_read_magnetic_moment_directions(geometry, buffer, start, count, map) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_read_magnetic_moment_directions
            type(c_ptr), value :: geometry
            real(kind=c_double) :: buffer
            integer(kind=c_int), value :: start
            integer(kind=c_int), value :: count
            integer(kind=c_int), value :: map
        end function escdf_geometry_read_magnetic_moment_directions

        ! Interface: geometry/read_metadata
        function escdf_geometry_read_metadata(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_read_metadata
            type(c_ptr) :: geometry
        end function escdf_geometry_read_metadata

        ! Interface: geometry/set_dimension_types
        function escdf_geometry_set_dimension_types(geometry, dimension_types, len) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_set_dimension_types
            type(c_ptr) :: geometry
            integer(kind=c_int), value :: dimension_types
            integer(kind=c_int), value :: len
        end function escdf_geometry_set_dimension_types

        ! Interface: geometry/set_number_of_physical_dimensions
        function escdf_geometry_set_number_of_physical_dimensions(geometry, number_of_physical_dimensions) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_set_number_of_physical_dimensions
            type(c_ptr) :: geometry
            integer(kind=c_int), value :: number_of_physical_dimensions
        end function escdf_geometry_set_number_of_physical_dimensions

        ! Interface: geometry/write_magnetic_moment_directions
        function escdf_geometry_write_magnetic_moment_directions(geometry, buffer, start, count, map) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_write_magnetic_moment_directions
            type(c_ptr), value :: geometry
            real(kind=c_double), value :: buffer
            integer(kind=c_int), value :: start
            integer(kind=c_int), value :: count
            integer(kind=c_int), value :: map
        end function escdf_geometry_write_magnetic_moment_directions

        ! Interface: geometry/write_metadata
        function escdf_geometry_write_metadata(geometry) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_geometry_write_metadata
            type(c_ptr), value :: geometry
        end function escdf_geometry_write_metadata

    end interface

contains

    ! API: geometry/close
    integer(kind=c_int) function escdff_geometry_close(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry

        ret = escdf_geometry_close(geometry%ptr)

    end function escdff_geometry_close

    ! API: geometry/close_group
    integer(kind=c_int) function escdff_geometry_close_group(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry

        ret = escdf_geometry_close_group(geometry%ptr)

    end function escdff_geometry_close_group

    ! API: geometry/create_group
    integer(kind=c_int) function escdff_geometry_create_group(geometry, handle, path) result(ret)

        use escdff_common

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry
        type(escdff_handle_t), intent(inout) :: handle
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_path = f_to_c_string(trim(path))
        ret = escdf_geometry_create_group(geometry%ptr, handle%ptr, c_path)

    end function escdff_geometry_create_group

    ! API: geometry/free
    subroutine escdff_geometry_free(geometry)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry

        call escdf_geometry_free(geometry%ptr)

    end subroutine escdff_geometry_free

    ! API: geometry/get_dimension_types
    integer(kind=c_int) function escdff_geometry_get_dimension_types(geometry, dimension_types, len) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry
        integer, intent(inout) :: dimension_types
        integer(kind=c_int), intent(in) :: len

        ret = escdf_geometry_get_dimension_types(geometry%ptr, dimension_types, len)

    end function escdff_geometry_get_dimension_types

    ! API: geometry/get_number_of_physical_dimensions
    integer function escdff_geometry_get_number_of_physical_dimensions(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry

        ret = escdf_geometry_get_number_of_physical_dimensions(geometry%ptr)

    end function escdff_geometry_get_number_of_physical_dimensions

    ! API: geometry/is_set_number_of_physical_dimensions
    integer(kind=c_int) function escdff_geometry_is_set_number_of_physical_dimensions(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry

        ret = escdf_geometry_is_set_number_of_physical_dimensions(geometry%ptr)

    end function escdff_geometry_is_set_number_of_physical_dimensions

    ! API: geometry/new
    type(escdff_geometry_t) function escdff_geometry_new() result(ret)

        implicit none

        ret%ptr = escdf_geometry_new()

    end function escdff_geometry_new

    ! API: geometry/open
    type(escdff_geometry_t) function escdff_geometry_open(handle, name) result(ret)

        use escdff_common

        implicit none

        type(escdff_handle_t), intent(in) :: handle
        character(len=*), intent(in) :: name

        character(kind=c_char) :: c_name(len_trim(name)+1)

        c_name = f_to_c_string(trim(name))
        ret%ptr = escdf_geometry_open(handle%ptr, c_name)

    end function escdff_geometry_open

    ! API: geometry/open_group
    integer(kind=c_int) function escdff_geometry_open_group(geometry, handle, path) result(ret)

        use escdff_common

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry
        type(escdff_handle_t), intent(inout) :: handle
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_path = f_to_c_string(trim(path))
        ret = escdf_geometry_open_group(geometry%ptr, handle%ptr, c_path)

    end function escdff_geometry_open_group

    ! API: geometry/ptr_dimension_types
    integer function escdff_geometry_ptr_dimension_types(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry

        ret = escdf_geometry_ptr_dimension_types(geometry%ptr)

    end function escdff_geometry_ptr_dimension_types

    ! API: geometry/read_magnetic_moment_directions
    integer(kind=c_int) function escdff_geometry_read_magnetic_moment_directions(geometry, buffer, start, count, map) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry
        double precision, intent(inout) :: buffer
        integer(kind=c_int), intent(in) :: start
        integer(kind=c_int), intent(in) :: count
        integer(kind=c_int), intent(in) :: map

        ret = escdf_geometry_read_magnetic_moment_directions(geometry%ptr, buffer, start, count, map)

    end function escdff_geometry_read_magnetic_moment_directions

    ! API: geometry/read_metadata
    integer(kind=c_int) function escdff_geometry_read_metadata(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry

        ret = escdf_geometry_read_metadata(geometry%ptr)

    end function escdff_geometry_read_metadata

    ! API: geometry/set_dimension_types
    integer(kind=c_int) function escdff_geometry_set_dimension_types(geometry, dimension_types, len) result(ret)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry
        integer, intent(in) :: dimension_types
        integer(kind=c_int), intent(in) :: len

        ret = escdf_geometry_set_dimension_types(geometry%ptr, dimension_types, len)

    end function escdff_geometry_set_dimension_types

    ! API: geometry/set_number_of_physical_dimensions
    integer(kind=c_int) function &
&       escdff_geometry_set_number_of_physical_dimensions(geometry, &
&           number_of_physical_dimensions) result(ret)

        implicit none

        type(escdff_geometry_t), intent(inout) :: geometry
        integer, intent(in) :: number_of_physical_dimensions

        ret = escdf_geometry_set_number_of_physical_dimensions(geometry%ptr, number_of_physical_dimensions)

    end function escdff_geometry_set_number_of_physical_dimensions

    ! API: geometry/write_magnetic_moment_directions
    integer(kind=c_int) function escdff_geometry_write_magnetic_moment_directions(geometry, buffer, start, count, map) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry
        double precision, intent(in) :: buffer
        integer(kind=c_int), intent(in) :: start
        integer(kind=c_int), intent(in) :: count
        integer(kind=c_int), intent(in) :: map

        ret = escdf_geometry_write_magnetic_moment_directions(geometry%ptr, buffer, start, count, map)

    end function escdff_geometry_write_magnetic_moment_directions

    ! API: geometry/write_metadata
    integer(kind=c_int) function escdff_geometry_write_metadata(geometry) result(ret)

        implicit none

        type(escdff_geometry_t), intent(in) :: geometry

        ret = escdf_geometry_write_metadata(geometry%ptr)

    end function escdff_geometry_write_metadata

end module escdff_geometry
