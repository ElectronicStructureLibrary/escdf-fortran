
module escdff_system

    use, intrinsic :: iso_c_binding
    use escdff_common
    use escdff_handle

    implicit none

    private

    public :: &
&       escdff_system_close, &
&       escdff_system_close_group, &
&       escdff_system_create_group, &
&       escdff_system_free, &
&       escdff_system_get_bulk_regions_for_semi_infinite_dimension, &
&       escdff_system_get_dimension_types, &
&       escdff_system_get_embedded_system, &
&       escdff_system_get_lattice_vectors, &
&       escdff_system_get_number_of_physical_dimensions, &
&       escdff_system_get_number_of_sites
    public :: &
&       escdff_system_get_number_of_species, &
&       escdff_system_get_number_of_symmetry_operations, &
&       escdff_system_get_symmorphic, &
&       escdff_system_get_system_name, &
&       escdff_system_get_time_reversal_symmetry, &
&       escdff_system_new, &
&       escdff_system_open, &
&       escdff_system_open_group
    public :: &
&       escdff_system_read_metadata, &
&       escdff_system_set_bulk_regions_for_semi_infinite_dimension, &
&       escdff_system_set_dimension_types, &
&       escdff_system_set_embedded_system, &
&       escdff_system_set_lattice_vectors, &
&       escdff_system_set_number_of_physical_dimensions, &
&       escdff_system_set_number_of_sites, &
&       escdff_system_set_number_of_species, &
&       escdff_system_set_number_of_symmetry_operations
    public :: &
&       escdff_system_set_symmorphic, &
&       escdff_system_set_system_name, &
&       escdff_system_set_time_reversal_symmetry

! FIXME: not implemented
!    public :: &
!&       escdff_system_get_spacegroup_3D_number, &
!&       escdff_system_read_magnetic_moment_directions
!&       escdff_system_set_spacegroup_3D_number
!&       escdff_system_write_magnetic_moment_directions, &
!&       escdff_system_write_metadata

    type, public :: escdff_system_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type escdff_system_t

    interface

        ! Interface: system/close
        function escdf_system_close(system) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_close
            type(c_ptr) :: system
        end function escdf_system_close

        ! Interface: system/close_group
        function escdf_system_close_group(system) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_close_group
            type(c_ptr) :: system
        end function escdf_system_close_group

        ! Interface: system/create_group
        function escdf_system_create_group(system, handle, path) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_create_group
            type(c_ptr) :: system
            type(c_ptr) :: handle
            character(kind=c_char) :: path(*)
        end function escdf_system_create_group

        ! Interface: system/free
        subroutine escdf_system_free(system) &
&                   bind(c)
            import
            type(c_ptr) :: system
        end subroutine escdf_system_free

        ! Interface: system/get_bulk_regions_for_semi_infinite_dimension
        function escdf_system_get_bulk_regions_for_semi_infinite_dimension(system, bulk_regions_for_semi_infinite_dimension) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_bulk_regions_for_semi_infinite_dimension
            type(c_ptr), value :: system
            real(kind=c_double) :: bulk_regions_for_semi_infinite_dimension
        end function escdf_system_get_bulk_regions_for_semi_infinite_dimension

        ! Interface: system/get_dimension_types
        function escdf_system_get_dimension_types(system, dimension_types) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_dimension_types
            type(c_ptr), value :: system
            integer(kind=c_int) :: dimension_types
        end function escdf_system_get_dimension_types

        ! Interface: system/get_embedded_system
        function escdf_system_get_embedded_system(system, embedded_system) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_embedded_system
            type(c_ptr), value :: system
            integer(kind=c_int) :: embedded_system
        end function escdf_system_get_embedded_system

        ! Interface: system/get_lattice_vectors
        function escdf_system_get_lattice_vectors(system, lattice_vectors) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_lattice_vectors
            type(c_ptr), value :: system
            real(kind=c_double) :: lattice_vectors
        end function escdf_system_get_lattice_vectors

        ! Interface: system/get_number_of_physical_dimensions
        function escdf_system_get_number_of_physical_dimensions(system, number_of_physical_dimensions) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_number_of_physical_dimensions
            type(c_ptr), value :: system
            integer(kind=c_int) :: number_of_physical_dimensions
        end function escdf_system_get_number_of_physical_dimensions

        ! Interface: system/get_number_of_sites
        function escdf_system_get_number_of_sites(system, number_of_sites) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_number_of_sites
            type(c_ptr), value :: system
            integer(kind=c_int) :: number_of_sites
        end function escdf_system_get_number_of_sites

        ! Interface: system/get_number_of_species
        function escdf_system_get_number_of_species(system, number_of_species) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_number_of_species
            type(c_ptr), value :: system
            integer(kind=c_int) :: number_of_species
        end function escdf_system_get_number_of_species

        ! Interface: system/get_number_of_symmetry_operations
        function escdf_system_get_number_of_symmetry_operations(system, number_of_symmetry_operations) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_number_of_symmetry_operations
            type(c_ptr), value :: system
            integer(kind=c_int) :: number_of_symmetry_operations
        end function escdf_system_get_number_of_symmetry_operations

        ! Interface: system/get_spacegroup_3D_number
! FIXME: not implemented
!        function escdf_system_get_spacegroup_3D_number(system, spacegroup_3D_number) &
!&                   bind(c)
!            import
!            integer(kind=c_int) :: escdf_system_get_spacegroup_3D_number
!            type(c_ptr), value :: system
!            integer(kind=c_int) :: spacegroup_3D_number
!        end function escdf_system_get_spacegroup_3D_number

        ! Interface: system/get_symmorphic
        function escdf_system_get_symmorphic(system, symmorphic) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_symmorphic
            type(c_ptr), value :: system
            integer(kind=c_int) :: symmorphic
        end function escdf_system_get_symmorphic

        ! Interface: system/get_system_name
        function escdf_system_get_system_name(system, system_name) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_system_name
            type(c_ptr), value :: system
            character(kind=c_char) :: system_name(*)
        end function escdf_system_get_system_name

        ! Interface: system/get_time_reversal_symmetry
        function escdf_system_get_time_reversal_symmetry(system, time_reversal_symmetry) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_get_time_reversal_symmetry
            type(c_ptr), value :: system
            integer(kind=c_int) :: time_reversal_symmetry
        end function escdf_system_get_time_reversal_symmetry

        ! Interface: system/new
        function escdf_system_new() &
&                   bind(c)
            import
            type(c_ptr) :: escdf_system_new
        end function escdf_system_new

        ! Interface: system/open
        function escdf_system_open(handle, name) &
&                   bind(c)
            import
            type(c_ptr) :: escdf_system_open
            type(c_ptr), value :: handle
            character(kind=c_char) :: name(*)
        end function escdf_system_open

        ! Interface: system/open_group
        function escdf_system_open_group(system, handle, path) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_open_group
            type(c_ptr) :: system
            type(c_ptr) :: handle
            character(kind=c_char) :: path(*)
        end function escdf_system_open_group

        ! Interface: system/read_magnetic_moment_directions
! FIXME: not implemented
!        function escdf_system_read_magnetic_moment_directions(system, buffer, start, count, map) &
!&                   bind(c)
!            import
!            integer(kind=c_int) :: escdf_system_read_magnetic_moment_directions
!            type(c_ptr), value :: system
!            real(kind=c_double) :: buffer
!            integer(kind=c_int), value :: start
!            integer(kind=c_int), value :: count
!            integer(kind=c_int), value :: map
!        end function escdf_system_read_magnetic_moment_directions

        ! Interface: system/read_metadata
        function escdf_system_read_metadata(system) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_read_metadata
            type(c_ptr) :: system
        end function escdf_system_read_metadata

        ! Interface: system/set_bulk_regions_for_semi_infinite_dimension
        function escdf_system_set_bulk_regions_for_semi_infinite_dimension(system, bulk_regions_for_semi_infinite_dimension) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_bulk_regions_for_semi_infinite_dimension
            type(c_ptr) :: system
            real(kind=c_double), value :: bulk_regions_for_semi_infinite_dimension
        end function escdf_system_set_bulk_regions_for_semi_infinite_dimension

        ! Interface: system/set_dimension_types
        function escdf_system_set_dimension_types(system, dimension_types) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_dimension_types
            type(c_ptr) :: system
            integer(kind=c_int), value :: dimension_types
        end function escdf_system_set_dimension_types

        ! Interface: system/set_embedded_system
        function escdf_system_set_embedded_system(system, embedded_system) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_embedded_system
            type(c_ptr) :: system
            integer(kind=c_int) :: embedded_system
        end function escdf_system_set_embedded_system

        ! Interface: system/set_lattice_vectors
        function escdf_system_set_lattice_vectors(system, lattice_vectors) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_lattice_vectors
            type(c_ptr) :: system
            real(kind=c_double), value :: lattice_vectors
        end function escdf_system_set_lattice_vectors

        ! Interface: system/set_number_of_physical_dimensions
        function escdf_system_set_number_of_physical_dimensions(system, number_of_physical_dimensions) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_number_of_physical_dimensions
            type(c_ptr) :: system
            integer(kind=c_int) :: number_of_physical_dimensions
        end function escdf_system_set_number_of_physical_dimensions

        ! Interface: system/set_number_of_sites
        function escdf_system_set_number_of_sites(system, number_of_sites) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_number_of_sites
            type(c_ptr) :: system
            integer(kind=c_int) :: number_of_sites
        end function escdf_system_set_number_of_sites

        ! Interface: system/set_number_of_species
        function escdf_system_set_number_of_species(system, number_of_species) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_number_of_species
            type(c_ptr) :: system
            integer(kind=c_int) :: number_of_species
        end function escdf_system_set_number_of_species

        ! Interface: system/set_number_of_symmetry_operations
        function escdf_system_set_number_of_symmetry_operations(system, number_of_symmetry_operations) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_number_of_symmetry_operations
            type(c_ptr) :: system
            integer(kind=c_int) :: number_of_symmetry_operations
        end function escdf_system_set_number_of_symmetry_operations

        ! Interface: system/set_spacegroup_3D_number
! FIXME: not implemented
!        function escdf_system_set_spacegroup_3D_number(system, spacegroup_3D_number) &
!&                   bind(c)
!            import
!            integer(kind=c_int) :: escdf_system_set_spacegroup_3D_number
!            type(c_ptr) :: system
!            integer(kind=c_int) :: spacegroup_3D_number
!        end function escdf_system_set_spacegroup_3D_number

        ! Interface: system/set_symmorphic
        function escdf_system_set_symmorphic(system, symmorphic) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_symmorphic
            type(c_ptr) :: system
            integer(kind=c_int) :: symmorphic
        end function escdf_system_set_symmorphic

        ! Interface: system/set_system_name
        function escdf_system_set_system_name(system, system_name) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_system_name
            type(c_ptr) :: system
            character(kind=c_char) :: system_name(*)
        end function escdf_system_set_system_name

        ! Interface: system/set_time_reversal_symmetry
        function escdf_system_set_time_reversal_symmetry(system, time_reversal_symmetry) &
&                   bind(c)
            import
            integer(kind=c_int) :: escdf_system_set_time_reversal_symmetry
            type(c_ptr) :: system
            integer(kind=c_int) :: time_reversal_symmetry
        end function escdf_system_set_time_reversal_symmetry

        ! Interface: system/write_magnetic_moment_directions
! FIXME: not implemented
!        function escdf_system_write_magnetic_moment_directions(system, buffer, start, count, map) &
!&                   bind(c)
!            import
!            integer(kind=c_int) :: escdf_system_write_magnetic_moment_directions
!            type(c_ptr), value :: system
!            real(kind=c_double), value :: buffer
!            integer(kind=c_int), value :: start
!            integer(kind=c_int), value :: count
!            integer(kind=c_int), value :: map
!        end function escdf_system_write_magnetic_moment_directions

        ! Interface: system/write_metadata
! FIXME: not implemented
!        function escdf_system_write_metadata(system) &
!&                   bind(c)
!            import
!            integer(kind=c_int) :: escdf_system_write_metadata
!            type(c_ptr), value :: system
!        end function escdf_system_write_metadata

    end interface

contains

    ! API: system/close
    integer(kind=c_int) function escdff_system_close(system) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system

        ret = escdf_system_close(system%ptr)

    end function escdff_system_close

    ! API: system/close_group
    integer(kind=c_int) function escdff_system_close_group(system) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system

        ret = escdf_system_close_group(system%ptr)

    end function escdff_system_close_group

    ! API: system/create_group
    integer(kind=c_int) function escdff_system_create_group(system, handle, path) result(ret)

        use escdff_common

        implicit none

        type(escdff_system_t), intent(inout) :: system
        type(escdff_handle_t), intent(inout) :: handle
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_path = f_to_c_string(trim(path))
        ret = escdf_system_create_group(system%ptr, handle%ptr, c_path)

    end function escdff_system_create_group

    ! API: system/free
    subroutine escdff_system_free(system)

        implicit none

        type(escdff_system_t), intent(inout) :: system

        call escdf_system_free(system%ptr)

    end subroutine escdff_system_free

    ! API: system/get_bulk_regions_for_semi_infinite_dimension
    integer(kind=c_int) function &
&       escdff_system_get_bulk_regions_for_semi_infinite_dimension(system, &
&           bulk_regions_for_semi_infinite_dimension) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        double precision, intent(inout) :: bulk_regions_for_semi_infinite_dimension

        ret = escdf_system_get_bulk_regions_for_semi_infinite_dimension(system%ptr, bulk_regions_for_semi_infinite_dimension)

    end function escdff_system_get_bulk_regions_for_semi_infinite_dimension

    ! API: system/get_dimension_types
    integer(kind=c_int) function escdff_system_get_dimension_types(system, dimension_types) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer, intent(inout) :: dimension_types

        ret = escdf_system_get_dimension_types(system%ptr, dimension_types)

    end function escdff_system_get_dimension_types

    ! API: system/get_embedded_system
    integer(kind=c_int) function escdff_system_get_embedded_system(system, embedded_system) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: embedded_system

        ret = escdf_system_get_embedded_system(system%ptr, embedded_system)

    end function escdff_system_get_embedded_system

    ! API: system/get_lattice_vectors
    integer(kind=c_int) function escdff_system_get_lattice_vectors(system, lattice_vectors) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        double precision, intent(inout) :: lattice_vectors

        ret = escdf_system_get_lattice_vectors(system%ptr, lattice_vectors)

    end function escdff_system_get_lattice_vectors

    ! API: system/get_number_of_physical_dimensions
    integer(kind=c_int) function escdff_system_get_number_of_physical_dimensions(system, number_of_physical_dimensions) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: number_of_physical_dimensions

        ret = escdf_system_get_number_of_physical_dimensions(system%ptr, &
&           number_of_physical_dimensions)

    end function escdff_system_get_number_of_physical_dimensions

    ! API: system/get_number_of_sites
    integer(kind=c_int) function escdff_system_get_number_of_sites(system, number_of_sites) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: number_of_sites

        ret = escdf_system_get_number_of_sites(system%ptr, number_of_sites)

    end function escdff_system_get_number_of_sites

    ! API: system/get_number_of_species
    integer(kind=c_int) function escdff_system_get_number_of_species(system, number_of_species) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: number_of_species

        ret = escdf_system_get_number_of_species(system%ptr, number_of_species)

    end function escdff_system_get_number_of_species

    ! API: system/get_number_of_symmetry_operations
    integer(kind=c_int) function escdff_system_get_number_of_symmetry_operations(system, number_of_symmetry_operations) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: number_of_symmetry_operations

        ret = escdf_system_get_number_of_symmetry_operations(system%ptr, number_of_symmetry_operations)

    end function escdff_system_get_number_of_symmetry_operations

    ! API: system/get_spacegroup_3D_number
! FIXME: not implemented
!    integer(kind=c_int) function escdff_system_get_spacegroup_3D_number(system, spacegroup_3D_number) result(ret)
!
!        implicit none
!
!        type(escdff_system_t), intent(in) :: system
!        integer(kind=c_int), intent(inout) :: spacegroup_3D_number
!
!        ret = escdf_system_get_spacegroup_3D_number(system%ptr, spacegroup_3D_number)
!
!    end function escdff_system_get_spacegroup_3D_number

    ! API: system/get_symmorphic
    integer(kind=c_int) function escdff_system_get_symmorphic(system, symmorphic) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: symmorphic

        ret = escdf_system_get_symmorphic(system%ptr, symmorphic)

    end function escdff_system_get_symmorphic

    ! API: system/get_system_name
    integer(kind=c_int) function escdff_system_get_system_name(system, system_name) result(ret)

        use escdff_common

        implicit none

        type(escdff_system_t), intent(in) :: system
        character(len=*), intent(inout) :: system_name

        character(kind=c_char) :: c_system_name(len_trim(system_name)+1)

        c_system_name = f_to_c_string(trim(system_name))
        ret = escdf_system_get_system_name(system%ptr, c_system_name)

    end function escdff_system_get_system_name

    ! API: system/get_time_reversal_symmetry
    integer(kind=c_int) function escdff_system_get_time_reversal_symmetry(system, time_reversal_symmetry) result(ret)

        implicit none

        type(escdff_system_t), intent(in) :: system
        integer(kind=c_int), intent(inout) :: time_reversal_symmetry

        ret = escdf_system_get_time_reversal_symmetry(system%ptr, time_reversal_symmetry)

    end function escdff_system_get_time_reversal_symmetry

    ! API: system/new
    type(escdff_system_t) function escdff_system_new() result(ret)

        implicit none

        ret%ptr = escdf_system_new()

    end function escdff_system_new

    ! API: system/open
    type(escdff_system_t) function escdff_system_open(handle, name) result(ret)

        use escdff_common

        implicit none

        type(escdff_handle_t), intent(in) :: handle
        character(len=*), intent(in) :: name

        character(kind=c_char) :: c_name(len_trim(name)+1)

        c_name = f_to_c_string(trim(name))
        ret%ptr = escdf_system_open(handle%ptr, c_name)

    end function escdff_system_open

    ! API: system/open_group
    integer(kind=c_int) function escdff_system_open_group(system, handle, path) result(ret)

        use escdff_common

        implicit none

        type(escdff_system_t), intent(inout) :: system
        type(escdff_handle_t), intent(inout) :: handle
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_path(len_trim(path)+1)

        c_path = f_to_c_string(trim(path))
        ret = escdf_system_open_group(system%ptr, handle%ptr, c_path)

    end function escdff_system_open_group

    ! API: system/read_magnetic_moment_directions
! FIXME: not implemented
!    integer(kind=c_int) function escdff_system_read_magnetic_moment_directions(system, buffer, start, count, map) result(ret)
!
!        implicit none
!
!        type(escdff_system_t), intent(in) :: system
!        double precision, intent(inout) :: buffer
!        integer(kind=c_int), intent(in) :: start
!        integer(kind=c_int), intent(in) :: count
!        integer(kind=c_int), intent(in) :: map
!
!        ret = escdf_system_read_magnetic_moment_directions(system%ptr, buffer, start, count, map)
!
!    end function escdff_system_read_magnetic_moment_directions

    ! API: system/read_metadata
    integer(kind=c_int) function escdff_system_read_metadata(system) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system

        ret = escdf_system_read_metadata(system%ptr)

    end function escdff_system_read_metadata

    ! API: system/set_bulk_regions_for_semi_infinite_dimension
    integer(kind=c_int) function &
&       escdff_system_set_bulk_regions_for_semi_infinite_dimension(system, &
&           bulk_regions_for_semi_infinite_dimension) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        double precision, intent(in) :: bulk_regions_for_semi_infinite_dimension

        ret = escdf_system_set_bulk_regions_for_semi_infinite_dimension(system%ptr, bulk_regions_for_semi_infinite_dimension)

    end function escdff_system_set_bulk_regions_for_semi_infinite_dimension

    ! API: system/set_dimension_types
    integer(kind=c_int) function escdff_system_set_dimension_types(system, dimension_types) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer, intent(in) :: dimension_types

        ret = escdf_system_set_dimension_types(system%ptr, dimension_types)

    end function escdff_system_set_dimension_types

    ! API: system/set_embedded_system
    integer(kind=c_int) function escdff_system_set_embedded_system(system, embedded_system) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: embedded_system

        ret = escdf_system_set_embedded_system(system%ptr, embedded_system)

    end function escdff_system_set_embedded_system

    ! API: system/set_lattice_vectors
    integer(kind=c_int) function escdff_system_set_lattice_vectors(system, lattice_vectors) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        double precision, intent(in) :: lattice_vectors

        ret = escdf_system_set_lattice_vectors(system%ptr, lattice_vectors)

    end function escdff_system_set_lattice_vectors

    ! API: system/set_number_of_physical_dimensions
    integer(kind=c_int) function escdff_system_set_number_of_physical_dimensions(system, number_of_physical_dimensions) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: number_of_physical_dimensions

        ret = escdf_system_set_number_of_physical_dimensions(system%ptr, number_of_physical_dimensions)

    end function escdff_system_set_number_of_physical_dimensions

    ! API: system/set_number_of_sites
    integer(kind=c_int) function escdff_system_set_number_of_sites(system, number_of_sites) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: number_of_sites

        ret = escdf_system_set_number_of_sites(system%ptr, number_of_sites)

    end function escdff_system_set_number_of_sites

    ! API: system/set_number_of_species
    integer(kind=c_int) function escdff_system_set_number_of_species(system, number_of_species) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: number_of_species

        ret = escdf_system_set_number_of_species(system%ptr, number_of_species)

    end function escdff_system_set_number_of_species

    ! API: system/set_number_of_symmetry_operations
    integer(kind=c_int) function escdff_system_set_number_of_symmetry_operations(system, number_of_symmetry_operations) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: number_of_symmetry_operations

        ret = escdf_system_set_number_of_symmetry_operations(system%ptr, &
&           number_of_symmetry_operations)

    end function escdff_system_set_number_of_symmetry_operations

    ! API: system/set_spacegroup_3D_number
! FIXME: not implemented
!    integer(kind=c_int) function escdff_system_set_spacegroup_3D_number(system, spacegroup_3D_number) result(ret)
!
!        implicit none
!
!        type(escdff_system_t), intent(inout) :: system
!        integer(kind=c_int), intent(in) :: spacegroup_3D_number
!
!        ret = escdf_system_set_spacegroup_3D_number(system%ptr, &
!&           spacegroup_3D_number)
!
!    end function escdff_system_set_spacegroup_3D_number

    ! API: system/set_symmorphic
    integer(kind=c_int) function escdff_system_set_symmorphic(system, symmorphic) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: symmorphic

        ret = escdf_system_set_symmorphic(system%ptr, symmorphic)

    end function escdff_system_set_symmorphic

    ! API: system/set_system_name
    integer(kind=c_int) function escdff_system_set_system_name(system, system_name) result(ret)

        use escdff_common

        implicit none

        type(escdff_system_t), intent(inout) :: system
        character(len=*), intent(in) :: system_name

        character(kind=c_char) :: c_system_name(len_trim(system_name)+1)

        c_system_name = f_to_c_string(trim(system_name))
        ret = escdf_system_set_system_name(system%ptr, c_system_name)

    end function escdff_system_set_system_name

    ! API: system/set_time_reversal_symmetry
    integer(kind=c_int) function escdff_system_set_time_reversal_symmetry(system, time_reversal_symmetry) result(ret)

        implicit none

        type(escdff_system_t), intent(inout) :: system
        integer(kind=c_int), intent(in) :: time_reversal_symmetry

        ret = escdf_system_set_time_reversal_symmetry(system%ptr, time_reversal_symmetry)

    end function escdff_system_set_time_reversal_symmetry

    ! API: system/write_magnetic_moment_directions
! FIXME: not implemented
!    integer(kind=c_int) function escdff_system_write_magnetic_moment_directions(system, buffer, start, count, map) result(ret)
!
!        implicit none
!
!        type(escdff_system_t), intent(in) :: system
!        double precision, intent(in) :: buffer
!        integer(kind=c_int), intent(in) :: start
!        integer(kind=c_int), intent(in) :: count
!        integer(kind=c_int), intent(in) :: map
!
!        ret = escdf_system_write_magnetic_moment_directions(system%ptr, &
!&           buffer, start, count, map)
!
!    end function escdff_system_write_magnetic_moment_directions

    ! API: system/write_metadata
! FIXME: not implemented
!    integer(kind=c_int) function escdff_system_write_metadata(system) result(ret)
!
!        implicit none
!
!        type(escdff_system_t), intent(in) :: system
!
!        ret = escdf_system_write_metadata(system%ptr)
!
!    end function escdff_system_write_metadata

end module escdff_system
