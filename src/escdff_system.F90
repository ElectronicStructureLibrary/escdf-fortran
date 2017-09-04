module escdff_system

    use, intrinsic :: iso_c_binding
    use escdff_common

    implicit none

    private

    public :: &
        escdff_system_t, &
        escdff_system_new, &
        escdff_system_read_metadata, &
        escdff_system_write_metadata, &
        escdff_system_free, &
        escdff_system_set_file
    public :: &
        escdf_system_get_absolute_or_reduced_coordinates, &
        escdf_system_is_set_absolute_or_reduced_coordinates, &
        escdf_system_set_absolute_or_reduced_coordinates, &
        escdf_system_get_embedded_system, &
        escdf_system_is_set_embedded_system, &
        escdf_system_set_embedded_system, &
        escdf_system_get_number_of_physical_dimensions, &
        escdf_system_is_set_number_of_physical_dimensions, &
        escdf_system_set_number_of_physical_dimensions, &
        escdf_system_get_number_of_sites
    public :: &
        escdf_system_is_set_number_of_sites, &
        escdf_system_set_number_of_sites, &
        escdf_system_get_number_of_species, &
        escdf_system_is_set_number_of_species, &
        escdf_system_set_number_of_species, &
        escdf_system_get_number_of_symmetry_operations, &
        escdf_system_is_set_number_of_symmetry_operations, &
        escdf_system_set_number_of_symmetry_operations, &
        escdf_system_get_space_group, &
        escdf_system_is_set_space_group
    public :: &
        escdf_system_set_space_group, &
        escdf_system_get_symmorphic, &
        escdf_system_is_set_symmorphic, &
        escdf_system_set_symmorphic, &
        escdf_system_get_time_reversal_symmetry, &
        escdf_system_is_set_time_reversal_symmetry, &
        escdf_system_set_time_reversal_symmetry, &
        escdf_system_read_atomic_numbers, &
        escdf_system_write_atomic_numbers, &
        escdf_system_read_bulk_regions_for_semi_infinite_dimension
    public :: &
        escdf_system_write_bulk_regions_for_semi_infinite_dimension, &
        escdf_system_read_cell_in_host, &
        escdf_system_write_cell_in_host, &
        escdf_system_read_chemical_symbols, &
        escdf_system_write_chemical_symbols, &
        escdf_system_read_concentration_of_species_at_site, &
        escdf_system_write_concentration_of_species_at_site, &
        escdf_system_read_dimension_types, &
        escdf_system_write_dimension_types, &
        escdf_system_read_forces
    public :: &
        escdf_system_write_forces, &
        escdf_system_read_lattice_vectors, &
        escdf_system_write_lattice_vectors, &
        escdf_system_read_local_rotations, &
        escdf_system_write_local_rotations, &
        escdf_system_read_magnetic_moments, &
        escdf_system_write_magnetic_moments, &
        escdf_system_read_number_of_species_at_site, &
        escdf_system_write_number_of_species_at_site, &
        escdf_system_read_reduced_symmetry_matrices
    public :: &
        escdf_system_write_reduced_symmetry_matrices, &
        escdf_system_read_reduced_symmetry_translations, &
        escdf_system_write_reduced_symmetry_translations, &
        escdf_system_read_site_in_host, &
        escdf_system_write_site_in_host, &
        escdf_system_read_site_positions, &
        escdf_system_write_site_positions, &
        escdf_system_read_site_regions, &
        escdf_system_write_site_regions, &
        escdf_system_read_species_at_sites
    public :: &
        escdf_system_write_species_at_sites, &
        escdf_system_read_species_names, &
        escdf_system_write_species_names, &
        escdf_system_read_stress_tensor, &
        escdf_system_write_stress_tensor

    type :: escdff_system_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type escdff_system_t

    interface
        subroutine escdf_system_new(system), bind(c)
            import
            type(c_ptr) :: system
        end subroutine escdf_system_new
    end interface

    interface
        subroutine escdf_system_read_metadata(system), bind(c)
            import
            type(c_ptr) :: system
        end subroutine escdf_system_read_metadata
    end interface

    interface
        subroutine escdf_system_write_metadata(system), bind(c)
            import
            type(c_ptr) :: system
        end subroutine escdf_system_write_metadata
    end interface

    interface
        subroutine escdf_system_free(system), bind(c)
            import
            type(c_ptr) :: system
        end subroutine escdf_system_free
    end interface

    interface
        subroutine escdf_system_read_absolute_or_reduced_coordinates(system,  &
    & absolute_or_reduced_coordinates)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: absolute_or_reduced_coordinates
        end subroutine escdf_system_read_absolute_or_reduced_coordinates
    end interface
    
    interface
        subroutine escdf_system_write_absolute_or_reduced_coordinates(system,  &
    & absolute_or_reduced_coordinates)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: absolute_or_reduced_coordinates
        end subroutine escdf_system_write_absolute_or_reduced_coordinates
    end interface
    
    interface
        subroutine escdf_system_read_embedded_system(system, embedded_system)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            character(len=*), intent(inout) :: embedded_system
        end subroutine escdf_system_read_embedded_system
    end interface
    
    interface
        subroutine escdf_system_write_embedded_system(system, embedded_system)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            character(len=*), intent(in) :: embedded_system
        end subroutine escdf_system_write_embedded_system
    end interface
    
    interface
        subroutine escdf_system_read_number_of_physical_dimensions(system,  &
    & number_of_physical_dimensions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: number_of_physical_dimensions
        end subroutine escdf_system_read_number_of_physical_dimensions
    end interface
    
    interface
        subroutine escdf_system_write_number_of_physical_dimensions(system,  &
    & number_of_physical_dimensions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: number_of_physical_dimensions
        end subroutine escdf_system_write_number_of_physical_dimensions
    end interface
    
    interface
        subroutine escdf_system_read_number_of_sites(system, number_of_sites)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: number_of_sites
        end subroutine escdf_system_read_number_of_sites
    end interface
    
    interface
        subroutine escdf_system_write_number_of_sites(system, number_of_sites)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: number_of_sites
        end subroutine escdf_system_write_number_of_sites
    end interface
    
    interface
        subroutine escdf_system_read_number_of_species(system, number_of_species)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: number_of_species
        end subroutine escdf_system_read_number_of_species
    end interface
    
    interface
        subroutine escdf_system_write_number_of_species(system,  &
    & number_of_species)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: number_of_species
        end subroutine escdf_system_write_number_of_species
    end interface
    
    interface
        subroutine escdf_system_read_number_of_symmetry_operations(system,  &
    & number_of_symmetry_operations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: number_of_symmetry_operations
        end subroutine escdf_system_read_number_of_symmetry_operations
    end interface
    
    interface
        subroutine escdf_system_write_number_of_symmetry_operations(system,  &
    & number_of_symmetry_operations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: number_of_symmetry_operations
        end subroutine escdf_system_write_number_of_symmetry_operations
    end interface
    
    interface
        subroutine escdf_system_read_space_group(system, space_group)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, intent(inout) :: space_group
        end subroutine escdf_system_read_space_group
    end interface
    
    interface
        subroutine escdf_system_write_space_group(system, space_group)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, intent(in) :: space_group
        end subroutine escdf_system_write_space_group
    end interface
    
    interface
        subroutine escdf_system_read_symmorphic(system, symmorphic)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            character(len=*), intent(inout) :: symmorphic
        end subroutine escdf_system_read_symmorphic
    end interface
    
    interface
        subroutine escdf_system_write_symmorphic(system, symmorphic)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            character(len=*), intent(in) :: symmorphic
        end subroutine escdf_system_write_symmorphic
    end interface
    
    interface
        subroutine escdf_system_read_time_reversal_symmetry(system,  &
    & time_reversal_symmetry)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            character(len=*), intent(inout) :: time_reversal_symmetry
        end subroutine escdf_system_read_time_reversal_symmetry
    end interface
    
    interface
        subroutine escdf_system_write_time_reversal_symmetry(system,  &
    & time_reversal_symmetry)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            character(len=*), intent(in) :: time_reversal_symmetry
        end subroutine escdf_system_write_time_reversal_symmetry
    end interface
    
    interface
        subroutine escdf_system_read_atomic_numbers(system, atomic_numbers)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: atomic_numbers
        end subroutine escdf_system_read_atomic_numbers
    end interface
    
    interface
        subroutine escdf_system_write_atomic_numbers(system, atomic_numbers)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: atomic_numbers
        end subroutine escdf_system_write_atomic_numbers
    end interface
    
    interface
        subroutine  &
    & escdf_system_read_bulk_regions_for_semi_infinite_dimension(system,  &
    & bulk_regions_for_semi_infinite_dimension)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: bulk_regions_for_semi_infinite_dimension
        end subroutine escdf_system_read_bulk_regions_for_semi_infinite_dimension
    end interface
    
    interface
        subroutine  &
    & escdf_system_write_bulk_regions_for_semi_infinite_dimension(system,  &
    & bulk_regions_for_semi_infinite_dimension)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: bulk_regions_for_semi_infinite_dimension
        end subroutine  &
    & escdf_system_write_bulk_regions_for_semi_infinite_dimension
    end interface
    
    interface
        subroutine escdf_system_read_cell_in_host(system, cell_in_host)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: cell_in_host
        end subroutine escdf_system_read_cell_in_host
    end interface
    
    interface
        subroutine escdf_system_write_cell_in_host(system, cell_in_host)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: cell_in_host
        end subroutine escdf_system_write_cell_in_host
    end interface
    
    interface
        subroutine escdf_system_read_chemical_symbols(system, chemical_symbols)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            character(len=*), pointer :: chemical_symbols
        end subroutine escdf_system_read_chemical_symbols
    end interface
    
    interface
        subroutine escdf_system_write_chemical_symbols(system, chemical_symbols)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            character(len=*), pointer :: chemical_symbols
        end subroutine escdf_system_write_chemical_symbols
    end interface
    
    interface
        subroutine escdf_system_read_concentration_of_species_at_site(system,  &
    & concentration_of_species_at_site)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: concentration_of_species_at_site
        end subroutine escdf_system_read_concentration_of_species_at_site
    end interface
    
    interface
        subroutine escdf_system_write_concentration_of_species_at_site(system,  &
    & concentration_of_species_at_site)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: concentration_of_species_at_site
        end subroutine escdf_system_write_concentration_of_species_at_site
    end interface
    
    interface
        subroutine escdf_system_read_dimension_types(system, dimension_types)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: dimension_types
        end subroutine escdf_system_read_dimension_types
    end interface
    
    interface
        subroutine escdf_system_write_dimension_types(system, dimension_types)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: dimension_types
        end subroutine escdf_system_write_dimension_types
    end interface
    
    interface
        subroutine escdf_system_read_forces(system, forces)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: forces
        end subroutine escdf_system_read_forces
    end interface
    
    interface
        subroutine escdf_system_write_forces(system, forces)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: forces
        end subroutine escdf_system_write_forces
    end interface
    
    interface
        subroutine escdf_system_read_lattice_vectors(system, lattice_vectors)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: lattice_vectors
        end subroutine escdf_system_read_lattice_vectors
    end interface
    
    interface
        subroutine escdf_system_write_lattice_vectors(system, lattice_vectors)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: lattice_vectors
        end subroutine escdf_system_write_lattice_vectors
    end interface
    
    interface
        subroutine escdf_system_read_local_rotations(system, local_rotations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: local_rotations
        end subroutine escdf_system_read_local_rotations
    end interface
    
    interface
        subroutine escdf_system_write_local_rotations(system, local_rotations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: local_rotations
        end subroutine escdf_system_write_local_rotations
    end interface
    
    interface
        subroutine escdf_system_read_magnetic_moments(system, magnetic_moments)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: magnetic_moments
        end subroutine escdf_system_read_magnetic_moments
    end interface
    
    interface
        subroutine escdf_system_write_magnetic_moments(system, magnetic_moments)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: magnetic_moments
        end subroutine escdf_system_write_magnetic_moments
    end interface
    
    interface
        subroutine escdf_system_read_number_of_species_at_site(system,  &
    & number_of_species_at_site)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: number_of_species_at_site
        end subroutine escdf_system_read_number_of_species_at_site
    end interface
    
    interface
        subroutine escdf_system_write_number_of_species_at_site(system,  &
    & number_of_species_at_site)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: number_of_species_at_site
        end subroutine escdf_system_write_number_of_species_at_site
    end interface
    
    interface
        subroutine escdf_system_read_reduced_symmetry_matrices(system,  &
    & reduced_symmetry_matrices)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: reduced_symmetry_matrices
        end subroutine escdf_system_read_reduced_symmetry_matrices
    end interface
    
    interface
        subroutine escdf_system_write_reduced_symmetry_matrices(system,  &
    & reduced_symmetry_matrices)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: reduced_symmetry_matrices
        end subroutine escdf_system_write_reduced_symmetry_matrices
    end interface
    
    interface
        subroutine escdf_system_read_reduced_symmetry_translations(system,  &
    & reduced_symmetry_translations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: reduced_symmetry_translations
        end subroutine escdf_system_read_reduced_symmetry_translations
    end interface
    
    interface
        subroutine escdf_system_write_reduced_symmetry_translations(system,  &
    & reduced_symmetry_translations)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: reduced_symmetry_translations
        end subroutine escdf_system_write_reduced_symmetry_translations
    end interface
    
    interface
        subroutine escdf_system_read_site_in_host(system, site_in_host)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: site_in_host
        end subroutine escdf_system_read_site_in_host
    end interface
    
    interface
        subroutine escdf_system_write_site_in_host(system, site_in_host)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: site_in_host
        end subroutine escdf_system_write_site_in_host
    end interface
    
    interface
        subroutine escdf_system_read_site_positions(system, site_positions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: site_positions
        end subroutine escdf_system_read_site_positions
    end interface
    
    interface
        subroutine escdf_system_write_site_positions(system, site_positions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: site_positions
        end subroutine escdf_system_write_site_positions
    end interface
    
    interface
        subroutine escdf_system_read_site_regions(system, site_regions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: site_regions
        end subroutine escdf_system_read_site_regions
    end interface
    
    interface
        subroutine escdf_system_write_site_regions(system, site_regions)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: site_regions
        end subroutine escdf_system_write_site_regions
    end interface
    
    interface
        subroutine escdf_system_read_species_at_sites(system, species_at_sites)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            integer, pointer :: species_at_sites
        end subroutine escdf_system_read_species_at_sites
    end interface
    
    interface
        subroutine escdf_system_write_species_at_sites(system, species_at_sites)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            integer, pointer :: species_at_sites
        end subroutine escdf_system_write_species_at_sites
    end interface
    
    interface
        subroutine escdf_system_read_species_names(system, species_names)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            character(len=*), pointer :: species_names
        end subroutine escdf_system_read_species_names
    end interface
    
    interface
        subroutine escdf_system_write_species_names(system, species_names)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            character(len=*), pointer :: species_names
        end subroutine escdf_system_write_species_names
    end interface
    
    interface
        subroutine escdf_system_read_stress_tensor(system, stress_tensor)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(inout) :: system
            double precision, pointer :: stress_tensor
        end subroutine escdf_system_read_stress_tensor
    end interface
    
    interface
        subroutine escdf_system_write_stress_tensor(system, stress_tensor)
            use iso_c_binding
            implicit none
            type(c_ptr), intent(in) :: system
            double precision, pointer :: stress_tensor
        end subroutine escdf_system_write_stress_tensor
    end interface

contains

    subroutine escdff_system_new(system, path, mode)
        type(escdff_system_t), intent(inout) :: system
        character(kind=c_char, len=*), intent(in) :: path
        character(kind=c_char, len=*), intent(in) :: mode

        system%ptr = escdf_system_new(path, mode)

    end subroutine escdff_system_new

    subroutine escdff_system_read_metadata(system)
        type(escdff_system_t), intent(inout) :: system

        call escdf_system_read_metadata(system%ptr)

    end subroutine escdff_system_read_metadata

    subroutine escdff_system_write_metadata(system)
        type(escdff_system_t), intent(in) :: system

        call escdf_system_write_metadata(system%ptr)

    end subroutine escdff_system_write_metadata

    subroutine escdff_system_free(system)
        type(escdff_system_t), intent(inout) :: system

        call escdf_system_free(system%ptr)
        system%ptr = C_NULL_PTR

    end subroutine escdff_system_free

    ! TODO: write wrappers for setters and getters

end module escdff_system
