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

program write_system_metadata

    use escdff_system

    implicit none

    character(len=*), parameter :: filename = "example_system_nacl.escdf"

    integer :: ierr

    double precision :: znucl(2)
    double precision :: lattice_vectors(3,3)
    double precision :: reduced_coordinates(3,2)

    type(escdf_handle) :: my_handle
    type(escdff_system) :: my_sys

    ! Define unit cell
    lattice_vectors(:,:) = [[10.639, 0.0, 0.0], [0.0, 10.639, 0.0], [0.0, 0.0, 10.639]]
    reduced_coordinates(:,:) = [[0.0, 0.0, 0.0], [0.5, 0.5, 0.5]]
    ! rprim  0.0 0.5 0.5, 0.5 0.0 0.5, 0.5 0.5 0.0

    ! Define atoms
    znucl(:) = [11.0, 17.0]

    ! Fill-in system data structure
    my_sys = escdff_system_new()

    ! Create new ESCDF file
    ierr =

end program write_system_metadata
