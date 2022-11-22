module particle_interface

use iso_c_binding
use mf_datatypes
use mf_utils
use particle_classes
use particle_utils

implicit none

class(ParticleSystem), pointer :: p_particle_system
class(ParticleSimulation), pointer:: p_particle_simulation

contains

subroutine particle_intf_system_init(n, min_radius, max_radius, v0) bind(C, name='particle_system_init')

    integer(c_int), value :: n
    real(c_double), value :: min_radius
    real(c_double), value :: max_radius
    real(c_double), value :: v0

    print*, 'Allocating particle system...'

    allocate(p_particle_system)
    allocate(p_particle_simulation)

    call p_particle_system % init(n, min_radius, max_radius, v0)
    call p_particle_simulation % init(p_particle_system)

end subroutine

subroutine particle_intf_system_particle_params(min_radius, max_radius, v0) bind(C, name='particle_system_params')

    real(c_double), intent(out) :: min_radius
    real(c_double), intent(out) :: max_radius
    real(c_double), intent(out) :: v0

    min_radius = p_particle_system % rmin()
    max_radius = p_particle_system % rmax()
    v0 = p_particle_system % v0()

end subroutine

subroutine particle_intf_system_particle(idx, x, y, r) bind(C, name='particle_system_particle')

    integer(c_int), value :: idx
    real(c_double), intent(out) :: x, y, r

    call p_particle_system % particle(idx, x, y, r)

end subroutine

function particle_intf_system_particle_count() result(count) bind(C, name='particle_system_particle_count')

    integer(c_int) :: count

    count = p_particle_system % particle_count()

end function

subroutine particle_intf_system_run_iteration() bind(C, name='particle_system_run_iteration')

    call p_particle_simulation % run_iteration()

end subroutine

subroutine particle_intf_system_destroy() bind(C, name='particle_system_destroy')

    print*, 'Deallocating particle system ...'

    deallocate(p_particle_simulation)
    deallocate(p_particle_system)

end subroutine

end module particle_interface
