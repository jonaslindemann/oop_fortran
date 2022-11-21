program myfortran

use mf_datatypes
use particle_classes

implicit none

class(ParticleSystem), pointer :: p_particle_system
class(ParticleSimulation), pointer:: p_particle_simulation

allocate(p_particle_system)
allocate(p_particle_simulation)

call p_particle_system % init(1000)
call p_particle_simulation % init(p_particle_system)

call p_particle_simulation % run(1000)

deallocate(p_particle_simulation)
deallocate(p_particle_system)

contains

end program myfortran
