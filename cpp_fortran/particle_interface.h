#ifndef PARTICLE_INTERFACE_H
#define PARTICLE_INTERFACE_H

extern "C" {

void particle_system_init(int n);
void particle_system_destroy();
void particle_system_run_iteration();
void particle_system_particle(int idx, double* x, double* y, double* z);
int particle_system_particle_count();

}

/*
subroutine particle_intf_system_init(n) bind(C, name='init_particle_system')

    integer(c_int), value :: n

    allocate(p_particle_system)
    allocate(p_particle_simulation)

    call p_particle_system % init(1000)
    call p_particle_simulation % init(p_particle_system)

end subroutine

subroutine particle_intf_system_run(n_iterations)

    integer(c_int), value :: n_iterations

    call p_particle_simulation % run(n_iterations)

end subroutine

subroutine particle_intf_system_destroy()

    deallocate(p_particle_simulation)
    deallocate(p_particle_system)

end subroutine
*/

#endif // PARTICLE_INTERFACE_H
