module particle_classes

use mf_datatypes
use particle_utils

implicit none

type ParticleSystem
private
    integer(ik) :: m_n_particles
    real(dp) :: m_v0
    real(dp) :: m_rmin
    real(dp) :: m_rmax
    real(dp) :: m_dt
    real(dp), allocatable :: m_pos(:,:)
    real(dp), allocatable :: m_vel(:,:)
    real(dp), allocatable :: m_r(:)
contains
    procedure :: init            => particle_system_init
    procedure :: destroy         => particle_system_destroy

    procedure :: print           => particle_system_print
    procedure :: write_sizes     => particle_system_write_sizes
    procedure :: write_positions => particle_system_write_positions

    procedure :: count           => particle_system_count
    procedure :: v0              => particle_system_v0
    procedure :: rmin            => particle_system_rmin
    procedure :: rmax            => particle_system_rmax
    procedure :: dt              => particle_system_dt
    procedure :: positions       => particle_system_positions
    procedure :: velocities      => particle_system_velocities
    procedure :: radius          => particle_system_radius

    procedure :: particle        => particle_system_particle
    procedure :: particle_count  => particle_system_particle_count

end type

type ParticleSimulation
private
    class(ParticleSystem), pointer :: m_psys
contains
    procedure :: init              => particle_simulator_init
    procedure :: update            => particle_simulator_update
    procedure :: check_boundaries  => particle_simulator_check_boundaries
    procedure :: check_collisions  => particle_simulator_check_collisions
    procedure :: run               => particle_simulator_run
    procedure :: run_iteration     => particle_simulator_run_iteration
end type

contains

subroutine particle_system_init(this, n)

    class(ParticleSystem) :: this
    integer(int32), intent(in) :: n
    integer(ik) :: i
    real(dp) :: alfa

    this % m_n_particles = n
    allocate(this % m_pos(n,2))
    allocate(this % m_vel(n,2))
    allocate(this % m_r(n))

    this % m_v0 = 0.001_dp
    this % m_rmin = 0.005_dp
    this % m_rmax = 0.015_dp

    call init_random_seed()

    call random_number(this % m_pos)
    call random_number(this % m_r)

    do i=1,this % m_n_particles
        call random_number(alfa)
        alfa = 2.0_dp*pi*alfa
        this % m_vel(i,1) = this % m_v0 * cos(alfa)
        this % m_vel(i,2) = this % m_v0 * sin(alfa)
    end do

    this % m_r = this % m_rmin + this % m_r * (this % m_rmax - this % m_rmin)

end subroutine particle_system_init

subroutine particle_system_destroy(this)

    class(ParticleSystem) :: this

    deallocate(this % m_pos, this % m_vel, this % m_r)
    this % m_n_particles = -1

end subroutine

subroutine particle_system_print(this)

    class(ParticleSystem) :: this
    integer(ik) :: i

    print *, 'Max particle x coord = ', maxval(this % m_pos(:,1))
    print *, 'Min particle x coord = ', minval(this % m_pos(:,1))
    print *, 'Max particle y coord = ', maxval(this % m_pos(:,2))
    print *, 'Min particle y coord = ', minval(this % m_pos(:,2))

end subroutine particle_system_print

subroutine particle_system_write_sizes(this)

    class(ParticleSystem) :: this
    integer(ik) :: i

    open(unit=15, file='particle.state', access='APPEND')
    write(15, '(I10)') this % m_n_particles
    do i = 1,this % m_n_particles
            write(15, '(2F12.5)') this % m_r(i)
    end do
    close(unit=15)

end subroutine particle_system_write_sizes

subroutine particle_system_write_positions(this)

    class(ParticleSystem) :: this
    integer(ik) :: i

    open(unit=15, file='particle.state', access='APPEND')
    write(15, '(I10)') this % m_n_particles
    do i = 1,this % m_n_particles
            write(15, '(2F12.5)') this % m_pos(i,:)
    end do
    close(unit=15)

end subroutine particle_system_write_positions

real(dp) function particle_system_v0(this) result(v0)

    class(ParticleSystem) :: this

    v0 = this % m_v0

end function

real(dp) function particle_system_rmin(this) result(rmin)

    class(ParticleSystem) :: this

    rmin = this % m_rmin

end function

real(dp) function particle_system_rmax(this) result(rmax)

    class(ParticleSystem) :: this

    rmax = this % m_rmax

end function

real(dp) function particle_system_dt(this) result(dt)

    class(ParticleSystem) :: this

    dt   = this % m_dt

end function

function particle_system_positions(this) result(arr)

    class(ParticleSystem) :: this
    real(dp), allocatable :: arr(:,:)

    arr = this % m_pos

end function

function particle_system_velocities(this) result(arr)

    class(ParticleSystem) :: this
    real(dp), allocatable :: arr(:,:)

    arr = this % m_vel

end function

function particle_system_radius(this) result(arr)

    class(ParticleSystem) :: this
    real(dp), allocatable :: arr(:)

    arr = this % m_r

end function

function particle_system_count(this) result(count)

    class(ParticleSystem) :: this
    integer :: count

    count = this % m_n_particles

end function

subroutine particle_system_particle(this, idx, x, y, r)

    class(ParticleSystem) :: this
    integer, intent(in) :: idx
    real(dp), intent(out) :: x, y, r

    x = this % m_pos(idx, 1)
    y = this % m_pos(idx, 2)
    r = this % m_r(idx)

end subroutine

function particle_system_particle_count(this) result(count)

    class(ParticleSystem) :: this
    integer :: count

    count = size(this % m_pos, 1)

end function

! ---------------------------------------------------------------

subroutine particle_simulator_init(this, psys)

    class(ParticleSimulation) :: this
    class(ParticleSystem), pointer :: psys

    this % m_psys => psys

end subroutine particle_simulator_init

subroutine particle_simulator_update(this, dtin)

    class(ParticleSimulation) :: this

    real(dp), allocatable :: pos(:,:)
    real(dp), allocatable :: vel(:,:)
    real(dp), allocatable :: r(:)
    integer(ik) :: i
    real(dp), intent(in), optional :: dtin
    real(dp) :: dt

    pos = this % m_psys % positions()
    vel = this % m_psys % velocities()
    r = this % m_psys % radius()

    if (present(dtin)) then
            dt = dtin
    else
            dt = this % m_psys % rmin()/(3.0_dp*this % m_psys % v0())
    end if

    pos = pos + vel * dt
    print*, dt

end subroutine particle_simulator_update

subroutine particle_simulator_check_boundaries(this)

    class(ParticleSimulation) :: this
    real(dp), allocatable :: pos(:,:)
    real(dp), allocatable :: vel(:,:)
    real(dp), allocatable :: r(:)
    integer(ik) :: i

    pos = this % m_psys % positions()
    vel = this % m_psys % velocities()
    r = this % m_psys % radius()

    do i=1,this % m_psys % count()
        if (pos(i,1) < r(i)) vel(i,1) = -vel(i,1)
        if (pos(i,1)>1.0_dp-r(i)) vel(i,1) = -vel(i,1)
        if (pos(i,2)<r(i)) vel(i,2) = -vel(i,2)
        if (pos(i,2)>1.0_dp-r(i)) vel(i,2) = -vel(i,2)
    end do

end subroutine

subroutine particle_simulator_check_collisions(this)

    class(ParticleSimulation) :: this
    real(dp), allocatable :: pos(:,:)
    real(dp), allocatable :: vel(:,:)
    real(dp), allocatable :: r(:)

    integer(ik) :: i, j
    real(dp) :: d, r1, r2
    real(dp) :: vi(2), vj(2)
    real(dp) :: si(2), sj(2)
    real(dp) :: n(2), vdiff(2)
    real(dp) :: q

    pos = this % m_psys % positions()
    vel = this % m_psys % velocities()
    r = this % m_psys % radius()

    !       | -------------|
    !               d
    ! | --- o --- |   | -- o -- |
    !          r1       r2
    !
    ! collide is true if d < (r1+r2)

    do i=1,this % m_psys % count()
        do j=i+1,this % m_psys % count()
            n = pos(j,:) - pos(i,:)
            d = sqrt((pos(j,1)-pos(i,1))**2.0_dp+(pos(j,2)-pos(i,2))**2)
            vdiff = vel(j,:) - vel(i,:)
            if ((d<(r(i)+r(j))).and.(dot_product(n,vdiff)<0.0_dp)) then
                q = dot_product(vdiff,n)/dot_product(n,n)
                vel(i,:) = vel(i,:) + n * q
                vel(j,:) = vel(j,:) - n * q
            endif
        end do
    end do

end subroutine

subroutine particle_simulator_run(this, n_iterations)

    class(ParticleSimulation) :: this
    integer, intent(in) :: n_iterations
    integer :: i

    call this % m_psys % write_sizes()

    do i = 1,n_iterations
        call this % check_collisions()
        call this % check_boundaries()
        call this % update()
        call this % m_psys % write_positions()
    end do

end subroutine

subroutine particle_simulator_run_iteration(this)

    class(ParticleSimulation) :: this

    call this % check_collisions()
    call this % check_boundaries()
    call this % update()

end subroutine

end module particle_classes
