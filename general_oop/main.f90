program myfortran

    use shape_classes

implicit none

type simple_array_element
    type(shape_class), pointer :: element
end type

type shape_array_element
    class(shape_class), pointer :: element
end type

integer :: i

type(shape_class) :: s
type(shape_class), allocatable :: s2
type(square_class) :: sq
type(circle_class) :: c

! Instantiating a Particle object

type(particle_class) :: p

class(square_class), pointer :: p_square
class(circle_class), pointer :: p_circle
class(shape_class), pointer :: p_shape
type(shape_class), pointer :: p_simple_shape

type(simple_array_element), allocatable :: simple_shapes(:)
type(shape_array_element), allocatable :: shapes(:)
type(shape_class) :: static_shapes(20)
type(shape_class), allocatable :: dyn_shapes(:)

class(triangle_class), pointer :: p_triangle
class(point_class), pointer :: p_p0
class(point_class), pointer :: p_p1
class(point_class), pointer :: p_p2

! --- Simple examples

s = shape_class()
!call s % init()
call s % print()
call s % set_pos(2.0, 2.0)
call s % print()

call make_objects()

allocate(s2)

call s2 % init()
call s2 % print()

deallocate(s2)

call sq % init()
call sq % print()
call c % init()
call c % print()

! --- Static array of shapes

do i=1,20
    call static_shapes(i) % init()
    call static_shapes(i) % print()
end do

! --- Dynamic array of shapes

allocate(dyn_shapes(20))

do i=1,20
   call dyn_shapes(i) % init()
   call dyn_shapes(i) % print()
end do

deallocate(dyn_shapes)

! --- Dynamic array of dynamic objects

allocate(simple_shapes(20))

do i=1,20
    allocate(p_simple_shape)
    call p_simple_shape % init()
    simple_shapes(i) % element => p_simple_shape
end do

do i=1,20
    p_simple_shape => simple_shapes(i) % element
    call p_simple_shape % print()
end do

do i=1,20
    p_simple_shape => simple_shapes(i) % element
    deallocate(p_simple_shape)
end do

deallocate(simple_shapes)

! --- Polymorphism

allocate(p_square)
allocate(p_circle)

call p_square % init()
call p_circle % init()

print*, '-----'

p_shape => p_square

call p_shape % print()

p_shape => p_circle

call p_shape % print()

print*, '-----'

allocate(shapes(20))

do i = 1, 20
    allocate(p_square)
    shapes(i) % element => p_square
end do

do i = 1, 20
    p_shape => shapes(i) % element
    call p_shape % print()
end do

deallocate(shapes)

! --- Composition: triangle_class

allocate(p_triangle)
call p_triangle % init()

p_p0 => p_triangle % get_point(1)
p_p1 => p_triangle % get_point(1)
p_p2 => p_triangle % get_point(1)

call p_p0 % set_pos(0.0, 0.0)
call p_p1 % set_pos(1.0, 0.0)
call p_p2 % set_pos(1.0, 1.0)

call p_triangle % print()

deallocate(p_triangle)

contains

subroutine make_objects()

    type(shape_class) :: s4

    call s4 % init()
    call s4 % set_pos(5.0, 6.0)
    call s4 % print()

end subroutine

end program myfortran
