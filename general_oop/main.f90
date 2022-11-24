program myfortran

    use shape_classes

implicit none

type SimpleShapeArray
    type(Shape), pointer :: element
end type

type ShapeArray
    class(Shape), pointer :: element
end type

integer :: i

type(Shape) :: s
type(Shape), allocatable :: s2
type(Square) :: sq
type(Circle) :: c

! Instantiating a Particle object

type(Particle) :: p

class(Square), pointer :: p_square
class(Circle), pointer :: p_circle
class(Shape), pointer :: p_shape
type(Shape), pointer :: p_simple_shape
type(SimpleShapeArray), allocatable :: simple_shapes(:)
type(ShapeArray), allocatable :: shapes(:)
type(Shape) :: static_shapes(20)
type(Shape), allocatable :: dyn_shapes(:)

class(Triangle), pointer :: p_triangle
class(Point), pointer :: p_p0
class(Point), pointer :: p_p1
class(Point), pointer :: p_p2

allocate(p_triangle)
call p_triangle % init()

p_p0 => p_triangle % get_point(1)
p_p1 => p_triangle % get_point(1)
p_p2 => p_triangle % get_point(1)

call p_p0 % set_pos(0.0, 0.0)
call p_p1 % set_pos(1.0, 0.0)
call p_p2 % set_pos(1.0, 1.0)

call p_triangle % print()

stop

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

stop

do i=1,20
    call static_shapes(i) % init()
    call static_shapes(i) % print()
end do

allocate(dyn_shapes(20))

do i=1,20
   call dyn_shapes(i) % init()
   call dyn_shapes(i) % print()
end do

deallocate(dyn_shapes)

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

s = Shape()
!call s % init()
call s % print()
call s % set_pos(2.0, 2.0)
call s % print()

allocate(s2)

call s2 % init()
call s2 % print()

deallocate(s2)

call sq % init()
call sq % print()
call c % init()
call c % print()

allocate(shapes(20))

do i = 1, 20
    allocate(p_square)
    shapes(i) % element => p_square
end do

do i = 1, 20
    p_shape => shapes(i) % element
    call p_shape % print()
end do

contains

    ! --- Add your subroutines and functions here
    ! 
    ! subroutine mysub
    ! 
    ! end subroutine mysub

end program myfortran
