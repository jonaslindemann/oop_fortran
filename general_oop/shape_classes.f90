module shape_classes

implicit none

! --- Variable declarations here

type Particle
    real :: pos(3)
    real :: vel(3)
    real :: mass
end type

type Shape
    private
    real :: pos(2)
contains
    procedure :: init => shape_init
    procedure :: set_pos => shape_set_pos
    procedure :: print => shape_print
    final :: shape_destructor
end type

interface Shape
    module procedure shape_constructor
end interface

type, extends(Shape) :: Square
    private
    real :: side
contains
    procedure :: init => square_init
    procedure :: print => square_print
end type

type, extends(Shape) :: Circle
    private
    real :: radius
contains
    procedure :: init => circle_init
    procedure :: print => circle_print
    procedure :: set_radius => circle_set_radius
end type

type Point
private
    real :: m_x, m_y
contains
    procedure :: init => point_init
    procedure :: set_pos => point_set_pos
    procedure :: print => point_print
end type

type PointElement
    class(Point), pointer :: p_element
end type

type Triangle
private
    type(PointElement) :: m_points(3)
contains
    procedure :: init => triangle_init
    procedure :: get_point => triangle_get_pointo
    procedure :: print => triangle_print
end type


! -----------------------------------------------------

contains

! -----------------------------------------------------

subroutine shape_init(this)

    class(Shape) :: this

    print*, 'shape_init()'
    this % pos = (/ 0.0, 0.0 /)

end subroutine

function shape_constructor() result(shape_inst)

    type(Shape) :: shape_inst

    print*, 'shape_constructor()'
    call shape_init(shape_inst)

end function

subroutine shape_destructor(this)

    type(Shape) :: this

    print*, 'shape_destructor()'

end subroutine

subroutine shape_set_pos(this, x, y)

    class(Shape) :: this
    real, intent(in) :: x
    real, intent(in) :: y

    this % pos(1) = x
    this % pos(2) = y

end subroutine

subroutine shape_print(this)

    class(Shape) :: this

    print*, 'shape_print()'
    print*, 'x = ', this % pos(1)
    print*, 'y = ', this % pos(2)

end subroutine

! -----------------------------------------------------

subroutine square_init(this)

    class(Square) :: this

    print*, 'square_init()'

    call shape_init(this)


    this % side = 1.0

end subroutine

subroutine square_print(this)

    class(Square) :: this

    print*, 'square_print()'
    print*, 'side = ', this % side
    call shape_print(this)

end subroutine

! -----------------------------------------------------

subroutine circle_init(this)

    class(Circle) :: this

    print*, 'circle_init()'
    call shape_init(this)

    this % radius = 1.0

end subroutine

subroutine circle_set_radius(this, r)

    class(Circle), intent(out) :: this
    real, intent(in) :: r

    this % radius = r

end subroutine

subroutine circle_print(this)

    class(Circle) :: this

    print*, 'cirlce_print()'
    print*, 'radius = ', this % radius
    call shape_print(this)

end subroutine

! -----------------------------------------------------

subroutine point_init(this, x, y)

    class(Point) :: this
    real :: x, y

    this % m_x = x
    this % m_y = y

end subroutine

subroutine point_set_pos(this, x, y)

    class(Point) :: this
    real x, y

    this % m_x = x
    this % m_y = y

end subroutine

subroutine point_print(this)

    class(Point) :: this

    print*, 'Point'
    print*, this % m_x, this % m_y

end subroutine

! -----------------------------------------------------

subroutine triangle_init(this)

    class(Triangle) :: this
    class(Point), pointer :: p_point

    integer :: i

    do i=1,3
        allocate(p_point)
        call p_point % init(0.0, 0.0)
        this % m_points(i) % p_element => p_point
    end do

end subroutine

function triangle_get_point(this, idx) result(p_inst)

    class(Triangle) :: this
    class(Point), pointer :: p_inst
    integer :: idx

    p_inst => this % m_points(idx) % p_element

end function

subroutine triangle_print(this)

    class(Triangle) :: this
    class(Point), pointer :: p_point
    integer :: i

    print*, 'Triangle'

    do i=1,3
        call this % m_points(i) % p_element % print()
    end do

end subroutine

end module shape_classes


