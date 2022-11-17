module shape_classes

implicit none

! --- Variable declarations here

type Shape
    private
    real :: pos(2)
contains
    procedure :: init => shape_init
    procedure :: set_pos => shape_set_pos
    procedure :: print => shape_print
end type

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

! -----------------------------------------------------

contains

! -----------------------------------------------------

subroutine shape_init(this)

    class(Shape), intent(out) :: this

    print*, 'shape_init()'
    this % pos = (/ 0.0, 0.0 /)

end subroutine

subroutine shape_set_pos(this, x, y)

    class(Shape), intent(out) :: this
    real, intent(in) :: x
    real, intent(in) :: y

    this % pos(1) = x
    this % pos(2) = y

end subroutine

subroutine shape_print(this)

    class(Shape), intent(in) :: this

    print*, 'shape_print()'
    print*, 'x = ', this % pos(1)
    print*, 'y = ', this % pos(2)

end subroutine

! -----------------------------------------------------

subroutine square_init(this)

    class(Square), intent(out) :: this

    print*, 'square_init()'

    call shape_init(this)


    this % side = 1.0

end subroutine

subroutine square_print(this)

    class(Square), intent(in) :: this

    print*, 'square_print()'
    print*, 'side = ', this % side
    call shape_print(this)

end subroutine

! -----------------------------------------------------

subroutine circle_init(this)

    class(Circle), intent(out) :: this

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

    class(Circle), intent(in) :: this

    print*, 'cirlce_print()'
    print*, 'radius = ', this % radius
    call shape_print(this)

end subroutine

end module shape_classes
