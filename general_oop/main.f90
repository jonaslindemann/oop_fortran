program myfortran

    ! --- Put use statements here
    !
    use shape_classes

    implicit none

    type ShapeArray
        class(Shape), pointer :: element
    end type

    integer :: i

    type(Shape) :: s
    type(Shape), pointer :: s2
    type(Square) :: sq
    type(Circle) :: c

    class(Square), pointer :: p_square
    class(Circle), pointer :: p_circle
    class(Shape), pointer :: p_shape

    type(ShapeArray), allocatable :: shapes(:)

!    call s % init()
!    call s % print()
!    call s % set_pos(2.0, 2.0)
!    call s % print()

!    allocate(s2)

!    call s2 % init()
!    call s2 % print()

!    deallocate(s2)

!    call sq % init()
!    call sq % print()

!    call c % init()
!    call c % print()

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
