program fortran_cpp

use iso_c_binding

implicit none

interface
    subroutine simple() bind(C, name="simple")
    end subroutine simple

    subroutine easy(a, b) bind(C, name="easy")
        use iso_c_binding
        integer(c_int), value :: a
        integer(c_int), value :: b
    end subroutine

    subroutine no_problem(a, b, c) bind(C, name="no_problem")
        use iso_c_binding
        real(c_float), value :: a
        real(c_float), value :: b
        real(c_float), intent(out) :: c
    end subroutine

    subroutine no_problemas(a, b, c) bind(C, name="no_problemas")
        use iso_c_binding
        real(c_float), value :: a
        real(c_float), value :: b
        real(c_float), intent(out) :: c
    end subroutine

    subroutine many_numbers(a, b, c, n) bind(C, name="many_numbers")
        use iso_c_binding
        real(c_float) :: a(n)
        real(c_float) :: b(n)
        real(c_float) :: c(n)
        integer(c_int), value :: n
    end subroutine

    function myfunc(x) result(y) bind(C, name="myfunc")
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
    end function

end interface

real(c_float) :: c1, c2
real(c_float), allocatable, dimension(:) :: a, b, c
integer :: i
real(c_double) :: x, y

allocate(a(20), b(20), c(20))

a = 42.0
b = 43.0
c = 0.0

call simple()
call easy(2, 4)
call no_problem(2.0, 4.0, c1)

print*, c1

call no_problemas(2.0, 4.0, c2)

print*, c2

call many_numbers(a, b, c, 20)

do i=1,20
    print*, c(i)
end do

print*, myfunc(1.0_c_double)

contains

end program fortran_cpp
