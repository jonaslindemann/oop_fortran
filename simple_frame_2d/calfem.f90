module calfem

use mf_datatypes
use mf_utils

implicit none

! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
! Class implementation
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

interface NodeSet
    module procedure node_set_constructor
end interface NodeSet

type NodeSet
    real(dp), allocatable :: m_positions(:,:)
    integer, allocatable :: m_dofs(:,:)
contains
    procedure :: init => node_set_init
    procedure :: print => node_set_print
    procedure :: resize => node_set_resize
    final :: node_set_destructor
end type

! ---------------------------------------------------------------------

interface ElementSet
    module procedure element_set_constructor
end interface ElementSet

type ElementSet
    integer, allocatable :: m_topo(:,:)
contains
    procedure :: init => element_set_init
    procedure :: print => element_set_print
    procedure :: resize => element_set_resize
end type

! ---------------------------------------------------------------------

interface BarElementSet
    module procedure bar_element_set_constructor
end interface BarElementSet

type, extends(ElementSet) :: BarElementSet

contains
    procedure :: init => bar_element_set_init
    procedure :: resize => bar_element_set_resize
end type

! ---------------------------------------------------------------------

interface BarElement
    module procedure bar_element_constructor
end interface

type BarElement
    real(dp) :: m_p0(2)
    real(dp) :: m_p1(2)
    real(dp) :: m_A
    real(dp) :: m_E
contains
    procedure :: init => bar_element_init
    procedure :: set_p0 => bar_element_set_p0
    procedure :: set_p1 => bar_element_set_p1
    procedure :: set_ep => bar_element_set_ep
    procedure :: Ke => bar_element_ke
    procedure :: print => bar_element_print
end type

! ---------------------------------------------------------------------

interface BarModel
    module procedure bar_model_constructor
end interface

type BarModel
    type(NodeSet), allocatable :: m_node_set
    type(BarElementSet), allocatable :: m_element_set
contains
    procedure :: init => bar_model_init
    procedure :: node_set => bar_model_node_set
    procedure :: element_set => bar_model_element_set
end type

contains

! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
! Class implementation
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

subroutine node_set_init(this, n_nodes, n_dofs_per_node)

    class(NodeSet) :: this
    integer, intent(in) :: n_nodes
    integer, intent(in) :: n_dofs_per_node

    allocate(this % m_positions(n_nodes, 2))
    allocate(this % m_dofs(n_nodes, n_dofs_per_node))

    this % m_positions = 0.0_dp
    this % m_dofs = -1

end subroutine

subroutine node_set_print(this)

    class(NodeSet) :: this

    call print_matrix(this % m_positions, 'Positions')
    call print_matrix_i(this % m_dofs, 'Dofs')

end subroutine

subroutine node_set_resize(this, n_nodes, n_dofs_per_node)

    class(NodeSet) :: this
    integer, intent(in) :: n_nodes
    integer, intent(in) :: n_dofs_per_node

    real(dp), allocatable :: positions(:,:)
    integer, allocatable :: dofs(:,:)

    allocate(positions(size(this % m_positions, 1), size(this % m_positions, 2)))
    allocate(dofs(size(this % m_dofs, 1), size(this % m_dofs, 2)))

    positions = this % m_positions
    dofs = this % m_dofs

    deallocate(this % m_positions, this % m_dofs)

    allocate(this % m_positions(n_nodes, 2))
    allocate(this % m_dofs(n_nodes, n_dofs_per_node))

    this % m_positions = 0.0_dp
    this % m_dofs = -1

    if (size(this % m_positions, 1) >= size(positions, 1)) then
        this % m_positions(1:size(positions, 1), :) = positions
    else
        this % m_positions(:,:) = positions(1:size(this % m_positions), :)
    end if

    deallocate(positions, dofs)

end subroutine


subroutine node_set_destructor(this)

    type(NodeSet) :: this

    print*, 'node_set_destructor'

    deallocate(this % m_positions, this % m_dofs)

end subroutine

function node_set_constructor(n_nodes, n_dofs_per_node) result(node_set)

    type(NodeSet) :: node_set
    integer, intent(in) :: n_nodes
    integer, intent(in) :: n_dofs_per_node

    print*, 'node_set_constructor'

    call node_set_init(node_set, n_nodes, n_dofs_per_node)

end function

! ---------------------------------------------------------------------

subroutine element_set_init(this, n_topo, n_dofs_per_element)

    class(ElementSet) :: this
    integer, intent(in) :: n_topo
    integer, intent(in), optional :: n_dofs_per_element

    if (present(n_dofs_per_element)) then
        allocate(this % m_topo(n_topo, n_dofs_per_element))
    else
        allocate(this % m_topo(n_topo, 6))
    end if

    this % m_topo = -1

end subroutine

subroutine element_set_print(this)

    class(ElementSet) :: this

    call print_matrix(this % m_topo, 'Topo')

end subroutine

subroutine element_set_resize(this, n_topo, n_dofs_per_element)

    class(ElementSet) :: this
    integer, intent(in) :: n_topo
    integer, intent(in), optional :: n_dofs_per_element

    integer, allocatable :: topo(:,:)
    integer :: actual_n_dofs_per_element


    if (present(n_dofs_per_element)) then
        actual_n_dofs_per_element = n_dofs_per_element
    else
        actual_n_dofs_per_element = 6
    end if


    allocate(topo(size(this % m_topo, 1), size(this % m_topo, 2)))

    topo = this % m_topo

    deallocate(this % m_topo)

    allocate(this % m_topo(n_topo, actual_n_dofs_per_element))

    this % m_topo = -1

    if (size(this % m_topo, 1) >= size(topo, 1)) then
        this % m_topo(1:size(topo, 1), 1:size(topo,2)) = topo
    else
        this % m_topo(:,1:size(topo,2)) = topo(1:size(this % m_topo), 1:size(topo,2))
    end if

    deallocate(topo)

end subroutine


subroutine element_set_destructor(this)

    type(ElementSet) :: this

    print*, 'element_set_destructor'

    deallocate(this % m_topo)

end subroutine

function element_set_constructor(n_topo, n_dofs_per_element) result(element_set)

    type(ElementSet) :: element_set
    integer, intent(in) :: n_topo
    integer, intent(in) :: n_dofs_per_element

    print*, 'node_set_constructor'

    call element_set_init(element_set, n_topo, n_dofs_per_element)

end function

! ---------------------------------------------------------------------

subroutine bar_element_set_init(this, n_topo, n_dofs_per_element)

    class(BarElementSet) :: this
    integer, intent(in) :: n_topo
    integer, intent(in), optional :: n_dofs_per_element

    call this % elementset % init(n_topo, 4)

end subroutine

subroutine bar_element_set_resize(this, n_topo, n_dofs_per_element)

    class(BarElementSet) :: this
    integer, intent(in) :: n_topo
    integer, intent(in), optional :: n_dofs_per_element

    call this % elementset % resize(n_topo, 4)

end subroutine

function bar_element_set_constructor(n_topo) result(bar_element_set)

    type(BarElementSet) :: bar_element_set
    integer, intent(in) :: n_topo

    print*, 'bar_element_set_constructor'

    call bar_element_set_init(bar_element_set, n_topo, 4)

end function

! ---------------------------------------------------------------------

subroutine bar_element_init(this, x0, y0, x1, y1, E, A)

    class(BarElement) :: this
    real(dp), intent(in), optional :: x0
    real(dp), intent(in), optional :: y0
    real(dp), intent(in), optional :: x1
    real(dp), intent(in), optional :: y1
    real(dp), intent(in), optional :: E
    real(dp), intent(in), optional :: A

    this % m_p0 = 0.0_dp
    this % m_p1 = 1.0_dp

    this % m_E = 2.1e9_dp
    this % m_A = 0.1_dp*0.1_dp

    if (present(x0)) this % m_p0(1) = x0
    if (present(x0)) this % m_p0(2) = y0
    if (present(x0)) this % m_p1(1) = x1
    if (present(x0)) this % m_p1(2) = y1
    if (present(x0)) this % m_E = E
    if (present(x0)) this % m_A = A

end subroutine

subroutine bar_element_set_p0(this, x, y)

    class(BarElement) :: this
    real(dp), intent(in) :: x, y

    this % m_p0(1) = x
    this % m_p0(2) = y

end subroutine

subroutine bar_element_set_p1(this, x, y)

    class(BarElement) :: this
    real(dp), intent(in) :: x, y

    this % m_p1(1) = x
    this % m_p1(2) = y

end subroutine

subroutine bar_element_set_ep(this, E, A)

    class(BarElement) :: this
    real(dp), intent(in) :: E, A

    this % m_E = E
    this % m_A = A

end subroutine

function bar_element_ke(this) result(Ke)

    class(BarElement) :: this
    real(dp) :: Ke(4,4)
    real(dp) :: b(2)
    real(dp) :: L
    real(dp) :: Kle(2,2)
    real(dp) :: c
    real(dp) :: G(2,4)
    real(dp) :: nxx, nyx

    b(1) = this % m_p1(1) - this % m_p0(1)
    b(2) = this % m_p1(2) - this % m_p0(2)

    L = sqrt(dot_product(b,b))

    c = this % m_E * this % m_A / L

    Kle(1,:) = (/ 1.0_dp, -1.0_dp /) * c
    Kle(2,:) = (/-1.0_dp,  1.0_dp /) * c

    nxx = (this % m_p1(1) - this % m_p0(1))/L
    nyx = (this % m_p1(2) - this % m_p0(2))/L

    G(1,:) = (/ nxx, nyx, 0.0_dp, 0.0_dp /)
    G(2,:) = (/ 0.0_dp, 0.0_dp, nxx, nyx /)

    Ke = matmul(transpose(G), matmul(Kle, G))

end function

subroutine bar_element_print(this)

    class(BarElement) :: this

    call print_matrix(this % Ke(), 'Ke')

end subroutine

function bar_element_constructor(x0, y0, x1, y1, E, A) result(bar_element)

    type(BarElement) :: bar_element
    real(dp), intent(in), optional :: x0
    real(dp), intent(in), optional :: y0
    real(dp), intent(in), optional :: x1
    real(dp), intent(in), optional :: y1
    real(dp), intent(in), optional :: E
    real(dp), intent(in), optional :: A

    print*, 'bar_element_set_constructor'

    call bar_element_init(bar_element, x0, y0, x1, y1, E, A)

end function

! ---------------------------------------------------------------------

subroutine bar_model_init(this, n_nodes, n_elements)

    class(BarModel) :: this

    integer, intent(in) :: n_nodes
    integer, intent(in) :: n_elements

    this % m_node_set = NodeSet(n_nodes, 2)
    this % m_element_set = BarElementSet(n_elements)

end subroutine

function bar_model_node_set(this) result(node_set)

    class(BarModel) :: this
    type(NodeSet) :: node_set

    node_set = this % m_node_set

end function

function bar_model_element_set(this) result(element_set)

    class(BarModel) :: this
    type(BarElementSet) :: element_set

    element_set = this % m_element_set

end function

function bar_model_constructor(n_nodes, n_elements) result(bar_model)

    type(BarModel) :: bar_model
    integer, intent(in) :: n_nodes
    integer, intent(in) :: n_elements

    call bar_model_init(bar_model, n_nodes, n_elements)

end function

! ---------------------------------------------------------------------

end module calfem
