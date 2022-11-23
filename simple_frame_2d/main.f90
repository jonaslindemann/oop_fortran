program myfortran

use mf_datatypes
use mf_utils
use calfem

implicit none

type(NodeSet) :: node_set
type(BarElementSet) :: bar_element_set
type(BarElement) :: bar
type(BarModel) :: bar_model

call set_print_format(12)

bar = BarElement()
call bar % print()

bar_model = BarModel(4, 2)
!node_set = bar_model % node_set()
!bar_element_set = bar_model % element_set()

!call bar_element_set % print()

end program myfortran
