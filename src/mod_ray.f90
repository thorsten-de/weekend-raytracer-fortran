module mod_ray
  implicit none
  
  type :: Ray
    real, DIMENSION(3) :: origin, direction
  contains
    procedure, pass(self) :: at => get_point_at
  end type Ray
contains
  pure function get_point_at(self, t) result(at)
    class(Ray), INTENT(IN) :: self
    real, INTENT(IN) :: t
    real :: at(3)

    at = self%origin + self%direction * t
  end function get_point_at
  
end module mod_ray