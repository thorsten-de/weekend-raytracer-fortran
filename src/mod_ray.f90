module mod_ray
  use mod_vectors
  implicit none
  
  type :: Ray
    real, DIMENSION(3) :: origin, direction
  contains
    procedure, pass(self) :: at => get_point_at
  end type Ray

  type, abstract :: Material
  contains
    procedure, public, pass(self) :: scatter
  end type Material

  type :: HitRecord
    real ::t, p(3) = [0, 0, 0], normal(3) = [0,0,0]
    class(Material), ALLOCATABLE :: material
    logical :: front_face = .true.
  contains
    procedure, pass(self) :: set_face_normal
  end type HitRecord


contains
  pure function get_point_at(self, t) result(at)
    class(Ray), INTENT(IN) :: self
    real, INTENT(IN) :: t
    real :: at(3)

    at = self%origin + self%direction * t
  end function get_point_at


  subroutine set_face_normal(self, r, outward_normal)
    class(HitRecord), INTENT(INOUT) :: self
    class(Ray), INTENT(IN) :: r
    real, INTENT(IN) :: outward_normal(3)

    self%front_face = (r%direction .dot. outward_normal) < 0
    if (self%front_face) then
      self%normal = outward_normal
    else 
      self%normal = -outward_normal
    end if
  end subroutine set_face_normal
  
  logical function scatter(self, r_in, rec, attenuation, scattered)
    class(Material), INTENT(INOUT) :: self
    type(Ray), intent(in) :: r_in
    type(HitRecord), intent(in) :: rec
    type(Ray), intent(out) :: scattered
    real, intent(out) :: attenuation(3)


    scatter = .false.
  end function scatter
  
end module mod_ray