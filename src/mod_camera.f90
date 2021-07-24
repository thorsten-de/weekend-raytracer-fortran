module mod_camera
  use mod_vectors
  use mod_types
  implicit none

  real, parameter :: default_aspect_ratio = 16./9., &
                     default_viewport_height = 2.
  type :: Camera
    real :: aspect_ratio = default_aspect_ratio
    real :: viewport_height = default_viewport_height
    real :: viewport_width = default_aspect_ratio * default_viewport_height
    real :: focal_length = 1.0

    real, DIMENSION(3) :: origin = [0., 0., 0.], &
                          horizontal, &
                          vertical, &
                          lower_left_corner
  contains
    procedure, public, pass(self) :: get_ray
  end type Camera
contains
  pure function get_ray(self, u, v) result(r)
    class(Camera), INTENT(IN) :: self
    real, INTENT(IN) :: u, v
    type(Ray) :: r

    r = Ray(origin=self%origin, direction=self%lower_left_corner + u*self%horizontal + v*self%vertical - self%origin)
  end function get_ray
  
end module mod_camera