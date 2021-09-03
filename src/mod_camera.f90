module mod_camera
  use mod_ray
  use mod_utils
  use mod_vectors
  implicit none

  real, parameter :: default_aspect_ratio = 16./9., &
                     default_viewport_height = 2.
  type :: Camera

    real, DIMENSION(3) :: origin = [0., 0., 0.], &
                          horizontal, &
                          vertical, &
                          lower_left_corner, &
                          u, v, w
    real :: lens_radius
  contains
    procedure, public, pass(self) :: get_ray
  end type Camera

  interface Camera
    module procedure :: camera_constructor
  end interface Camera
contains

  pure type(Camera) function camera_constructor(look_from, look_at, v_up, v_fov, ratio, aperture, focus_dist) result(res)
    real, INTENT(IN) :: v_fov, ratio, aperture, focus_dist, look_from(3), look_at(3), v_up(3)
    real :: h, viewport_height, viewport_width
    real :: u(3), v(3), w(3) 

    h = tan(v_fov * atan(1.) / 90) ! hint: PI = 4 * atan(1.) and 1deg = PI/180 rad

    viewport_height = 2. * h
    viewport_width = ratio * viewport_height

    w = unit_vector(look_from - look_at)
    u = unit_vector(v_up .cross. w)
    v = w .cross. u

    res%w = w
    res%u = u
    res%v = v

    res%origin = look_from
    res%horizontal = focus_dist * viewport_width * u
    res%vertical = focus_dist * viewport_height * v
    res%lower_left_corner = res%origin - res%horizontal / 2 - res%vertical / 2 - focus_dist * w

    res%lens_radius = aperture / 2
  end function camera_constructor

  function get_ray(self, s, t) result(r)
    class(Camera), INTENT(IN) :: self
    real, INTENT(IN) :: s, t
    type(Ray) :: r
    real :: rd(3), offset(3)

    rd = self%lens_radius * random_in_unit_disc()
    offset = self%u * rd(X) + self%v * rd(Y)

    r = Ray(origin=self%origin + offset, &
      direction=self%lower_left_corner + s*self%horizontal + t*self%vertical - self%origin - offset)
  end function get_ray
  
end module mod_camera