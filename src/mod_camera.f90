module mod_camera
  use mod_ray
  use mod_utils
  implicit none

  real, parameter :: default_aspect_ratio = 16./9., &
                     default_viewport_height = 2.
  type :: Camera

    real, DIMENSION(3) :: origin = [0., 0., 0.], &
                          horizontal, &
                          vertical, &
                          lower_left_corner
  contains
    procedure, public, pass(self) :: get_ray
  end type Camera

  interface Camera
    module procedure :: camera_constructor
  end interface Camera
contains

  pure type(Camera) function camera_constructor(look_from, look_at, v_up, v_fov, ratio) result(res)
    real, INTENT(IN) :: v_fov, ratio, look_from(3), look_at(3), v_up(3)
    real :: h, viewport_height, viewport_width
    real :: u(3), v(3), w(3) 

    h = tan(v_fov * atan(1.) / 90) ! hint: PI = 4 * atan(1.) and 1deg = PI/180 rad

    viewport_height = 2. * h
    viewport_width = ratio * viewport_height

    w = unit_vector(look_from - look_at)
    u = unit_vector(v_up .cross. w)
    v = w .cross. u

    res%origin = look_from
    res%horizontal = viewport_width * u
    res%vertical = viewport_height * v
    res%lower_left_corner = res%origin - res%horizontal / 2 - res%vertical / 2 - w
  end function camera_constructor

  pure function get_ray(self, s, t) result(r)
    class(Camera), INTENT(IN) :: self
    real, INTENT(IN) :: s, t
    type(Ray) :: r

    r = Ray(origin=self%origin, direction=self%lower_left_corner + s*self%horizontal + t*self%vertical - self%origin)
  end function get_ray
  
end module mod_camera