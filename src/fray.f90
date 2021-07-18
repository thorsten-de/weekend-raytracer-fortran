program fray
  use iso_fortran_env, only: stdout => output_unit, stderr => error_unit

  use mod_vectors
  use mod_types


  implicit none

  ! Image parameters
  real, PARAMETER :: aspect_ratio = 16.0 / 9.0;
  integer, PARAMETER :: image_width   = 800, &
                        image_height  = int(image_width / aspect_ratio)
  real, PARAMETER :: iw = image_width - 1, ih = image_height - 1


  ! Camera settings
  real, PARAMETER ::  viewport_height   = 2.0, &
                      viewport_width    = aspect_ratio * viewport_height, &
                      focal_length      = 1.0

  real, PARAMETER, DIMENSION(3) :: origin            = [0.0, 0.0, 0.0], &
                                   horizontal        = [viewport_width, 0.0, 0.0], &
                                   vertical          = [0.0, viewport_height, 0.0], &
                                   lower_left_corner = origin - horizontal/2 - vertical/2 - [0.0, 0.0, focal_length]


  integer :: i, j
  real :: u, v
  type(Ray) :: r


  write (stdout, '(a)') "P3"
  write (stdout, '(2(i3, 1x))') image_width, image_height
  write (stdout , '(i3)') 255
  
  do j= image_height - 1, 0, - 1
    write(stderr, *) 'Scanlines remaining: ', j
    do i=0, image_width - 1
      u = i / iw
      v = j / ih
      r = Ray(origin, lower_left_corner + u * horizontal + v * vertical - origin)
     write (stdout, '(3(i3, 1x))') color_out(ray_color(r))
    end do
  end do


contains
  pure function ray_color(r) result(res)
    class(Ray), INTENT(IN)::r
    real :: unit_direction(3), res(3), N(3), t
    t = hit_sphere(point(0.0, 0.0, -1.0), 0.5, r)

    if (t > 0.0) then 
      N = unit_vector(r%at(t) - vector(0.0, 0.0, -1.0))
      res = 0.5 * (N + 1.0) ! 0.5 * (N+1)
      RETURN
    end if

    unit_direction = unit_vector(r%direction)

    t = 0.5*(unit_direction(Y) + 1.0)
    res = (1.0-t) * color(1.0, 1.0, 1.0) + t * color(0.5, 0.7, 1.0)
  end function ray_color


  pure real function hit_sphere(center, radius, r)
    real, INTENT(IN) :: center(3), radius
    class(Ray), INTENT(IN) :: r
    real :: oc(3), a, b, c, discriminant
    
    oc = r%origin - center
    a = r%direction .dot. r%direction
    b = 2.0 * (oc .dot. r%direction)
    c  = oc .dot. oc - radius * radius
    discriminant = b*b - 4*a*c

    if (discriminant < 0) then
      hit_sphere = -1.0
    else
      hit_sphere = (-b -sqrt(discriminant)) / (2.0*a)
    end if

  end function hit_sphere


end program fray

