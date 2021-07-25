program fray
  use iso_fortran_env, only: stdout => output_unit, stderr => error_unit

  use mod_vectors
  use mod_ray
  use mod_shapes
  use mod_camera


  implicit none

  ! Image parameters
  real, PARAMETER :: aspect_ratio = 16.0 / 9.0;
  integer, PARAMETER :: image_width   = 400, &
                        image_height  = int(image_width / aspect_ratio), &
                        samples_per_pixel = 100

  real, PARAMETER :: iw = image_width - 1, ih = image_height - 1

  type(Camera) :: cam 
  type(Ray) :: r

  integer :: i, j, s
  real :: u, v
  real :: pixel_color(3)
  
  type(Sphere) :: spheres(2)
  type(HittableList) :: my_world
  !type(Sphere) :: sphere
  

  spheres(1) = Sphere([0., 0., -1.], 0.4)
  spheres(2) = Sphere([0., -100.5, -1.], 100.)
!  spheres(2)  = spheres(1)
  my_world = HittableList(spheres)
  
  cam = Camera()

  write (stdout, '(a)') "P3"
  write (stdout, '(2(i3, 1x))') image_width, image_height
  write (stdout , '(i3)') 255
  
  do j= image_height - 1, 0, - 1
    write(stderr, *) 'Scanlines remaining: ', j
    do i=0, image_width - 1
      pixel_color = [0., 0., 0.]
      do s=1, samples_per_pixel 
        u = (i + random_uniform()) / iw
        v = (j + random_uniform()) / ih
        r = cam%get_ray(u, v)
        pixel_color = pixel_color + ray_color(r, my_world)
      end do
     write (stdout, '(3(i3, 1x))') color_out(pixel_color, samples_per_pixel)
    end do
  end do


contains
  function ray_color(r, world) result(res)
    class(Ray), INTENT(IN) :: r
    class(Hittable), INTENT(IN) :: world
    type(HitRecord) :: rec
    real :: unit_direction(3), res(3), t
 
 
    if (world%hit(r, 0.00, huge(1.), rec)) then
      res = 0.5 * (rec%normal + 1)
      RETURN
     end if


    unit_direction = unit_vector(r%direction)
    t = 0.5*(unit_direction(Y) + 1.0)
    res = (1.0-t) * color(1.0, 1.0, 1.0) + t * color(0.5, 0.7, 1.0)
  end function ray_color


end program fray

