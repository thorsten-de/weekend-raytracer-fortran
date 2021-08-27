program fray
  use iso_fortran_env, only: stdout => output_unit, stderr => error_unit

  use mod_vectors
  use mod_ray
  use mod_shapes
  use mod_camera
  use mod_material


  implicit none

  ! Image parameters
  real, PARAMETER :: aspect_ratio = 16.0 / 9.0;
  integer, PARAMETER :: image_width   = 640, &
                        image_height  = int(image_width / aspect_ratio), &
                        samples_per_pixel = 100, &
                        max_depth = 50

  real, PARAMETER :: iw = image_width - 1, ih = image_height - 1

  type(Camera) :: cam 
  type(Ray) :: r

  integer :: i, j, s
  real :: u, v
  real :: pixel_color(3)! colors
  
  class(Hittable), ALLOCATABLE :: my_world
  type(Sphere) :: spheres(4)
  class(Material), ALLOCATABLE :: mat_ground, mat_center, mat_left, mat_right
  integer :: indices(2), is, ie
  real :: image(3, image_width, image_height)[*]
  integer, PARAMETER :: collecting_image = 1

  image = 0.
  
  mat_ground = Lambertian(color(0.8, 0.8, 0.0))
  mat_center = Dielectric(1.5)
  ! mat_left = Dielectric(1.5)
  ! mat_right = Lambertian(color(0.7, 0.3, 0.3))
  mat_left = Metal(color(0.8, 0.8, 0.8), 0.3)
  mat_right = Metal(color(0.8, 0.6, 0.2), 1.0)



  spheres(1) = Sphere([ 0.0, -100.5, -1.], 100., mat_ground)
  spheres(2) = Sphere([ 0.0, 0.0, -1.0], 0.5, mat_center)
  spheres(3) = Sphere([-1.0, 0.0, -1.0], 0.5, mat_left)
  spheres(4) = Sphere([ 1.0, 0.0, -1.0], 0.5, mat_right)

!  spheres(2)  = spheres(1)
  my_world = HittableList(spheres)  
  cam = Camera()

  ! get the slicing indices
  indices = tile_image(image_width);
  is = indices(1)
  ie = indices(2)


  ! calculate the pixels inside of this slice, and put them on the collecting image
  do j = image_height - 1, 0, - 1
    write(stderr, *) this_image(), 'of', num_images(), is, ':', ie,   '  ... Scanlines remaining: ', j
    do i=is, ie
      pixel_color = [0., 0., 0.]
      do s=1, samples_per_pixel 
        u = (i + random_uniform()) / iw
        v = (j + random_uniform()) / ih
        r = cam%get_ray(u, v)
        pixel_color = pixel_color + ray_color(r, my_world, max_depth)
      end do
      image(:, i+1, j+1)[collecting_image] = pixel_color
    end do
  end do

  ! wait for all images to be done with their slices
  sync all

  ! write the collected image to disk
  if (this_image() == collecting_image) then
    write (stdout, '(a)') "P3"
    write (stdout, '(2(i4, 1x))') image_width, image_height
    write (stdout , '(i3)') 255

    do j= image_height , 1, -1
      do i=1, image_width
        write (stdout, '(3(i3, 1x))') color_out(image(:, i, j), samples_per_pixel)
      end do
    end do
  end if

contains
  recursive function ray_color(r, world, depth) result(res)
    class(Ray), INTENT(IN) :: r
    class(Hittable), INTENT(IN) :: world
    type(HitRecord) :: rec
    type(Ray) :: scattered
    integer, INTENT(IN) :: depth
    real :: unit_direction(3), res(3), attenuation(3), t
 
    if (depth <= 0) then
      res = color(0., 0., 0.)
      return
    end if

 
    if (world%hit(r, 0.001, huge(1.), rec)) then
      attenuation = 0
      if (rec%material%scatter(r, rec, attenuation, scattered)) then
        res = attenuation * ray_color(scattered, world, depth - 1)
        RETURN
      end if
      res = [0., 0., 0.]
      RETURN
    end if

    unit_direction = unit_vector(r%direction)
    t = 0.5*(unit_direction(Y) + 1.0)
    res = (1.0-t) * color(1.0, 1.0, 1.0) + t * color(0.5, 0.7, 1.0)
  end function ray_color

  function tile_image(width)
    integer, INTENT(IN) :: width  
    integer :: tile_image(2), tile_width

    tile_width = width / num_images()
    tile_image(2) = this_image() * tile_width - 1
    tile_image(1) = tile_image(2) - tile_width + 1
  end function tile_image



end program fray

