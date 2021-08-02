module mod_vectors
  use mod_utils
  implicit none


    public :: operator(.dot.)
    public :: operator(.cross.)

    integer, parameter :: X = 1
    integer, parameter :: Y = 2
    integer, parameter :: Z = 3



    interface operator(.dot.)
      module procedure :: dot_vectors
    end interface 
    
    interface operator(.cross.)
      module procedure :: cross_vectors
    end interface 

    interface vector
      module procedure :: vector_0, vector_3
    end interface vector
    interface point
      module procedure :: vector_0, vector_3
    end interface point
    

  contains

  pure function color_out(pixel_color, samples_per_pixel)  result(int_v)
    real, intent(in) :: pixel_color(3)
    INTEGER, intent(in) :: samples_per_pixel
    INTEGER :: int_v(size(pixel_color))
    ! real :: scale, col(3)
    
    ! scale = 1.0 / samples_per_pixel
    ! col = pixel_color * scale

    int_v = int(256. * clamp(sqrt(pixel_color *(1. / samples_per_pixel )), 0., 0.999))
  end function color_out

  pure function unit_vector(v)
    real, INTENT(IN) :: v(:)
    real :: unit_vector(size(v))

    unit_vector = v / length(v)    
  end function

  pure real function length(v)
    real, INTENT(IN) :: v(:)

    length = SQRT(length_squared(v))
  end function length


  pure real function length_squared(v)
    real, INTENT(IN) :: v(:)

    length_squared = sum(v * v)
  end function length_squared

  pure real function dot_vectors(u, v) result(res)
    real, INTENT(IN) :: v(:), u(:)
    res = dot_product(v, u)
  end function dot_vectors

  pure function cross_vectors(u, v) result(cross)
  real, INTENT(IN) :: v(3), u(3)
  real :: cross(3)
    cross(1) = u(2) * v(3) - u(3) * v(2)
    cross(2) = u(3) * v(1) - u(1) * v(3)
    cross(3) = u(1) * v(2) - u(2) * v(1)
  end function cross_vectors

  pure function vector_0()
    real :: vector_0(3)

    vector_0 = [0.0, 0.0, 0.0]
  end function vector_0


  pure function vector_3(x, y, z)
    real, INTENT(IN) :: x, y, z
    real :: vector_3(3) 
    
    vector_3 = [x, y, z]
  end function vector_3

  function random_in_unit_sphere() result(res)
    real :: res(3)
    
    do 
      res = random_distribution(3, -1., 1.)
      if (length_squared(res) < 1) return
    end do
  end function random_in_unit_sphere

  function random_unit_vector() result(res) 
    real :: res(3)

    res = unit_vector(random_in_unit_sphere())
  end function random_unit_vector

  function random_in_hemisphere(normal) result(res)
    real, INTENT(IN) :: normal(3)
    real :: res(3)

    res = random_in_unit_sphere()
    if ((res .dot. normal) <= 0) then
      res = -res
    end if
  end function random_in_hemisphere

  pure function near_zero(v) result(res)
    real, INTENT(IN) :: v(3)
    real, PARAMETER :: s = 1e-8
    logical :: res

    res = all(v < s)
  end function near_zero

  pure function reflect(v, n)
    real, INTENT(IN) :: v(3), n(3)
    real :: reflect(3)

    reflect = v - 2*(n .dot. v) * n
  end function reflect

  pure function refract(uv, n, etai_over_etat)
    real, INTENT(IN) :: uv(3), n(3), etai_over_etat
    real :: cos_theta, r_out_perp(3), r_out_parallel(3), refract(3)

    cos_theta = min(-uv .dot. n, 1.0)
    r_out_perp = etai_over_etat * (uv + cos_theta * n)
    r_out_parallel = -sqrt(abs(1.0 - length_squared(r_out_perp))) * n
    refract = r_out_perp + r_out_parallel
  end function refract



  pure function color(r, g, b)
    real, INTENT(IN) :: r, g, b
    real :: color(3) 
    
    color = [r, g, b]
  end function color
end module mod_vectors
