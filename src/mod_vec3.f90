module mod_vec3
  use iso_fortran_env, only: real32
  implicit none

  type :: Vec3
    real(real32) :: v(3) = [0, 0, 0]
  contains
    procedure, pass(self) :: color_out
    procedure, pass(self) :: length
    procedure, pass(self) :: length_squared
    procedure, pass(v1) :: dot


  end type Vec3

  interface Point
    module procedure :: point_constructor
  end interface Point

  interface Color
    module procedure :: color_constructor
  end interface Color
  
contains
  pure function point_constructor(x, y, z) result(point)
    real, INTENT(IN) :: x, y, z
    real :: point(3)

   point =  [x, y, z]
  end function point_constructor
  
  pure type(Vec3) function color_constructor(r, g, b) result(color)
    real(real32), INTENT(IN) :: r, g, b

    color%v = [r, g, b]
  end function color_constructor

  pure function color_out(self )  result(int_v)
    class(Vec3), intent(in) :: self
    INTEGER :: int_v(3)
    
    int_v = int(self%v * 255.999)
  end function color_out

  pure real(real32) function length(self)
    class(Vec3), INTENT(IN) :: self

    length = SQRT(self%length_squared())
  end function length


  pure real(real32) function length_squared(self)
    class(Vec3), INTENT(IN) :: self

    length_squared = sum(self%v * self%v)
  end function length_squared

  pure elemental real(real32) function dot(v1, v2) result(res)
    class(Vec3), intent(in) :: v1, v2

    res = dot_product(v1%v, v2%v)

  end function dot
end module mod_vec3