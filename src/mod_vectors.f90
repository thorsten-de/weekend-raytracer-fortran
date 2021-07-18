module mod_vectors
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

  contains

  pure function color_out(v)  result(int_v)
    real, intent(in) :: v(:)
    INTEGER :: int_v(size(v))
    
    int_v = int(v * 255.999)
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

  pure function vector(x, y, z)
    real, INTENT(IN) :: x, y, z
    real :: vector(3) 
    
    vector = [x, y, z]
  end function vector

  pure function color(r, g, b)
    real, INTENT(IN) :: r, g, b
    real :: color(3) 
    
    color = [r, g, b]
  end function color


  pure function point(x, y, z)
    real, INTENT(IN) :: x, y, z
    real :: point(3) 
    
    point = [x, y, z]
  end function point

end module mod_vectors
