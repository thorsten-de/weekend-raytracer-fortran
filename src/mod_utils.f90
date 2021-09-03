module mod_utils
  implicit none

  
contains
  real function random_uniform()
    call random_number(random_uniform) 
  end function random_uniform

  function random_distribution(length, min, max) result(res)
    integer, INTENT(IN) :: length
    real :: res(length)
    real, INTENT(IN) :: min, max

    call random_number(res)
    res = (max-min)*res + min
  end function random_distribution

  real function uniform_distribution(min, max)
    real, INTENT(IN) :: min, max
    real :: u
    u = random_uniform()
    uniform_distribution = (max-min)*u + min
  end function uniform_distribution

  real pure elemental function clamp(x, min, max)
    real, INTENT(IN) :: x, min, max

    if (x < min) then
      clamp = min
      RETURN
    else if (x > max) then
      clamp = max
    else 
      clamp = x
    end if
  end function clamp
end module mod_utils