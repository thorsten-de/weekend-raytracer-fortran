module mod_utils
  implicit none
  
contains
  real function random_uniform()
    call random_number(random_uniform) 
  end function random_uniform

  real function uniform_distribution(min, max)
    real, INTENT(IN) :: min, max
    real :: u
    u = random_uniform()
    uniform_distribution = (max-min)*u + min
  end function uniform_distribution
end module mod_utils