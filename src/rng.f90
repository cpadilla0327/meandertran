module rng

  use iso_fortran_env, only: dp => real64
  implicit none
 
contains

  function rand_real(a, b) result(val)

    real(dp), intent(in) :: a, b
    real(dp) :: val, r

    call random_number(r)
    val = a + (b - a) * r

  end function rand_real

  function rand_int(a, b) result(val)

    integer, intent(in) :: a, b
    integer :: val
    real(dp) :: r
    
    call random_number(r)
    val = a + floor((b - a + 1) * r)

  end function rand_int

  function rand_choice(a, b) result(val)

    integer, intent(in) :: a, b
    integer :: val
    real(dp) :: r

    call random_number(r)
    if (r < 0.5) then
       val = a
    else
       val = b
    end if

  end function rand_choice
 
end module rng