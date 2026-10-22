program geometry
  implicit none
  real, allocatable :: arr1(:), arr2(:), arr3(:), arr4(:)
  integer :: i, n

  n = 10
  allocate(arr1(n))
  allocate(arr2(n))
  allocate(arr3(n))
  
  call random_number(arr1)
  call random_number(arr2)
  arr1 = int(arr1 * 10)
  arr2 = int(arr2 * 10)
  
  arr3 = gradient(arr1)
  arr4 = gradient(arr1, arr2)

contains
  
  function gradient(y, x) result(dy)
    real, intent(in) :: y(:)
    real, intent(in), optional :: x(:)
    real :: dy(size(y))
    integer :: i

    if (.not. present(x)) then
       do i = 1, size(y)
          if (i == 1) then
             dy(i) = (y(i + 1) - y(i)) / ((i + 1) - i)
          else if (i == size(y)) then
             dy(i) = (y(i) - y(i - 1)) / (i - (i - 1))
          else
             dy(i) = (y(i + 1) - y(i - 1)) / ((i + 1) - (i - 1))
          end if
       end do

       print *, ''
       write (*, '(a10, a10)') 'y', 'dy/dx'
       do i = 1, size(y)
          write (*, '(f10.2, f10.2)') y(i), dy(i)
       end do
    end if
    
    if (present(x)) then
       do i = 1, size(y)
          if (i == 1) then
             dy(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i))
          else if (i == size(y)) then
             dy(i) = (y(i) - y(i - 1)) / (x(i) - x(i - 1))
          else
             dy(i) = (y(i + 1) - y(i - 1)) / (x(i + 1) - x(i - 1))
          end if
       end do
       
       print *, ''
       write (*, '(a10, a10, a10)') 'y', 'x', 'dy/dx'
       do i = 1, size(y)
          write (*, '(f10.2, f10.2, f10.2)') y(i), x(i), dy(i)
       end do
    end if
    
  end function gradient
  
end program geometry
