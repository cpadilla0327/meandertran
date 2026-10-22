module kinoshita_class
  use iso_fortran_env, only: dp => real64
  use rng
  implicit none
  private :: pi
  real(dp), parameter :: pi = 3.14159265358979
  
  type :: Kinoshita
     real(dp), allocatable :: x(:), y(:), z(:), zeta(:)
     real(dp) :: w, d, theta0, ds, wavelength, js, jf
     integer :: direction, nbends
   contains
     procedure :: initialize
     procedure :: generate
     procedure :: export
  end type Kinoshita

contains

  subroutine initialize(self)
    class(Kinoshita), intent(inout) :: self

    self%w = rand_real(80.0_dp, 120.0_dp)
    self%d = rand_real(4.0_dp, 8.0_dp)
    self%theta0 = rand_real(30.0_dp, 60.0_dp)
    self%wavelength = rand_real(10 * self%w, 15 * self%w)
    self%ds = self%wavelength/100
    self%nbends = rand_int(5, 10)
    self%js = rand_real(0.0_dp, 1.0_dp/32.0_dp)
    self%jf = rand_real(0.0_dp, 1.0_dp/192.0_dp)
    self%direction = rand_choice(-1, 1)

  end subroutine initialize

  subroutine generate(self)
    class(Kinoshita), intent(inout) :: self
    real(dp), allocatable :: s(:), sine_term(:), skewness_term(:), flatness_term(:), theta(:)
    integer :: i
    real(dp) :: theta0_rad

    allocate(s(int(self%nbends * self%wavelength / self%ds) + 1))
    do i = 1, size(s)
       s(i) = (self%nbends * self%wavelength) * real(i - 1, dp) / real(size(s) - 1, dp)
    end do

    allocate(sine_term(size(s)), skewness_term(size(s)), flatness_term(size(s)))
    allocate(self%z(size(s)), self%zeta(size(s)))

    theta0_rad = self%theta0 * (pi / 180_dp)
    sine_term = theta0_rad * sin(2 * pi * s / self%wavelength)
    skewness_term = self%js * cos(6 * pi * s / self%wavelength)
    flatness_term = self%jf * sin(6 * pi * s / self%wavelength)
    theta = self%direction * sine_term + theta0_rad**3 * (skewness_term - flatness_term)

    self%x = cos(theta) * self%ds
    self%y = sin(theta) * self%ds
 
    do i = 2, size(theta)
       self%x(i) = self%x(i) + self%x(i - 1)
       self%y(i) = self%y(i) + self%y(i - 1) 
    end do

    self%x = [0.0_dp, self%x]
    self%y = [0.0_dp, self%y]
    self%z = 0.0_dp
    self%zeta = 0.0_dp
    
   end subroutine generate

   subroutine export(self, n)
     class(Kinoshita) :: self
     integer :: io, stat, i, n
     character(len=512) :: file_name, num, msg

     write(num, '(I0)') n
     file_name = "data/kinoshita_0" // trim(num) // ".csv"
     
     open(newunit=io, file=file_name, status="replace", action="write", iostat=stat, iomsg=msg)
     
    if (stat /= 0) then
       stop trim(msg)
    end if
    
    write(io, 100) 'w = ', self%w
    write(io, 100) 'd = ', self%d
    write(io, 100) 'theta0 = ', self%theta0
    write(io, 100) 'ds = ', self%ds
    write(io, 100) 'L = ', self%wavelength
    write(io, 100) 'js = ', self%js
    write(io, 100) 'jf = ', self%jf
    write(io, '(A, I0)') 'direction = ', self%direction
    write(io, '(A, I0)') 'nbends = ', self%nbends
    100 format(A, F0.16)
    
    write(io, '(A)') 'x,y'
    do i = 1, size(self%x)
       write(io, '(G0.16, A, G0.16)') self%x(i), ',', self%y(i)
    end do
    
    close(io)
     
   end subroutine export
   
end module kinoshita_class
