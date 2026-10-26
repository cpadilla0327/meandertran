module kinoshita_class
   use iso_fortran_env, only: dp => real64
   use rng, only: rand_real, rand_int, rand_choice
   implicit none
   private
   public :: Kinoshita
   real(dp), parameter :: pi = acos(-1.0_dp)
  
  type :: Kinoshita
      real(dp), allocatable :: x(:), y(:), z(:), zeta(:)
      real(dp) :: width, depth, slope, theta0, ds, wavelength, js, jf
      integer :: direction, nbends

   contains
      procedure :: initialize
      procedure :: generate
      procedure :: export_csv

   end type Kinoshita

contains

  subroutine initialize(self, inputs_int, inputs_real)
      class(Kinoshita), intent(inout) :: self
      integer, intent(in) :: inputs_int(5)
      real(dp), intent(in) :: inputs_real(12)

      self%width = rand_real(inputs_real(1), inputs_real(2))
      self%depth = rand_real(inputs_real(3), inputs_real(4))
      self%slope = rand_real(inputs_real(5), inputs_real(6))
      self%theta0 = rand_real(inputs_real(7), inputs_real(8))
      self%wavelength = 10.9_dp * self%width**(1.01_dp)  ! From Stream Stabilization Handbook (Biedenharn et al., 1997)
      self%ds = self%wavelength / 100
      self%nbends = rand_int(inputs_int(4), inputs_int(5))
      self%js = rand_real(inputs_real(9), inputs_real(10))
      self%jf = rand_real(inputs_real(11), inputs_real(12))
      self%direction = rand_choice(inputs_int(2), inputs_int(3))

   end subroutine initialize

   subroutine generate(self)
      class(Kinoshita), intent(inout) :: self
      real(dp), allocatable :: s(:), sine_term(:), skewness_term(:), flatness_term(:), theta(:)
      integer :: i
      real(dp) :: theta0_rad

      if (allocated(self%x)) deallocate(self%x)
      if (allocated(self%y)) deallocate(self%y)
      if (allocated(self%z)) deallocate(self%z)
      if (allocated(self%zeta)) deallocate(self%zeta)

      allocate(s(int(self%nbends * self%wavelength / self%ds) + 1))

      do i = 1, size(s)
         s(i) = (self%nbends * self%wavelength) * real(i - 1, dp) / real(size(s) - 1, dp)
      end do

      theta0_rad = self%theta0 * (pi / 180.0_dp)
      sine_term = theta0_rad * sin(2 * pi * s / self%wavelength)
      skewness_term = self%js * cos(6 * pi * s / self%wavelength)
      flatness_term = self%jf * sin(6 * pi * s / self%wavelength)
      theta = self%direction * sine_term + theta0_rad**3 * (skewness_term - flatness_term)

      self%x = cos(theta) * self%ds
      self%y = sin(theta) * self%ds

      self%x(1) = 0.0_dp
      self%y(1) = 0.0_dp    

      do i = 2, size(s)
         self%x(i) = self%x(i) + self%x(i-1)
         self%y(i) = self%y(i) + self%y(i-1)
      end do

      self%z = self%slope * self%x(size(self%x):1:-1)

      allocate(self%zeta(size(s)))
      self%zeta = 0.0_dp
    
   end subroutine generate

   subroutine export_csv(self, n)
      class(Kinoshita), intent(in) :: self
      integer, intent(in) :: n
      integer :: io, stat, i
      character(len=512) :: file_name, num, msg

      write(num, "(i0)") n
      file_name = "data/kinoshita_0" // trim(num) // ".csv"
      
      open(newunit=io, file=file_name, status="replace", action="write", iostat=stat, iomsg=msg)
      
      if (stat /= 0) then
      stop trim(msg)
      end if
    
      write(io, 100) "width = ", self%width
      write(io, 100) "depth = ", self%depth
      write(io, 100) "slope = ", self%slope
      write(io, 100) "theta0 = ", self%theta0
      write(io, 100) "ds = ", self%ds
      write(io, 100) "wavelength = ", self%wavelength
      write(io, 100) "js = ", self%js
      write(io, 100) "jf = ", self%jf
      write(io, '(a, i0)') "direction = ", self%direction
      write(io, '(a, i0)') "nbends = ", self%nbends
      100 format(A, F0.16)
      
      write(io, "(a)") "x,y,z"
      do i = 1, size(self%x)
         write(io, 101) self%x(i), ',', self%y(i), ',', self%z(i)
         101 format(g0.16, a, g0.16, a, g0.16)
      end do
      
      close(io)
     
   end subroutine export_csv
   
end module kinoshita_class