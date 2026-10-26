program main
  use iso_fortran_env, only: dp => real64
  use kinoshita_class
  implicit none
  integer, allocatable :: params_int(:)
  real(dp), allocatable :: params_real(:)
  type(Kinoshita) :: ch
  integer :: i
  
  ! --- Read input file for parameters in data/input ---
  call read_inputs(params_int, params_real)
  write(*,"(/, a, i0)") "Number of samples to generate: ", params_int(1)

  ! --- Linux specific CLI commands ---
  call execute_command_line("mkdir -p data")    
  call execute_command_line("rm data/*.csv")
  
  ! --- Core of main program ---
  do i = 1, params_int(1)

    ! --- Create Kinoshita meander ---
    call ch%initialize(params_int, params_real)
    call ch%generate()

    ! --- Export to current channel to .csv ---
    call ch%export_csv(i)
    
    write (*, 100, advance="no") achar(13), "Generating samples... ", i, '/', params_int(1)
    100 format (a, a, i0, a, i0)

  end do
  
  write(*, "(/, a)") "Samples generated successfully. Saved in data/"

  contains

  subroutine read_inputs(inputs_int, inputs_real)
    integer, allocatable, intent(inout) :: inputs_int(:)
    real(dp), allocatable, intent(inout) :: inputs_real(:)
    integer :: io, stat, i
    character(128) :: msg

    allocate(inputs_int(5), inputs_real(12))

    ! --- Read input.txt ---
    open(newunit=io, file="data/input", status="old", action="read", iostat = stat, iomsg=msg)

    if (stat /= 0) then 
    print *, trim(msg) 
    stop
    end if

    ! --- Read integer inputs (number of samples, bends and direction) ---
    do i = 1, size(inputs_int)
    if (i == 1 ) then 
    read(io,*) inputs_int(i)
    else if (mod(i,2) == 0) then
    read(io,*) inputs_int(i), inputs_int(i+1)
    else if (i == size(inputs_int)) then
    exit
    end if  
    end do

    ! --- Read real inputs (width, depth, slope, theta0, ds, wavelength, js, jf) ---
    do i = 1, size(inputs_real), 2
    read(io,*) inputs_real(i), inputs_real(i+1)
    end do

    close(io)

    end subroutine read_inputs

end program main