program main
  use iso_fortran_env, only: dp => real64
  use kinoshita_class
  implicit none
  integer :: io, stat, n_samples, i
  character(128) :: msg
  type(Kinoshita) :: ch

  ! --- Read input.txt ---
  open(newunit=io, file="data/input", status="old", action="read", iostat = stat, iomsg=msg)
  if (stat /= 0) then 
    print *, trim(msg) 
    stop
  end if
  read(io,*) n_samples

  write(*,"(/, a, i0)") "Number of samples to generate: ", n_samples

  call execute_command_line("mkdir -p data")    
  call execute_command_line("rm data/*.csv")
  
  do i = 1, n_samples

    ! --- Deallocate existing arrays
    if (allocated(ch%x)) deallocate(ch%x)
    if (allocated(ch%y)) deallocate(ch%y)
    if (allocated(ch%z)) deallocate(ch%z)
    if (allocated(ch%zeta)) deallocate(ch%zeta)

    ! --- Create Kinoshita meander ---
    call ch%initialize()
    call ch%generate()

    ! --- Export to .csv
    call ch%export(i)
    
    write (*, 100, advance="no") achar(13), "Generating samples... ", i, '/', n_samples
    100 format (a, a, i0, a, i0)

  end do
  
  write(*, "(/, a)") "Samples generated successfully. Saved in data/"

end program main