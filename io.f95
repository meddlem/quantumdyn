module io
  use constants
  use structures
  implicit none
  private
  public :: user_in, get_usr_args
contains

  subroutine user_in(Q)
    type(modl_par), intent(inout) :: Q
  
    if (Q%V_type == 2) then
      write(*,'(/,A,/)') '************ Input *************' 
      write(*,'(A)',advance='no') "k = " 
      read(*,*) Q%k
    endif

    write(*,'(A)') "Running simulation..."
  end subroutine

  subroutine get_usr_args(Q, P)
    type(modl_par), intent(inout) :: Q
    type(plt_par), intent(inout)  :: P

    character(10) :: arg
    integer       :: i

    ! default 
    Q%V_type = 3 ! Harmonic potential
    P%plot_re = .false. ! Plot density 

    ! check command line arguments
    do i=1,iargc()
      call getarg(i,arg)
      if (trim(arg) == '-a') then
        Q%V_type = 1 ! scattering potential
      endif
      if (trim(arg) == '-t') then
        Q%V_type = 2 ! scattering potential
      endif
      if ((trim(arg) == '--PlotRe') .or. (trim(arg) == '-r')) then
        P%plot_re = .true. ! plot real part
      endif
    enddo
  end subroutine
end module
