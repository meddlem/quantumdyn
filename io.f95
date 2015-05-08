module io
  use constants
  use structures
  implicit none
  private
  public :: user_in, get_usr_args, output
contains

  subroutine user_in(Q)
    type(modl_par), intent(inout) :: Q
  
    if (any(Q%sim_type == ['tun', 'har'])) then
      write(*,'(/,A,/)') '************ Input *************' 
      write(*,'(A)',advance='no') "k = " 
      read(*,*) Q%k
    else
      Q%k = 0._dp
    endif

    write(*,'(A)') "Running simulation..."
  end subroutine

  subroutine get_usr_args(Q, P)
    type(modl_par), intent(inout) :: Q
    type(plt_par), intent(inout)  :: P

    character(10) :: arg
    integer       :: i

    ! default 
    Q%sim_type = 'har' ! Harmonic potential
    P%plot_re = .false. ! Plot density 

    ! check command line arguments
    do i=1,iargc()
      call getarg(i,arg)
      if (trim(arg) == '--hsq') then
        Q%sim_type = 'hsq' ! adiabatic change Harmonic -> ISQW
      endif
      if (trim(arg) == '--tun') then
        Q%sim_type = 'tun' ! scattering potential
      endif
      if (trim(arg) == '--hqa') then
        Q%sim_type = 'hqa' ! adiabatic change Harmonic -> quartic potential
      endif
      if (trim(arg) == '--exp') then
        Q%sim_type = 'exp' ! adiabatic change Harmonic -> exponential pot
      endif
      if (trim(arg) == '-r') then
        P%plot_re = .true. ! plot real part
      endif
    enddo
  end subroutine

  subroutine output(Q, T)
    type(modl_par), intent(in) :: Q
    real(dp), intent(in)       :: T

    logical       :: exs
    character(40) :: output_fmt

    output_fmt = '(A,F9.4)'
    
    if (Q%sim_type == 'tun') then
      write(*,output_fmt) "T = ", T

      ! write to file 
      inquire(file='ET.dat',exist=exs)
      if (exs) then
        open(12,file ='ET.dat',status='old',position='append',&
          action='write')
      else 
        open(12,file ='ET.dat',status='new',action='write')
      endif
        write(12,'(F9.5,1X,F9.5)') Q%k**2, T
      close(12)  
    endif
  end subroutine
end module
