module simulation
  use constants
  use structures
  use plotroutines
  implicit none
  private
  public :: time_evo

contains
  subroutine time_evo(psi, x, A, Q, P)
    complex(dp), intent(inout) :: psi(:)
    complex(dp), intent(in)    :: A(:,:)
    real(dp), intent(in)       :: x(:) 
    type(modl_par), intent(in) :: Q
    type(plt_par), intent(in)  :: P

    real(dp), allocatable :: V(:), V1(:), V2(:)
    real(dp) :: T
    logical  :: exs
    integer  :: i

    allocate(V(Q%M), V1(Q%M), V2(Q%M))
    call animate_plot(Q, P)
    
    do i = 1,Q%N
      ! calculate potential using trapezoidal rule
      call potential(V1, x, i*Q%dt, Q)
      call potential(V2, x, (i+1)*Q%dt, Q)
      V = 0.5_dp*(V1 + V2)

      ! time integration 
      call solve_nxt(psi, V, A, Q)

      if (Q%sim_type == 'tun') then
        if (i*Q%dt > (Q%L/4 + 2.35482_dp)/Q%k) then
          
          ! calc transmission coeff
          where (x < Q%L/2) psi = zero
          T = sum(abs(psi)**2*Q%dx)

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

          ! stop iteration after wavepacket goes through
          exit
        endif
      endif

      if (mod(i,P%plot_interval) == 0) then
        call plot_wavef(psi, x, V, Q)
      endif
    enddo

    call close_plot()
    deallocate(V, V1, V2)
  end subroutine

  subroutine solve_nxt(psi, V, A, Q)
    complex(dp), intent(inout) :: psi(:)
    complex(dp), intent(in)    :: A(:,:)
    real(dp), intent(in)       :: V(:)
    type(modl_par), intent(in) :: Q

    complex(dp), allocatable :: g(:), A_tmp(:,:)
    integer                  :: info

    allocate(A_tmp(3,Q%M), g(Q%M))
    
    ! init temp arrays
    A_tmp = A
    A_tmp(2,:) = A_tmp(2,:) + 0.5_dp*i_u*Q%dt*V

    ! explicit part of calculation, mat-vec multiplication, using BLAS routine
    call zgbmv('N', Q%M, Q%M, 1, 1, one, conjg(A_tmp), 3, psi, 1, zero, g, 1)

    ! solve for wavefunction at t=n+1, using LAPACK routine
    call zgtsv(Q%M, 1, A_tmp(1,1:Q%M-1), A_tmp(2,:), A_tmp(3,1:Q%M-1), g, &
      Q%M, info)

    ! collect wavefunction at t=n+1
    psi = g

    deallocate(A_tmp, g)
  end subroutine

  pure subroutine potential(V, x, t, Q)
    real(dp), intent(inout)    :: V(:)
    real(dp), intent(in)       :: x(:), t
    type(modl_par), intent(in) :: Q

    if (Q%sim_type == 'hsq') then
      ! adiabatic change harmonic potential -> ISQW
      if (t < Q%tau) then
        V = (1._dp - t/Q%tau)**2*(x-Q%L/2)**2 
      else
        V = 0._dp
      endif
    
    elseif (Q%sim_type == 'hqa') then
      ! adiabatic change harmonic potential -> quartic potential
      if (t < Q%tau) then
        V = (1._dp - t/Q%tau)*(x - Q%L/2)**2 + t/Q%tau*(x - Q%L/2)**4
      else
        V = (x - Q%L/2)**4
      endif
    
    elseif (Q%sim_type == 'tun') then
      ! fixed height barrier
      V = 0._dp
      where(abs(x-Q%L/2) < Q%L/100) V = 4._dp

    elseif (Q%sim_type == 'har') then
      ! harmonic potential 
      V = 0._dp
      V = (x-Q%L/2)**2 

    elseif (Q%sim_type == 'exp') then
      ! adiabatic: harmonic -> exponential potential
      if (t < Q%tau) then
        V = (1._dp - t/Q%tau)**2*(x - Q%L/2)**2 + &
          (t/Q%tau)**2*(1._dp - exp(-abs(x - Q%L/2)))
      else
        V = 1._dp - exp(-abs(x - Q%L/2)) 
      endif
    endif
  end subroutine
end module
