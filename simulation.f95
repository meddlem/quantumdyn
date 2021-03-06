module simulation
  use constants
  use structures
  use plotroutines
  implicit none
  private
  public :: time_evo

contains
  subroutine time_evo(psi, x, A, Q, P, T)
    complex(dp), intent(inout) :: psi(:)
    complex(dp), intent(in)    :: A(:,:)
    real(dp), intent(in)       :: x(:) 
    type(modl_par), intent(in) :: Q
    type(plt_par), intent(in)  :: P
    real(dp), intent(out)      :: T

    real(dp), allocatable :: V(:), V1(:), V2(:)
    real(dp)              :: vx
    integer               :: i

    allocate(V(Q%M), V1(Q%M), V2(Q%M))
    ! init potential
    call potential(V, x, 0._dp, Q)
    ! average velocity of the wavepacket
    vx = 2*Q%k
    ! initial plot
    call snapshot(psi, x, V, Q, P, 1)
    call animate_plot(Q, P)
    
    do i = 1,Q%N
      ! calculate potential using trapezoidal rule
      if (any(Q%sim_type == ['hqa', 'hsq', 'exp'])) then
        call potential(V1, x, i*Q%dt, Q)
        call potential(V2, x, (i+1)*Q%dt, Q)
        V = 0.5_dp*(V1 + V2)
      endif

      ! time integration 
      call solve_nxt(psi, V, A, Q)

      if (Q%sim_type == 'tun') then
        if (abs(vx*i*Q%dt) > Q%L/2) then
          ! calc transmission coeff
          T = sum(abs(psi(Q%M/2:Q%M))**2*Q%dx)
          ! stop iteration after wavepacket goes through
          exit
        endif
      endif

      if (mod(i,P%plot_interval) == 0) then
        call plot_wavef(psi, x, V, Q, .true.)
      endif
    enddo

    ! final plot
    call close_plot()
    call snapshot(psi, x, V, Q, P, 2)
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
        V = (1._dp - t/Q%tau)**2*(x - Q%L/2)**2 + (t/Q%tau)**2*(x - Q%L/2)**4
      else
        V = (x - Q%L/2)**4
      endif
    
    elseif (Q%sim_type == 'tun') then
      ! fixed height barrier
      V = 0._dp
      where(abs(x-Q%L/2) < Q%W) V = Q%V0

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
