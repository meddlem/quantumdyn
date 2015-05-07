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

      if (mod(i,P%plot_interval) == 0) then
        call plot_wavef(psi, x, V, Q)
      endif
    enddo

    call close_plot()
    deallocate(V, V1, V2)
  end subroutine

  subroutine solve_nxt(psi, V, A, Q)
    complex(dp), intent(inout) :: psi(:)
    real(dp), intent(inout)    :: V(:)
    complex(dp), intent(in)    :: A(:,:)
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

    if (Q%V_type == 1) then
      ! adiabatic change harmonic potential -> ISQW
      if (t < Q%tau) then
        V = (1._dp - t/Q%tau)**2*(x-Q%L/2)**2 
      else
        V = 0._dp
      endif
    elseif (Q%V_type == 2) then
      ! fixed height barrier
      V = 0._dp
      where(abs(x-Q%L/2) < Q%L/40) V = 2._dp
    elseif (Q%V_type == 3) then
      ! harmonic potential 
      V = 0._dp
      V = (x-Q%L/2)**2 
    endif
  end subroutine
end module
