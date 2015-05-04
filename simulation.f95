module simulation
  use constants
  use plotroutines
  implicit none
  private
  public :: run_sim

contains
  subroutine run_sim(psi, x, V, n, L, dt, M, Ax)
    complex(dp), intent(inout) :: psi(:)
    complex(dp), intent(in)    :: Ax(:,:)
    real(dp), intent(in)       :: x(:), dt, L
    real(dp), intent(inout)    :: V(:)
    integer, intent(in)        :: n, M

    integer  :: i

    do i=1,n
      call solve_nxt(psi, x, i*dt, dt, V, L, M, Ax)

      if(mod(i,10)==0) call plot_wavef(psi, x, V, M)
    enddo
  end subroutine

  subroutine solve_nxt(psi, x, t, dt, V, L, M, Ax)
    complex(dp), intent(inout) :: psi(:)
    real(dp), intent(inout)    :: V(:)
    complex(dp), intent(in)    :: Ax(:,:)
    real(dp), intent(in)       :: x(:), t, dt, L
    integer, intent(in)        :: M

    complex(dp), allocatable :: g(:), Ax_d_tmp(:), Ax_l_tmp(:), Ax_u_tmp(:)
    integer                  :: info

    allocate(Ax_d_tmp(M), Ax_l_tmp(M-1), Ax_u_tmp(M-1), g(M))
    
    ! init temp arrays
    call potential(V, x, t, L)

    Ax_l_tmp = Ax(1,1:M-1)
    Ax_d_tmp = Ax(2,:) + cmplx(0._dp, 0.5_dp*dt*V, dp)
    Ax_u_tmp = Ax(1,1:M-1)
    
    ! explicit part of calculation, mat-vec multiplication
    call zgbmv('N', M, M, 1, 1, one, conjg(Ax), 3, psi, 1, zero, g, 1)

    ! solve for wavefunction at t=n+1
    call zgtsv(M, 1, Ax_l_tmp, Ax_d_tmp, Ax_u_tmp, g, M, info)

    ! collect wavefunction at t=n+1
    psi = g

    deallocate(Ax_d_tmp, Ax_l_tmp, Ax_u_tmp, g)
  end subroutine

  pure subroutine potential(V, x, t, L)
    real(dp), intent(inout) :: V(:)
    real(dp), intent(in)    :: x(:), t, L
    
    ! block/scattering potential
    !V = 0._dp
    !where(28._dp<x .and. x<32._dp) V = 1._dp
    
    ! harmonic potential
    V = 1._dp*(x-L/2)**2 !+ 0._dp*t
  end subroutine
end module
