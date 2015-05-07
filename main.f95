program main
  use constants
  use structures
  use initialize
  use simulation
  use io
  implicit none

  type(modl_par) :: Q
  type(plt_par)  :: P

  call get_usr_args(Q, P)
  call user_in(Q)
  call init_param(Q, P)
  call run_sim(Q, P)

  contains
    subroutine run_sim(Q, P)
      type(modl_par), intent(in) :: Q
      type(plt_par), intent(in)  :: P

      complex(dp), allocatable :: psi(:), A(:,:)
      real(dp), allocatable    :: x(:) 
      
      allocate(psi(Q%M), x(Q%M), A(3,Q%M))
      call init_wavef(psi, x, Q)
      call init_ops(A, Q)

      ! time integration
      call time_evo(psi, x, A, Q, P)

      deallocate(psi, x, A)
    end subroutine
end program
