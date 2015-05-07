module initialize
  use constants
  use structures 
  implicit none
  private
  public :: init_param, init_wavef, init_ops

contains
  subroutine init_param(Q)
    type(modl_par), intent(inout) :: Q

    ! set model parameters
    Q%dx = 0.01_dp
    Q%dt = 0.025_dp
    
    if (Q%V_type == 1) then
      Q%L = 12._dp
      Q%M = floor(Q%L/Q%dx)
      Q%plot_interval = 9
      Q%n = 20000
    elseif (Q%V_type == 2) then
      Q%L = 50._dp
      Q%M = floor(Q%L/Q%dx)
      Q%plot_interval = 9
      Q%n = 5000
    endif
    
    Q%tau = 400._dp
  end subroutine

  subroutine init_wavef(psi, x, Q)
    complex(dp), intent(inout) :: psi(:) 
    real(dp), intent(inout)    :: x(:)
    type(modl_par), intent(in) :: Q 

    real(dp), allocatable    :: r(:), Hx(:)
    real(dp)                 :: A
    integer                  :: i
    
    allocate(r(Q%M), Hx(Q%M))
   
    do i = 1,Q%M
      x(i) = i*Q%dx
    enddo

    if (Q%V_type == 1) then
      r = abs(x - Q%L/2)
      Hx = (x - Q%L/2)
      A = 1._dp
    elseif (Q%V_type == 2) then
      r = abs(x - Q%L/4)
      Hx = 1._dp
      A = 1._dp
    endif

    psi = Hx*exp(-0.5_dp*A*r**2)*exp(i_u*Q%k*x)

    ! normalize wavefunction
    psi = psi/sqrt(sum(abs(psi)**2*Q%dx))
  end subroutine

  subroutine init_ops(A, Q)
    complex(dp), intent(inout) :: A(:,:)
    type(modl_par), intent(in) :: Q

    real(dp) :: r

    r = Q%dt/Q%dx**2

    ! initialize matrix operator in band storage fmt
    A(1,:) = -0.5_dp*i_u*r
    A(2,:) = one + i_u*r
    A(3,:) = -0.5_dp*i_u*r
  end subroutine
end module 
