module initialize
  use constants
  use structures 
  implicit none
  private
  public :: init_param, init_wavef, init_ops

contains
  subroutine init_param(Q, P)
    type(modl_par), intent(inout) :: Q
    type(plt_par), intent(inout)  :: P

    ! override model parameters

    if (Q%sim_type == 'tun') then
      Q%L = 100._dp
    elseif (Q%sim_type == 'har') then
      P%plot_interval = 1
    endif
    
    Q%M = floor(Q%L/Q%dx)
  end subroutine

  subroutine init_wavef(psi, x, Q)
    complex(dp), intent(inout) :: psi(:) 
    real(dp), intent(inout)    :: x(:)
    type(modl_par), intent(in) :: Q 

    real(dp), allocatable    :: r(:), Hx(:)
    real(dp)                 :: sigma
    integer                  :: i
    
    allocate(r(Q%M), Hx(Q%M))
   
    do i = 1,Q%M
      x(i) = i*Q%dx
    enddo

    sigma = 1._dp
    r = abs(x - Q%L/2)
    Hx = 1._dp
    
    if (Q%sim_type == 'hsq') then
      ! 1st excited state of harmonic potential
      Hx = (x - Q%L/2)
    elseif (Q%sim_type == 'tun') then
      ! gaussian wavepacket
      r = abs(x - Q%L/4)
      Hx = 1._dp
      sigma = Q%L/20._dp
    elseif (any(Q%sim_type == ['har', 'hqa'])) then
      ! 2nd excited state of harmonic potential
      Hx = 4*(x - Q%L/2)**2 - 2
    endif

    psi = Hx*exp(-0.5_dp*r**2/sigma**2)*exp(i_u*Q%k*x)

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
