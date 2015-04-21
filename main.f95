program main
  use constants
  use initialize
  use evolve
  use plotroutines
  use io
  implicit none

  complex(dp), allocatable :: psi(:,:), A_x_conj(:,:,:), A_y_conj(:,:,:),&
    A_x_d(:,:), A_y_d(:,:), A_u(:,:)
  real(dp), allocatable    :: x(:,:), y(:,:), V(:,:)
  real(dp) :: k_x, k_y, dx, dt, L
  integer  :: M, n

  call user_in(k_x,k_y,dx,dt,L,M,n)
  allocate(psi(M,M),x(M,M),y(M,M),V(M,M),A_x_conj(M,M,M),A_y_conj(M,M,M) &
    ,A_x_d(M,M),A_y_d(M,M),A_u(M-1,M))
  
  call init_wavef(psi,x,y,dx,L,k_x,k_y,M)
  call init_V(V,x,y,L)
  call init_ops(A_x_d,A_y_d,A_u,A_x_conj,A_y_conj,V,dt,dx,M)
  call animate_plot(L)

  call run_sim(psi,x,y,V,n,M,A_x_d,A_y_d,A_u,A_x_conj,A_y_conj)
  
  call close_plot()
  deallocate(psi,x,y,V,A_x_conj,A_y_conj,A_x_d,A_y_d,A_u)
end program
