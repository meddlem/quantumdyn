module structures
  use constants
  implicit none

  ! this module contains additional datatypes used in simulation
  ! default values are set for all parameters, later changes can be made in
  ! init_param subroutine
  
  type modl_par 
    ! data type that contains all model parameters 

    ! initial wavevector 
    real(dp) :: k = 0._dp
    ! lattice length
    real(dp) :: L = 12._dp
    ! spatial and temporal discretization steps 
    real(dp) :: dx = 0.01_dp
    real(dp) :: dt = 0.02_dp
    ! number of time steps/iterations
    integer  :: N = 30000
    ! time scale adiabatic potential variation
    real(dp) :: tau = 400._dp 
    ! number of lattice points 
    integer  :: M 
    ! defines potential, initial wavefunction
    character(3) :: sim_type = 'har'
  end type
  
  type plt_par
    ! data type containing all plot parameters

    ! number of iterations between plots
    integer  :: plot_interval = 19
    ! min and max y values for plot
    real(dp) :: rng(2) = [-1._dp, 1._dp]
    ! plot real part or density
    logical  :: plot_re = .false.
  end type
end module
